# Shared utilities and scoring definitions for questionnaire analyses.

options(stringsAsFactors = FALSE)

get_script_directory <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command, value = TRUE)
  if (length(file_argument) == 1L) {
    script_path <- sub("^--file=", "", file_argument)
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

analysis_directory <- get_script_directory()
project_directory <- dirname(analysis_directory)
preprocessing_directory <- file.path(project_directory, "preprocessing")
analysis_data_file <- file.path(preprocessing_directory, "analysis_with_scale_scores.csv")
score_codebook_file <- file.path(preprocessing_directory, "scale_score_codebook.csv")
results_directory <- file.path(analysis_directory, "results")

assert_condition <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

ensure_directory <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  assert_condition(dir.exists(path), sprintf("Could not create directory: %s", path))
  invisible(path)
}

require_packages <- function(packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  assert_condition(
    length(missing_packages) == 0L,
    sprintf(
      "Missing R packages: %s. Run 00_install_required_packages.R first.",
      paste(missing_packages, collapse = ", ")
    )
  )
  invisible(TRUE)
}

read_utf8_csv <- function(file_path, all_character = FALSE) {
  assert_condition(file.exists(file_path), sprintf("File not found: %s", file_path))
  utils::read.csv(
    file_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = if (all_character) "character" else NA,
    na.strings = character(0),
    quote = '"',
    comment.char = "",
    encoding = "UTF-8"
  )
}

quote_csv_field <- function(value) {
  if (length(value) == 0L || is.na(value)) value <- ""
  text <- enc2utf8(as.character(value))
  text <- gsub('"', '""', text, fixed = TRUE, useBytes = TRUE)
  paste0('"', text, '"')
}

write_utf8_csv <- function(data, file_path) {
  ensure_directory(dirname(file_path))
  lines <- character(nrow(data) + 1L)
  lines[[1]] <- paste(vapply(names(data), quote_csv_field, character(1)), collapse = ",")
  if (nrow(data) > 0L) {
    for (row_index in seq_len(nrow(data))) {
      lines[[row_index + 1L]] <- paste(
        vapply(
          data[row_index, , drop = FALSE],
          function(column) quote_csv_field(column[[1]]),
          character(1)
        ),
        collapse = ","
      )
    }
  }
  temporary_file <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(file_path)), "_"),
    tmpdir = tempdir(),
    fileext = ".csv"
  )
  on.exit(unlink(temporary_file, force = TRUE), add = TRUE)
  connection <- base::file(temporary_file, open = "wb")
  writeLines(lines, connection, sep = "\r\n", useBytes = TRUE)
  close(connection)
  copied <- file.copy(temporary_file, file_path, overwrite = TRUE)
  assert_condition(copied, sprintf("Could not write CSV: %s", file_path))
  invisible(file_path)
}

write_utf8_text <- function(lines, file_path) {
  ensure_directory(dirname(file_path))
  connection <- base::file(file_path, open = "wb")
  writeLines(enc2utf8(lines), connection, sep = "\r\n", useBytes = TRUE)
  close(connection)
  invisible(file_path)
}

numeric_column <- function(values, variable_name) {
  text <- trimws(as.character(values))
  blank <- is.na(values) | is.na(text) | !nzchar(text)
  result <- suppressWarnings(as.numeric(text))
  invalid <- !blank & is.na(result)
  assert_condition(!any(invalid), sprintf("%s contains non-numeric values", variable_name))
  result[blank] <- NA_real_
  result
}

make_item_names <- function(scale_id, item_numbers, suffix = "") {
  paste0(sprintf("%s_%02d", scale_id, item_numbers), suffix)
}

load_analysis_data <- function() {
  data <- read_utf8_csv(analysis_data_file, all_character = FALSE)
  required <- c(
    "participant_id", "source_file", "age", "gender_code", "education_code",
    "marital_status_code", "income_code", "attention_all_pass"
  )
  missing <- setdiff(required, names(data))
  assert_condition(length(missing) == 0L, sprintf("Analysis data missing: %s", paste(missing, collapse = ", ")))
  assert_condition(nrow(data) > 0L, "Analysis data contains no rows")
  assert_condition(!anyDuplicated(data$participant_id), "participant_id contains duplicates")
  data
}

score_variables <- function() {
  codebook <- read_utf8_csv(score_codebook_file, all_character = TRUE)
  assert_condition("score_variable" %in% names(codebook), "Score codebook has no score_variable column")
  unique(codebook$score_variable)
}

ecs_versions <- c(
  I = "general", J = "happiness", K = "satisfaction", L = "enthusiasm",
  M = "pride", N = "fascination", O = "being_loved", P = "feeling_energetic"
)

ecs_item_sets <- function() {
  result <- list()
  for (scale_id in names(ecs_versions)) {
    version <- unname(ecs_versions[[scale_id]])
    result[[paste0("ECS_", version, "_awareness")]] <- make_item_names(scale_id, c(2, 5, 8))
    result[[paste0("ECS_", version, "_action")]] <- make_item_names(scale_id, c(1, 3, 4, 6, 7, 9, 10))
  }
  result
}

reliability_item_sets <- function() {
  base_sets <- list(
    CERQ_putting_into_perspective = c("A_01", "A_02"),
    CERQ_catastrophizing = c("A_03", "A_04"),
    CERQ_other_blame = c("A_05", "A_06"),
    CERQ_self_blame = c("A_07", "A_08"),
    CERQ_acceptance = c("A_09", "A_10"),
    CERQ_rumination = c("A_11", "A_12"),
    CERQ_positive_refocusing = c("A_14", "A_15"),
    CERQ_refocus_on_planning = c("A_16", "A_17"),
    CERQ_positive_reappraisal = c("A_18", "A_19"),
    CERQ_adaptive = c("A_01", "A_02", "A_09", "A_10", "A_14", "A_15", "A_16", "A_17", "A_18", "A_19"),
    CERQ_maladaptive = c("A_03", "A_04", "A_05", "A_06", "A_07", "A_08", "A_11", "A_12"),
    ERQ_reappraisal = make_item_names("B", 1:6),
    EAC_emotional_processing = make_item_names("C", 1:8),
    PANAS_positive_affect = make_item_names("D", 1:10),
    SWLS_life_satisfaction = make_item_names("E", 1:3),
    SVS_vitality = make_item_names("F", 1:3),
    PWBS_personal_growth = c("G_02", "G_03", "G_04_rev"),
    PWBS_purpose_in_life = c("G_01_rev", "G_05", "G_06_rev"),
    PWBS_eudaimonic_wellbeing = c("G_01_rev", "G_02", "G_03", "G_04_rev", "G_05", "G_06_rev"),
    ECRGO_attachment_anxiety = c("H_02", "H_04", "H_05", "H_06", "H_08", "H_09", "H_10", "H_12", "H_15", "H_17", "H_19_rev", "H_21", "H_24", "H_26", "H_28", "H_30", "H_31", "H_32"),
    ECRGO_avoidance_of_intimacy = c("H_01", "H_03_rev", "H_07", "H_11_rev", "H_14", "H_16_rev", "H_18", "H_20", "H_22_rev", "H_25_rev", "H_27_rev", "H_29_rev"),
    K6_psychological_distress = make_item_names("Q", 1:6)
  )
  c(base_sets, ecs_item_sets())
}

key_correlation_variables <- c(
  "ECS_general_awareness_mean", "ECS_general_action_mean",
  "ECS_happiness_awareness_mean", "ECS_happiness_action_mean",
  "ECS_satisfaction_awareness_mean", "ECS_satisfaction_action_mean",
  "ECS_enthusiasm_awareness_mean", "ECS_enthusiasm_action_mean",
  "ECS_pride_awareness_mean", "ECS_pride_action_mean",
  "ECS_fascination_awareness_mean", "ECS_fascination_action_mean",
  "ECS_being_loved_awareness_mean", "ECS_being_loved_action_mean",
  "ECS_feeling_energetic_awareness_mean", "ECS_feeling_energetic_action_mean",
  "CERQ_adaptive_mean", "CERQ_maladaptive_mean", "ERQ_reappraisal_mean",
  "EAC_emotional_processing_mean", "PANAS_positive_affect_mean",
  "SWLS_life_satisfaction_mean", "SVS_vitality_mean",
  "PWBS_eudaimonic_wellbeing_mean", "K6_psychological_distress_total",
  "ECRGO_attachment_anxiety_mean", "ECRGO_avoidance_of_intimacy_mean"
)

prepare_background_variables <- function(data) {
  result <- data.frame(
    participant_id = data$participant_id,
    age_c = numeric_column(data$age, "age") - mean(numeric_column(data$age, "age"), na.rm = TRUE),
    gender = factor(
      ifelse(numeric_column(data$gender_code, "gender_code") %in% c(1, 2), numeric_column(data$gender_code, "gender_code"), NA),
      levels = c(1, 2), labels = c("male", "female")
    ),
    education_c = numeric_column(data$education_code, "education_code") - mean(numeric_column(data$education_code, "education_code"), na.rm = TRUE),
    income_c = numeric_column(data$income_code, "income_code") - mean(numeric_column(data$income_code, "income_code"), na.rm = TRUE),
    marital = factor(
      numeric_column(data$marital_status_code, "marital_status_code"),
      levels = 1:5,
      labels = c("married_cohabiting", "single", "partner", "separated_divorced", "widowed")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  result$gender_female <- ifelse(is.na(result$gender), NA_real_, as.numeric(result$gender == "female"))
  for (level_name in levels(result$marital)[-1]) {
    variable_name <- paste0("marital_", level_name)
    result[[variable_name]] <- ifelse(is.na(result$marital), NA_real_, as.numeric(result$marital == level_name))
  }
  result
}

active_covariates <- function(background_data) {
  candidates <- c(
    "age_c", "gender_female", "education_c", "income_c",
    grep("^marital_", names(background_data), value = TRUE)
  )
  candidates[vapply(background_data[candidates], function(x) {
    values <- x[!is.na(x)]
    length(unique(values)) > 1L
  }, logical(1))]
}

round_numeric_columns <- function(data, digits = 3L) {
  numeric_columns <- vapply(data, is.numeric, logical(1))
  data[numeric_columns] <- lapply(data[numeric_columns], round, digits = digits)
  data
}

lavaan_fit_row <- function(fit, model_name, scale_name = NA_character_) {
  require_packages("lavaan")
  requested <- c(
    "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust",
    "tli.robust", "rmsea.robust", "rmsea.ci.lower.robust",
    "rmsea.ci.upper.robust", "srmr", "aic", "bic"
  )
  measures <- tryCatch(lavaan::fitMeasures(fit, requested), error = function(error) numeric(0))
  get_measure <- function(name, fallback = NA_real_) {
    if (name %in% names(measures)) unname(measures[[name]]) else fallback
  }
  chi_square <- get_measure("chisq.scaled")
  degrees_freedom <- get_measure("df.scaled", tryCatch(lavaan::fitMeasures(fit, "df"), error = function(error) NA_real_))
  data.frame(
    scale = scale_name,
    model = model_name,
    n = lavaan::lavInspect(fit, "nobs"),
    converged = lavaan::lavInspect(fit, "converged"),
    chisq = chi_square,
    df = degrees_freedom,
    chisq_df = chi_square / degrees_freedom,
    p_value = get_measure("pvalue.scaled"),
    cfi = get_measure("cfi.robust"),
    tli = get_measure("tli.robust"),
    rmsea = get_measure("rmsea.robust"),
    rmsea_ci_lower = get_measure("rmsea.ci.lower.robust"),
    rmsea_ci_upper = get_measure("rmsea.ci.upper.robust"),
    srmr = get_measure("srmr"),
    aic = get_measure("aic"),
    bic = get_measure("bic"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

write_analysis_session_info <- function(file_path, extra_lines = character(0)) {
  lines <- c(
    sprintf("Run time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Input: %s", normalizePath(analysis_data_file, winslash = "/", mustWork = TRUE)),
    extra_lines,
    "",
    capture.output(sessionInfo())
  )
  write_utf8_text(lines, file_path)
}

