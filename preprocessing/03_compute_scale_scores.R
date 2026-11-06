# Scale and subscale scoring for the prepared questionnaire data
#
# Default input:          prepared_analysis_data.csv
# Default full output:    analysis_with_scale_scores.csv
# Default compact output: scale_scores.csv
# Default score codebook: scale_score_codebook.csv
#
# Usage:
#   Rscript 03_compute_scale_scores.R
#   Rscript 03_compute_scale_scores.R input.csv full.csv compact.csv codebook.csv
#
# Scoring policy:
# - Cleaning, attention-check exclusion, demographic coding, and reverse scoring
#   are performed by 02_prepare_analysis_data.R.
# - Scale scores are calculated here without overwriting item-level data.
# - A score is NA unless all of its required items are present. No imputation is
#   performed. Change minimum_answered_proportion only after a documented
#   analysis decision.
# - Most scores are item means to preserve the response scale. K6 is the
#   conventional 0-24 sum.

options(stringsAsFactors = FALSE)

minimum_answered_proportion <- 1.0

get_script_directory <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command, value = TRUE)

  if (length(file_argument) == 1L) {
    script_path <- sub("^--file=", "", file_argument)
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_directory <- get_script_directory()
arguments <- commandArgs(trailingOnly = TRUE)

input_file <- if (length(arguments) >= 1L) {
  normalizePath(arguments[[1]], winslash = "/", mustWork = TRUE)
} else {
  file.path(script_directory, "prepared_analysis_data.csv")
}

full_output_file <- if (length(arguments) >= 2L) {
  normalizePath(arguments[[2]], winslash = "/", mustWork = FALSE)
} else {
  file.path(script_directory, "analysis_with_scale_scores.csv")
}

compact_output_file <- if (length(arguments) >= 3L) {
  normalizePath(arguments[[3]], winslash = "/", mustWork = FALSE)
} else {
  file.path(script_directory, "scale_scores.csv")
}

codebook_output_file <- if (length(arguments) >= 4L) {
  normalizePath(arguments[[4]], winslash = "/", mustWork = FALSE)
} else {
  file.path(script_directory, "scale_score_codebook.csv")
}

assert_condition <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_condition(
  is.numeric(minimum_answered_proportion) &&
    length(minimum_answered_proportion) == 1L &&
    !is.na(minimum_answered_proportion) &&
    minimum_answered_proportion > 0 &&
    minimum_answered_proportion <= 1,
  "minimum_answered_proportion must be greater than 0 and no greater than 1"
)
assert_condition(file.exists(input_file), sprintf("Input file not found: %s", input_file))

output_paths <- c(full_output_file, compact_output_file, codebook_output_file)
assert_condition(
  length(unique(tolower(output_paths))) == length(output_paths),
  "Full data, compact scores, and codebook must use different paths"
)
assert_condition(
  !tolower(input_file) %in% tolower(output_paths),
  "An output path must not overwrite the prepared input"
)

read_utf8_csv <- function(file_path, all_character = TRUE) {
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
  lines <- character(nrow(data) + 1L)
  lines[[1]] <- paste(
    vapply(names(data), quote_csv_field, character(1)),
    collapse = ","
  )

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

  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
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
  assert_condition(copied, sprintf("Could not write output file: %s", file_path))
}

parse_numeric_column <- function(values, column_name) {
  text <- trimws(as.character(values))
  blank <- !nzchar(text)
  numeric_values <- suppressWarnings(as.numeric(text))
  invalid <- !blank & is.na(numeric_values)
  assert_condition(
    !any(invalid),
    sprintf("%s contains non-numeric values", column_name)
  )
  numeric_values[blank] <- NA_real_
  numeric_values
}

parse_logical_column <- function(values, column_name) {
  text <- tolower(trimws(as.character(values)))
  result <- rep(NA, length(text))
  result[text %in% c("true", "t", "1")] <- TRUE
  result[text %in% c("false", "f", "0")] <- FALSE
  assert_condition(
    !anyNA(result),
    sprintf("%s contains values that are not TRUE/FALSE", column_name)
  )
  result
}

make_item_names <- function(scale_id, item_numbers, suffix = "") {
  paste0(sprintf("%s_%02d", scale_id, item_numbers), suffix)
}

specifications <- list()

add_score <- function(
  score_name,
  scale,
  subscale,
  items,
  aggregation,
  possible_min,
  possible_max,
  higher_scores_mean,
  source_reference,
  reverse_items = character(0),
  excluded_items = character(0)
) {
  specifications[[length(specifications) + 1L]] <<- list(
    score_name = score_name,
    scale = scale,
    subscale = subscale,
    items = items,
    aggregation = aggregation,
    possible_min = possible_min,
    possible_max = possible_max,
    higher_scores_mean = higher_scores_mean,
    source_reference = source_reference,
    reverse_items = reverse_items,
    excluded_items = excluded_items
  )
}

# CERQ-short: nine two-item strategies. A_13 is an attention check.
cerq_source <- paste0(
  "Garnefski & Kraaij (2006), CERQ-short; adaptive/maladaptive grouping ",
  "used in https://doi.org/10.1007/s11031-023-10035-0"
)
add_score("CERQ_putting_into_perspective_mean", "CERQ-short", "Putting into perspective", c("A_01", "A_02"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_catastrophizing_mean", "CERQ-short", "Catastrophizing", c("A_03", "A_04"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_other_blame_mean", "CERQ-short", "Other-blame", c("A_05", "A_06"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_self_blame_mean", "CERQ-short", "Self-blame", c("A_07", "A_08"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_acceptance_mean", "CERQ-short", "Acceptance", c("A_09", "A_10"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_rumination_mean", "CERQ-short", "Rumination", c("A_11", "A_12"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_positive_refocusing_mean", "CERQ-short", "Positive refocusing", c("A_14", "A_15"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_refocus_on_planning_mean", "CERQ-short", "Refocus on planning", c("A_16", "A_17"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_positive_reappraisal_mean", "CERQ-short", "Positive reappraisal", c("A_18", "A_19"), "mean", 1, 5, "More frequent use of the strategy", cerq_source, excluded_items = "A_13")
add_score("CERQ_adaptive_mean", "CERQ-short", "Adaptive cognitive emotion regulation", c("A_01", "A_02", "A_09", "A_10", "A_14", "A_15", "A_16", "A_17", "A_18", "A_19"), "mean", 1, 5, "More frequent use of adaptive strategies", cerq_source, excluded_items = "A_13")
add_score("CERQ_maladaptive_mean", "CERQ-short", "Maladaptive cognitive emotion regulation", c("A_03", "A_04", "A_05", "A_06", "A_07", "A_08", "A_11", "A_12"), "mean", 1, 5, "More frequent use of maladaptive strategies", cerq_source, excluded_items = "A_13")

add_score("ERQ_reappraisal_mean", "ERQ-J", "Cognitive reappraisal", make_item_names("B", 1:6), "mean", 1, 7, "Greater use of cognitive reappraisal", "Gross & John (2003); current questionnaire attachment form 2")
add_score("EAC_emotional_processing_mean", "EAC", "Emotional processing", make_item_names("C", 1:8), "mean", 1, 4, "Greater emotional processing", "Stanton et al. (2000), https://doi.org/10.1037/0022-3514.78.6.1150; current questionnaire uses the eight emotional-processing items")
add_score("PANAS_positive_affect_mean", "PANAS", "Positive affect", make_item_names("D", 1:10), "mean", 1, 6, "More positive affect", "Current questionnaire attachment form 4; item order was normalized by 01_merge_raw_data.R")
add_score("SWLS_life_satisfaction_mean", "SWLS", "Life satisfaction", make_item_names("E", 1:3), "mean", 1, 7, "Greater life satisfaction", "Three-item selection used in https://doi.org/10.1007/s11031-023-10035-0")
add_score("SVS_vitality_mean", "SVS-J", "Subjective vitality", make_item_names("F", 1:3), "mean", 1, 7, "Greater subjective vitality", "Three-item selection used in https://doi.org/10.1007/s11031-023-10035-0")

pwbs_source <- paste0(
  "PWBS-42 Japanese version: https://mhpn.m.u-tokyo.ac.jp/rs/pwb-42; ",
  "six-item selection and overall mean used in https://doi.org/10.1007/s11031-023-10035-0"
)
add_score("PWBS_personal_growth_mean", "PWBS-42 selection", "Personal growth", c("G_02", "G_03", "G_04_rev"), "mean", 1, 7, "Greater personal growth", pwbs_source, reverse_items = "G_04")
add_score("PWBS_purpose_in_life_mean", "PWBS-42 selection", "Purpose in life", c("G_01_rev", "G_05", "G_06_rev"), "mean", 1, 7, "Greater purpose in life", pwbs_source, reverse_items = c("G_01", "G_06"))
add_score("PWBS_eudaimonic_wellbeing_mean", "PWBS-42 selection", "Eudaimonic well-being", c("G_01_rev", "G_02", "G_03", "G_04_rev", "G_05", "G_06_rev"), "mean", 1, 7, "Greater eudaimonic well-being", pwbs_source, reverse_items = c("G_01", "G_04", "G_06"))

ecr_source <- "Nakao & Kato (2004), ECR-GO Table 1, https://doi.org/10.15017/3567"
ecr_anxiety_items <- c("H_02", "H_04", "H_05", "H_06", "H_08", "H_09", "H_10", "H_12", "H_15", "H_17", "H_19_rev", "H_21", "H_24", "H_26", "H_28", "H_30", "H_31", "H_32")
ecr_avoidance_items <- c("H_01", "H_03_rev", "H_07", "H_11_rev", "H_14", "H_16_rev", "H_18", "H_20", "H_22_rev", "H_25_rev", "H_27_rev", "H_29_rev")
add_score("ECRGO_attachment_anxiety_mean", "ECR-GO", "Attachment anxiety", ecr_anxiety_items, "mean", 1, 7, "Greater fear of abandonment", ecr_source, reverse_items = "H_19", excluded_items = c("H_13", "H_23"))
add_score("ECRGO_avoidance_of_intimacy_mean", "ECR-GO", "Avoidance of intimacy", ecr_avoidance_items, "mean", 1, 7, "Greater avoidance of intimacy", ecr_source, reverse_items = c("H_03", "H_11", "H_16", "H_22", "H_25", "H_27", "H_29"), excluded_items = c("H_13", "H_23"))

# ECS: Table 3 of the validation paper identifies items 2, 5, and 8 as
# awareness; the remaining seven retained items form action.
ecs_source <- "Van der Kaap-Deeder et al. (2023), Table 3 and Appendix 1, https://doi.org/10.1007/s11031-023-10035-0"
ecs_versions <- c(
  I = "general", J = "happiness", K = "satisfaction", L = "enthusiasm",
  M = "pride", N = "fascination", O = "being_loved", P = "feeling_energetic"
)

for (scale_id in names(ecs_versions)) {
  version <- unname(ecs_versions[[scale_id]])
  add_score(
    paste0("ECS_", version, "_awareness_mean"),
    paste0("ECS-", version),
    "Awareness",
    make_item_names(scale_id, c(2, 5, 8)),
    "mean", 1, 5,
    "Greater awareness of positive-emotion-inducing contexts",
    ecs_source
  )
  add_score(
    paste0("ECS_", version, "_action_mean"),
    paste0("ECS-", version),
    "Action",
    make_item_names(scale_id, c(1, 3, 4, 6, 7, 9, 10)),
    "mean", 1, 5,
    "More proactive action to initiate, maintain, or increase positive emotion",
    ecs_source
  )
}

add_score("K6_psychological_distress_total", "K6", "Psychological distress", make_item_names("Q", 1:6), "sum", 0, 24, "Greater psychological distress", "Kessler et al. (2002), https://doi.org/10.1017/S0033291702006074")

score_names <- vapply(specifications, `[[`, character(1), "score_name")
assert_condition(!anyDuplicated(score_names), "Scoring specification contains duplicate score names")
assert_condition(length(score_names) == 38L, "Expected 38 scale/subscale scores")

required_item_columns <- unique(unlist(lapply(specifications, `[[`, "items"), use.names = FALSE))
required_columns <- c("participant_id", "source_file", "age", "gender", "education", "marital_status", "income", "gender_code", "education_code", "marital_status_code", "income_code", "attention_all_pass", required_item_columns)

prepared_data <- read_utf8_csv(input_file, all_character = TRUE)
assert_condition(nrow(prepared_data) > 0L, "Input contains no participant rows")
assert_condition(!anyDuplicated(names(prepared_data)), "Input contains duplicate column names")
missing_columns <- setdiff(required_columns, names(prepared_data))
assert_condition(
  length(missing_columns) == 0L,
  sprintf("Input is missing required columns: %s", paste(missing_columns, collapse = ", "))
)
assert_condition(all(nzchar(prepared_data$participant_id)), "participant_id contains blank values")
assert_condition(!anyDuplicated(prepared_data$participant_id), "participant_id contains duplicates")

attention_all_pass <- parse_logical_column(prepared_data$attention_all_pass, "attention_all_pass")
assert_condition(all(attention_all_pass), "Prepared input still contains failed attention checks")

item_minimums <- c(A = 1, B = 1, C = 1, D = 1, E = 1, F = 1, G = 1, H = 1, I = 1, J = 1, K = 1, L = 1, M = 1, N = 1, O = 1, P = 1, Q = 0)
item_maximums <- c(A = 5, B = 7, C = 4, D = 6, E = 7, F = 7, G = 7, H = 7, I = 5, J = 5, K = 5, L = 5, M = 5, N = 5, O = 5, P = 5, Q = 4)

for (item_name in required_item_columns) {
  prepared_data[[item_name]] <- parse_numeric_column(prepared_data[[item_name]], item_name)
  scale_id <- substr(item_name, 1L, 1L)
  values <- prepared_data[[item_name]]
  present <- !is.na(values)
  assert_condition(
    all(values[present] == floor(values[present])),
    sprintf("%s contains non-integer item responses", item_name)
  )
  assert_condition(
    all(values[present] >= item_minimums[[scale_id]] & values[present] <= item_maximums[[scale_id]]),
    sprintf("%s contains values outside %d-%d", item_name, item_minimums[[scale_id]], item_maximums[[scale_id]])
  )
}

reverse_pairs <- c(
  G_01 = "G_01_rev", G_04 = "G_04_rev", G_06 = "G_06_rev",
  H_03 = "H_03_rev", H_11 = "H_11_rev", H_16 = "H_16_rev",
  H_19 = "H_19_rev", H_22 = "H_22_rev", H_25 = "H_25_rev",
  H_27 = "H_27_rev", H_29 = "H_29_rev"
)

for (raw_name in names(reverse_pairs)) {
  reversed_name <- unname(reverse_pairs[[raw_name]])
  assert_condition(
    all(c(raw_name, reversed_name) %in% names(prepared_data)),
    sprintf("Reverse-scoring pair is missing: %s / %s", raw_name, reversed_name)
  )
  raw_values <- parse_numeric_column(prepared_data[[raw_name]], raw_name)
  reversed_values <- parse_numeric_column(prepared_data[[reversed_name]], reversed_name)
  matches <- (is.na(raw_values) & is.na(reversed_values)) |
    (!is.na(raw_values) & !is.na(reversed_values) & reversed_values == 8 - raw_values)
  assert_condition(all(matches), sprintf("Stored reverse score does not match 8 - raw for %s", raw_name))
  prepared_data[[raw_name]] <- raw_values
  prepared_data[[reversed_name]] <- reversed_values
}

compute_score <- function(data, specification) {
  item_matrix <- as.matrix(data[, specification$items, drop = FALSE])
  storage.mode(item_matrix) <- "double"
  answered_items <- rowSums(!is.na(item_matrix))
  required_count <- ceiling(length(specification$items) * minimum_answered_proportion - 1e-12)

  if (identical(specification$aggregation, "mean")) {
    result <- rowMeans(item_matrix, na.rm = TRUE)
  } else if (identical(specification$aggregation, "sum")) {
    result <- rowSums(item_matrix, na.rm = TRUE)
  } else {
    stop(sprintf("Unknown aggregation: %s", specification$aggregation), call. = FALSE)
  }

  result[answered_items < required_count] <- NA_real_
  result
}

score_data <- data.frame(
  row.names = seq_len(nrow(prepared_data)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
for (specification in specifications) {
  score_data[[specification$score_name]] <- compute_score(prepared_data, specification)
  values <- score_data[[specification$score_name]]
  present <- !is.na(values)
  assert_condition(
    all(values[present] >= specification$possible_min - 1e-10 & values[present] <= specification$possible_max + 1e-10),
    sprintf("Calculated score outside possible range: %s", specification$score_name)
  )
}

full_output <- cbind(prepared_data, score_data, stringsAsFactors = FALSE)

demographic_columns <- c(
  "participant_id", "source_file", "age", "gender", "education",
  "marital_status", "income", "gender_code", "education_code",
  "marital_status_code", "income_code"
)
compact_output <- cbind(
  prepared_data[, demographic_columns, drop = FALSE],
  score_data,
  stringsAsFactors = FALSE
)

missing_rule_text <- sprintf(
  "NA unless at least %d%% of required items are answered; no imputation.",
  as.integer(round(minimum_answered_proportion * 100))
)

codebook_rows <- lapply(specifications, function(specification) {
  data.frame(
    score_variable = specification$score_name,
    scale = specification$scale,
    subscale = specification$subscale,
    items_used = paste(specification$items, collapse = "|"),
    item_count = length(specification$items),
    aggregation = specification$aggregation,
    possible_min = specification$possible_min,
    possible_max = specification$possible_max,
    higher_scores_mean = specification$higher_scores_mean,
    reverse_items = paste(specification$reverse_items, collapse = "|"),
    attention_items_excluded = paste(specification$excluded_items, collapse = "|"),
    missing_rule = missing_rule_text,
    source_reference = specification$source_reference,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})
score_codebook <- do.call(rbind, codebook_rows)
row.names(score_codebook) <- NULL

assert_condition(nrow(full_output) == nrow(prepared_data), "Full-output row count changed")
assert_condition(nrow(compact_output) == nrow(prepared_data), "Compact-output row count changed")
assert_condition(identical(full_output$participant_id, prepared_data$participant_id), "Participant order changed in full output")
assert_condition(identical(compact_output$participant_id, prepared_data$participant_id), "Participant order changed in compact output")
assert_condition(nrow(score_codebook) == length(score_names), "Codebook does not contain one row per score")

write_utf8_csv(full_output, full_output_file)
write_utf8_csv(compact_output, compact_output_file)
write_utf8_csv(score_codebook, codebook_output_file)

# Read saved files back to verify their structure and UTF-8 content.
full_verification <- read_utf8_csv(full_output_file, all_character = TRUE)
compact_verification <- read_utf8_csv(compact_output_file, all_character = TRUE)
codebook_verification <- read_utf8_csv(codebook_output_file, all_character = TRUE)

assert_condition(nrow(full_verification) == nrow(prepared_data), "Saved full-output row count mismatch")
assert_condition(nrow(compact_verification) == nrow(prepared_data), "Saved compact-output row count mismatch")
assert_condition(nrow(codebook_verification) == length(score_names), "Saved codebook row count mismatch")
assert_condition(identical(names(full_verification), names(full_output)), "Saved full-output column order mismatch")
assert_condition(identical(names(compact_verification), names(compact_output)), "Saved compact-output column order mismatch")
assert_condition(identical(full_verification$participant_id, prepared_data$participant_id), "Saved full-output participant order mismatch")
assert_condition(identical(compact_verification$participant_id, prepared_data$participant_id), "Saved compact-output participant order mismatch")
assert_condition(identical(codebook_verification$score_variable, score_names), "Saved codebook score order mismatch")

saved_text <- paste(
  readLines(full_output_file, warn = FALSE, encoding = "UTF-8"),
  readLines(compact_output_file, warn = FALSE, encoding = "UTF-8"),
  readLines(codebook_output_file, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
assert_condition(
  !grepl("<U\\+[0-9A-Fa-f]{4,6}>", saved_text),
  "Saved output contains escaped Unicode markers instead of UTF-8 text"
)

missing_score_count <- sum(is.na(as.matrix(score_data)))

message(sprintf("Input participants: %d", nrow(prepared_data)))
message(sprintf("Scale/subscale scores created: %d", length(score_names)))
message(sprintf("Missing calculated score cells: %d", missing_score_count))
message(sprintf("Full analysis output: %s", full_output_file))
message(sprintf("Compact score output: %s", compact_output_file))
message(sprintf("Score codebook: %s", codebook_output_file))
message("All score-construction checks passed.")
