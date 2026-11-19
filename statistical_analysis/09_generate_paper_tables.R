# Publication-ready CSV tables corresponding to Tables 2, 3, and 4
# in Van der Kaap-Deeder et al. (2023), adapted to the present study.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")

output_directory <- ensure_directory(file.path(results_directory, "09_paper_tables"))

read_result <- function(...) {
  read_utf8_csv(file.path(results_directory, ...), all_character = FALSE)
}

require_columns <- function(data, columns, label) {
  missing_columns <- setdiff(columns, names(data))
  assert_condition(
    length(missing_columns) == 0L,
    sprintf("%s is missing columns: %s", label, paste(missing_columns, collapse = ", "))
  )
}

paper_labels <- read_utf8_csv(
  file.path(analysis_script_directory, "09_paper_table_labels.csv"),
  all_character = TRUE
)
require_columns(paper_labels, c("group", "key", "label_ja"), "Paper table labels")

get_paper_labels <- function(group, keys) {
  group_rows <- paper_labels[paper_labels$group == group, , drop = FALSE]
  matched_rows <- match(keys, group_rows$key)
  assert_condition(
    !any(is.na(matched_rows)),
    sprintf("Missing %s labels: %s", group, paste(keys[is.na(matched_rows)], collapse = ", "))
  )
  unname(group_rows$label_ja[matched_rows])
}

# -----------------------------------------------------------------------------
# Table 2: goodness-of-fit indices
# -----------------------------------------------------------------------------

efa_fit <- read_result("05_efa_ecs", "05_efa_fit_indices.csv")
cfa_fit <- read_result("05_cfa_ecs", "05_cfa_fit_indices.csv")
sem_fit <- read_result("06_sem_primary", "06_sem_fit_indices.csv")

fit_columns <- c(
  "scale", "model", "n", "converged", "chisq", "df", "chisq_df",
  "p_value", "cfi", "tli", "srmr", "rmsea", "rmsea_ci_lower",
  "rmsea_ci_upper", "aic", "bic"
)
require_columns(efa_fit, fit_columns, "EFA fit results")
require_columns(cfa_fit, fit_columns, "CFA fit results")
require_columns(sem_fit, fit_columns, "SEM fit results")

make_fit_rows <- function(data, section, labels, source_file) {
  assert_condition(nrow(data) == length(labels), sprintf("Unexpected row count for %s", section))
  result <- data[, fit_columns, drop = FALSE]
  result$section <- section
  result$model_label_ja <- unname(labels)
  result$source_file <- source_file
  result[, c(
    "section", "model_label_ja", "source_file", "scale", "model", "n",
    "chisq_df", "chisq", "df", "cfi", "tli", "srmr", "rmsea",
    "rmsea_ci_lower", "rmsea_ci_upper", "p_value", "aic", "bic", "converged"
  )]
}

efa_rows <- efa_fit[
  efa_fit$scale == "general" & efa_fit$model %in% c("1_factor", "2_factor"),
  , drop = FALSE
]
efa_rows <- efa_rows[match(c("1_factor", "2_factor"), efa_rows$model), , drop = FALSE]

cfa_scale_order <- c(
  "general", "happiness", "satisfaction", "enthusiasm", "pride",
  "fascination", "being_loved", "feeling_energetic"
)
cfa_rows <- cfa_fit[
  cfa_fit$model == "two_factor" & cfa_fit$scale %in% cfa_scale_order,
  , drop = FALSE
]
cfa_rows <- cfa_rows[match(cfa_scale_order, cfa_rows$scale), , drop = FALSE]

sem_model_order <- c("model1_sequential", "model2_er_controls")
sem_rows <- sem_fit[sem_fit$model %in% sem_model_order, , drop = FALSE]
sem_rows <- sem_rows[match(sem_model_order, sem_rows$model), , drop = FALSE]

table2 <- rbind(
  make_fit_rows(
    efa_rows,
    "EFA",
    get_paper_labels("table2", c("efa_1_factor", "efa_2_factor")),
    "05_efa_ecs/05_efa_fit_indices.csv"
  ),
  make_fit_rows(
    cfa_rows,
    "CFA",
    get_paper_labels("table2", paste0("cfa_", cfa_scale_order)),
    "05_cfa_ecs/05_cfa_fit_indices.csv"
  ),
  make_fit_rows(
    sem_rows,
    "SEM",
    get_paper_labels("table2", c("sem_model1_sequential", "sem_model2_er_controls")),
    "06_sem_primary/06_sem_fit_indices.csv"
  )
)
table2$table_order <- seq_len(nrow(table2))
table2 <- table2[, c("table_order", setdiff(names(table2), "table_order"))]
write_utf8_csv(
  round_numeric_columns(table2, 4L),
  file.path(output_directory, "09_table2_model_fit_indices.csv")
)

# -----------------------------------------------------------------------------
# Table 3: standardized loadings, R-squared, item means, and SDs
# -----------------------------------------------------------------------------

cfa_loadings <- read_result("05_cfa_ecs", "05_cfa_standardized_loadings.csv")
item_diagnostics <- read_result("02_reliability", "02_item_diagnostics.csv")
require_columns(
  cfa_loadings,
  c("scale", "lhs", "rhs", "std.all", "r_squared", "pvalue"),
  "CFA standardized loadings"
)
require_columns(
  item_diagnostics,
  c("scale", "item", "mean", "sd"),
  "Item diagnostics"
)

item_order <- c("I_05", "I_02", "I_08", "I_01", "I_03", "I_04", "I_06", "I_07", "I_09", "I_10")
item_text_ja <- stats::setNames(get_paper_labels("table3", item_order), item_order)

general_loadings <- cfa_loadings[cfa_loadings$scale == "general", , drop = FALSE]
general_loadings <- general_loadings[match(item_order, general_loadings$rhs), , drop = FALSE]
assert_condition(!any(is.na(general_loadings$rhs)), "General CFA loadings do not contain all ten ECS items")

general_item_diagnostics <- item_diagnostics[
  item_diagnostics$scale %in% c("ECS_general_awareness", "ECS_general_action"),
  , drop = FALSE
]
diagnostic_match <- match(item_order, general_item_diagnostics$item)
assert_condition(!any(is.na(diagnostic_match)), "Item diagnostics do not contain all ten general ECS items")
general_item_diagnostics <- general_item_diagnostics[diagnostic_match, , drop = FALSE]

table3 <- data.frame(
  table_order = seq_along(item_order),
  item_id = item_order,
  item_text_ja = unname(item_text_ja[item_order]),
  factor = general_loadings$lhs,
  awareness_loading = ifelse(general_loadings$lhs == "awareness", general_loadings$std.all, NA_real_),
  action_loading = ifelse(general_loadings$lhs == "action", general_loadings$std.all, NA_real_),
  standardized_loading = general_loadings$std.all,
  r_squared = general_loadings$r_squared,
  item_mean = general_item_diagnostics$mean,
  item_sd = general_item_diagnostics$sd,
  loading_p_value = general_loadings$pvalue,
  loading_source_file = "05_cfa_ecs/05_cfa_standardized_loadings.csv",
  descriptive_source_file = "02_reliability/02_item_diagnostics.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_utf8_csv(
  round_numeric_columns(table3, 4L),
  file.path(output_directory, "09_table3_cfa_item_statistics.csv")
)

# -----------------------------------------------------------------------------
# Table 4: means, SDs, and lower-triangle Pearson correlations
# -----------------------------------------------------------------------------

descriptives <- read_result("01_descriptive", "01_scale_score_descriptives.csv")
correlations <- read_result("03_correlations", "03_correlation_long.csv")
require_columns(descriptives, c("variable", "n", "missing", "mean", "sd"), "Scale descriptives")
require_columns(
  correlations,
  c("variable_1", "variable_2", "n", "r", "p_value", "p_fdr"),
  "Correlation results"
)

table4_variables <- c(
  "ECS_general_awareness_mean",
  "ECS_general_action_mean",
  "CERQ_adaptive_mean",
  "CERQ_maladaptive_mean",
  "ERQ_reappraisal_mean",
  "EAC_emotional_processing_mean",
  "PANAS_positive_affect_mean",
  "SWLS_life_satisfaction_mean",
  "SVS_vitality_mean",
  "PWBS_eudaimonic_wellbeing_mean",
  "K6_psychological_distress_total",
  "ECRGO_attachment_anxiety_mean",
  "ECRGO_avoidance_of_intimacy_mean"
)
table4_labels <- get_paper_labels("table4", table4_variables)

descriptive_match <- match(table4_variables, descriptives$variable)
assert_condition(!any(is.na(descriptive_match)), "Descriptives do not contain all Table 4 variables")
table4_descriptives <- descriptives[descriptive_match, , drop = FALSE]

significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  ""
}

format_correlation <- function(variable_1, variable_2) {
  row <- correlations[
    (correlations$variable_1 == variable_1 & correlations$variable_2 == variable_2) |
      (correlations$variable_1 == variable_2 & correlations$variable_2 == variable_1),
    , drop = FALSE
  ]
  assert_condition(nrow(row) == 1L, sprintf("Expected one correlation for %s and %s", variable_1, variable_2))
  value <- sub("^(-?)0\\.", "\\1.", sprintf("%.2f", as.numeric(row$r[[1]])))
  paste0(value, significance_stars(as.numeric(row$p_value[[1]])))
}

correlation_matrix <- matrix(
  "",
  nrow = length(table4_variables),
  ncol = length(table4_variables),
  dimnames = list(NULL, as.character(seq_along(table4_variables)))
)
for (row_index in seq_along(table4_variables)) {
  for (column_index in seq_along(table4_variables)) {
    if (column_index == row_index) {
      correlation_matrix[row_index, column_index] <- "\u2014"
    } else if (column_index < row_index) {
      correlation_matrix[row_index, column_index] <- format_correlation(
        table4_variables[[row_index]], table4_variables[[column_index]]
      )
    }
  }
}

table4 <- data.frame(
  number = seq_along(table4_variables),
  variable_label_ja = table4_labels,
  variable = table4_variables,
  n = table4_descriptives$n,
  missing = table4_descriptives$missing,
  mean = round(as.numeric(table4_descriptives$mean), 2L),
  sd = round(as.numeric(table4_descriptives$sd), 2L),
  correlation_matrix,
  descriptive_source_file = "01_descriptive/01_scale_score_descriptives.csv",
  correlation_source_file = "03_correlations/03_correlation_long.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_utf8_csv(table4, file.path(output_directory, "09_table4_descriptive_correlations.csv"))

# -----------------------------------------------------------------------------
# Table 5: indirect effects in the concurrent-validity SEM models
# -----------------------------------------------------------------------------

sem_result_directory <- file.path(results_directory, "06_sem_primary")
bootstrap_status_path <- file.path(sem_result_directory, "06_bootstrap_status.txt")
diagnostic_path <- file.path(sem_result_directory, "06_sem_diagnostics.txt")

bootstrap_status_lines <- if (file.exists(bootstrap_status_path)) {
  readLines(bootstrap_status_path, encoding = "UTF-8", warn = FALSE)
} else {
  character(0)
}
bootstrap_draws_line <- grep("^Bootstrap draws requested:", bootstrap_status_lines, value = TRUE)
bootstrap_draws <- if (length(bootstrap_draws_line) == 1L) {
  suppressWarnings(as.integer(sub("^Bootstrap draws requested:[[:space:]]*", "", bootstrap_draws_line)))
} else {
  0L
}
if (is.na(bootstrap_draws)) bootstrap_draws <- 0L

bootstrap_files <- c(
  model1_sequential = "06_model1_sequential_bootstrap_indirects.csv",
  model2_er_controls = "06_model2_er_controls_bootstrap_indirects.csv"
)
parameter_files <- c(
  model1_sequential = "06_model1_sequential_parameters.csv",
  model2_er_controls = "06_model2_er_controls_parameters.csv"
)
use_bootstrap_results <- bootstrap_draws >= 1000L && all(
  file.exists(file.path(sem_result_directory, bootstrap_files))
)

diagnostic_lines <- if (file.exists(diagnostic_path)) {
  readLines(diagnostic_path, encoding = "UTF-8", warn = FALSE)
} else {
  character(0)
}

read_diagnostic_flag <- function(model_name, flag_name) {
  pattern <- paste0("^", model_name, ": ", flag_name, "=")
  matched <- grep(pattern, diagnostic_lines, value = TRUE)
  if (length(matched) != 1L) return(NA)
  value <- sub(pattern, "", matched)
  if (value == "TRUE") return(TRUE)
  if (value == "FALSE") return(FALSE)
  NA
}

table5_effect_order <- list(
  model1_sequential = c(
    "indirect_life_satisfaction", "indirect_vitality", "indirect_eudaimonic", "indirect_distress",
    "ecr_anxiety_indirect_life", "ecr_anxiety_indirect_vitality",
    "ecr_anxiety_indirect_eudaimonic", "ecr_anxiety_indirect_distress",
    "ecr_avoidance_indirect_life", "ecr_avoidance_indirect_vitality",
    "ecr_avoidance_indirect_eudaimonic", "ecr_avoidance_indirect_distress"
  ),
  model2_er_controls = c(
    "indirect_life_satisfaction", "indirect_vitality", "indirect_eudaimonic", "indirect_distress",
    "ecr_anxiety_indirect_life", "ecr_anxiety_indirect_vitality",
    "ecr_anxiety_indirect_eudaimonic", "ecr_anxiety_indirect_distress",
    "ecr_avoidance_indirect_life", "ecr_avoidance_indirect_vitality",
    "ecr_avoidance_indirect_eudaimonic", "ecr_avoidance_indirect_distress",
    "adaptive_via_pa_life", "adaptive_via_pa_vitality",
    "adaptive_via_pa_eudaimonic", "adaptive_via_pa_distress",
    "maladaptive_via_pa_life", "maladaptive_via_pa_vitality",
    "maladaptive_via_pa_eudaimonic", "maladaptive_via_pa_distress"
  )
)

table5_effect_group <- function(effect_id) {
  if (grepl("^ecr_anxiety_", effect_id)) return("ecr_attachment_anxiety_extension")
  if (grepl("^ecr_avoidance_", effect_id)) return("ecr_intimacy_avoidance_extension")
  if (grepl("^adaptive_", effect_id)) return("adaptive_er_control")
  if (grepl("^maladaptive_", effect_id)) return("maladaptive_er_control")
  "primary_ec_chain"
}

format_original_style_ci_value <- function(value) {
  if (is.na(value)) return("")
  rounded_magnitude <- floor(abs(value) * 1000 + 0.5) / 1000
  formatted <- sub("^0\\.", ".", sprintf("%.3f", rounded_magnitude))
  if (value < 0) paste0("\u2212", formatted) else formatted
}

format_original_style_ci <- function(lower, upper) {
  vapply(
    seq_along(lower),
    function(index) {
      if (is.na(lower[[index]]) || is.na(upper[[index]])) return("")
      paste0(
        "[",
        format_original_style_ci_value(lower[[index]]),
        ", ",
        format_original_style_ci_value(upper[[index]]),
        "]"
      )
    },
    character(1)
  )
}

make_table5_rows <- function(model_name) {
  selected_file <- if (use_bootstrap_results) bootstrap_files[[model_name]] else parameter_files[[model_name]]
  source_path <- file.path(sem_result_directory, selected_file)
  data <- read_utf8_csv(source_path, all_character = FALSE)
  require_columns(
    data,
    c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all"),
    paste("SEM indirect effects", model_name)
  )
  effect_order <- table5_effect_order[[model_name]]
  data <- data[data$op == ":=" & data$lhs %in% effect_order, , drop = FALSE]
  data <- data[match(effect_order, data$lhs), , drop = FALSE]
  assert_condition(!any(is.na(data$lhs)), paste("Missing indirect effects for", model_name))

  ci_lower <- as.numeric(data$ci.lower)
  ci_upper <- as.numeric(data$ci.upper)
  ci_excludes_zero <- (ci_lower > 0 & ci_upper > 0) | (ci_lower < 0 & ci_upper < 0)
  model_converged <- read_diagnostic_flag(model_name, "converged")
  model_admissible <- read_diagnostic_flag(model_name, "admissible_post_check")
  final_reporting_ready <- isTRUE(use_bootstrap_results) && isTRUE(model_converged) && isTRUE(model_admissible)

  data.frame(
    model = model_name,
    model_label_ja = get_paper_labels(
      "table2",
      if (model_name == "model1_sequential") "sem_model1_sequential" else "sem_model2_er_controls"
    ),
    effect_group = vapply(data$lhs, table5_effect_group, character(1)),
    indirect_effect_id = data$lhs,
    indirect_path_ja = get_paper_labels("table5", data$lhs),
    path_formula = data$rhs,
    estimate_unstandardized = as.numeric(data$est),
    standardized_indirect_effect = as.numeric(data$std.all),
    se = as.numeric(data$se),
    z = as.numeric(data$z),
    p_value = as.numeric(data$pvalue),
    ci_95_lower = ci_lower,
    ci_95_upper = ci_upper,
    ci_95_display = format_original_style_ci(ci_lower, ci_upper),
    ci_excludes_zero = ci_excludes_zero,
    ci_method = if (use_bootstrap_results) "percentile_bootstrap" else "normal_theory_delta_method_provisional",
    bootstrap_draws = bootstrap_draws,
    model_converged = model_converged,
    model_admissible = model_admissible,
    final_reporting_ready = final_reporting_ready,
    report_in_original_style = ci_excludes_zero & final_reporting_ready,
    source_file = paste0("06_sem_primary/", selected_file),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

table5 <- rbind(
  make_table5_rows("model1_sequential"),
  make_table5_rows("model2_er_controls")
)
table5$table_order <- seq_len(nrow(table5))
table5 <- table5[, c("table_order", setdiff(names(table5), "table_order"))]
write_utf8_csv(
  round_numeric_columns(table5, 4L),
  file.path(output_directory, "09_table5_indirect_effects.csv")
)

write_analysis_session_info(
  file.path(output_directory, "09_session_info.txt"),
  c(
    "Generated publication-ready CSV files for Tables 2, 3, 4, and 5.",
    "Table 3 R-squared equals the squared standardized loading.",
    "Table 4 stars use unadjusted p-values: * p < .05; ** p < .01; *** p < .001.",
    "Table 4 includes ECR-GO attachment anxiety and intimacy avoidance as study-specific additions.",
    sprintf("Table 5 confidence-interval method: %s.", if (use_bootstrap_results) "percentile bootstrap" else "provisional normal-theory delta method"),
    sprintf("Table 5 bootstrap draws available: %d.", bootstrap_draws),
    "Table 5 includes the study-specific ECR-GO indirect paths and retains all requested indirect paths for auditability.",
    "Use report_in_original_style only when final_reporting_ready is TRUE."
  )
)

message("Publication-ready CSV files for Tables 2-5 generated.")
