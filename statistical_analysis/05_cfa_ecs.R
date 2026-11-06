# Confirmatory factor analyses for the general ECS and seven emotion versions.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")
require_packages("lavaan")

output_directory <- ensure_directory(file.path(results_directory, "05_cfa_ecs"))
data <- load_analysis_data()

fit_rows <- list()
loading_rows <- list()
comparison_rows <- list()
diagnostic_lines <- character(0)
summary_lines <- character(0)

for (scale_id in names(ecs_versions)) {
  scale_name <- unname(ecs_versions[[scale_id]])
  items <- make_item_names(scale_id, 1:10)
  missing_items <- setdiff(items, names(data))
  assert_condition(length(missing_items) == 0L, sprintf("Missing ECS items for %s", scale_name))

  analysis_data <- as.data.frame(
    lapply(items, function(item) numeric_column(data[[item]], item)),
    check.names = FALSE
  )
  names(analysis_data) <- items

  one_factor_model <- paste0("emotion_crafting =~ ", paste(items, collapse = " + "))
  awareness_items <- make_item_names(scale_id, c(2, 5, 8))
  action_items <- make_item_names(scale_id, c(1, 3, 4, 6, 7, 9, 10))
  two_factor_model <- paste(
    paste0("awareness =~ ", paste(awareness_items, collapse = " + ")),
    paste0("action =~ ", paste(action_items, collapse = " + ")),
    paste0(awareness_items[[2]], " ~~ ", awareness_items[[3]]),
    sep = "\n"
  )

  fit_one <- lavaan::cfa(
    one_factor_model, data = analysis_data, estimator = "MLR",
    missing = "fiml", meanstructure = TRUE
  )
  fit_two <- lavaan::cfa(
    two_factor_model, data = analysis_data, estimator = "MLR",
    missing = "fiml", meanstructure = TRUE
  )

  fit_rows[[paste0(scale_name, "_one")]] <- lavaan_fit_row(fit_one, "one_factor", scale_name)
  fit_rows[[paste0(scale_name, "_two")]] <- lavaan_fit_row(fit_two, "two_factor", scale_name)

  parameters <- lavaan::parameterEstimates(fit_two, standardized = TRUE, ci = TRUE)
  loadings <- parameters[parameters$op == "=~", c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")]
  loadings$scale <- scale_name
  loading_rows[[scale_name]] <- loadings[, c("scale", "lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")]

  comparison <- tryCatch(
    suppressWarnings(lavaan::lavTestLRT(fit_one, fit_two, method = "satorra.bentler.2001")),
    error = function(error) error
  )
  if (inherits(comparison, "error")) {
    comparison_rows[[scale_name]] <- data.frame(scale = scale_name, note = conditionMessage(comparison), stringsAsFactors = FALSE)
  } else {
    comparison_data <- data.frame(scale = scale_name, model = row.names(comparison), comparison, check.names = FALSE)
    row.names(comparison_data) <- NULL
    comparison_rows[[scale_name]] <- comparison_data
  }

  residual_variances <- parameters[parameters$op == "~~" & parameters$lhs == parameters$rhs, ]
  negative_variances <- residual_variances$rhs[residual_variances$est < 0]
  latent_correlation <- parameters[
    parameters$op == "~~" & parameters$lhs == "awareness" & parameters$rhs == "action",
    "std.all"
  ]
  diagnostic_lines <- c(
    diagnostic_lines,
    sprintf(
      "%s: converged=%s; negative residual variances=%s; awareness-action r=%s",
      scale_name,
      lavaan::lavInspect(fit_two, "converged"),
      if (length(negative_variances)) paste(negative_variances, collapse = "|") else "none",
      if (length(latent_correlation)) format(round(latent_correlation, 3), nsmall = 3) else "not available"
    )
  )
  summary_lines <- c(
    summary_lines,
    paste0("===== ", scale_name, " two-factor CFA ====="),
    capture.output(summary(fit_two, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)),
    ""
  )
}

fit_indices <- do.call(rbind, fit_rows)
row.names(fit_indices) <- NULL
fit_indices$fit_interpretation <- ifelse(
  fit_indices$model == "two_factor" & fit_indices$cfi >= 0.95 & fit_indices$tli >= 0.95 & fit_indices$rmsea <= 0.06 & fit_indices$srmr <= 0.05,
  "good_by_original_paper_thresholds",
  ifelse(
    fit_indices$model == "two_factor" & fit_indices$cfi >= 0.90 & fit_indices$tli >= 0.90 & fit_indices$rmsea <= 0.08 & fit_indices$srmr <= 0.10,
    "acceptable_by_original_paper_thresholds",
    "below_original_paper_thresholds"
  )
)

write_utf8_csv(round_numeric_columns(fit_indices, 4L), file.path(output_directory, "05_cfa_fit_indices.csv"))
write_utf8_csv(round_numeric_columns(do.call(rbind, loading_rows), 4L), file.path(output_directory, "05_cfa_standardized_loadings.csv"))
write_utf8_csv(round_numeric_columns(do.call(rbind, comparison_rows), 4L), file.path(output_directory, "05_cfa_model_comparisons.csv"))
write_utf8_text(diagnostic_lines, file.path(output_directory, "05_cfa_diagnostics.txt"))
write_utf8_text(summary_lines, file.path(output_directory, "05_cfa_full_summaries.txt"))
write_analysis_session_info(
  file.path(output_directory, "05_session_info.txt"),
  c(
    sprintf("Participants: %d", nrow(data)),
    "Estimator: MLR (maximum likelihood with robust standard errors).",
    "Two-factor model: awareness items 2/5/8; action items 1/3/4/6/7/9/10.",
    "Residual covariance between awareness items 5 and 8 follows the original paper.",
    "The original paper thresholds are used only as descriptive labels, not automatic acceptance decisions."
  )
)

message("CFA complete for general ECS and seven emotion-specific versions.")

