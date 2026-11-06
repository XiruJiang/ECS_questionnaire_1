# MANCOVA for background characteristics, psychological outcomes, and ECS.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")

output_directory <- ensure_directory(file.path(results_directory, "04_mancova"))
data <- load_analysis_data()
background <- prepare_background_variables(data)

outcome_variables <- c(
  "PANAS_positive_affect_mean", "SWLS_life_satisfaction_mean",
  "SVS_vitality_mean", "PWBS_eudaimonic_wellbeing_mean",
  "K6_psychological_distress_total"
)
ecs_variables <- c("ECS_general_awareness_mean", "ECS_general_action_mean")
required_scores <- c(outcome_variables, ecs_variables)
missing_scores <- setdiff(required_scores, names(data))
assert_condition(length(missing_scores) == 0L, sprintf("Missing MANCOVA scores: %s", paste(missing_scores, collapse = ", ")))

model_data <- background
for (variable_name in required_scores) {
  model_data[[variable_name]] <- numeric_column(data[[variable_name]], variable_name)
}

predictor_terms <- c("age_c", "gender", "marital", "education_c", "income_c")

make_exclusion_log <- function(model_data, required_variables, model_name) {
  complete <- stats::complete.cases(model_data[required_variables])
  reasons <- vapply(seq_len(nrow(model_data)), function(index) {
    missing <- required_variables[vapply(model_data[required_variables], function(column) is.na(column[[index]]), logical(1))]
    paste(missing, collapse = "|")
  }, character(1))
  data.frame(
    participant_id = model_data$participant_id[!complete],
    model = model_name,
    exclusion_reason = "missing_or_ineligible_background_value",
    missing_variables = reasons[!complete],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

extract_comparison <- function(comparison, term, test_name, model_name) {
  result_row <- comparison[nrow(comparison), , drop = FALSE]
  column_value <- function(name) {
    if (name %in% names(result_row)) as.numeric(result_row[[name]]) else NA_real_
  }
  statistic_name <- if (test_name == "Pillai") "Pillai" else "Wilks"
  f_value <- column_value("approx F")
  numerator_df <- column_value("num Df")
  denominator_df <- column_value("den Df")
  partial_eta_squared <- if (
    is.finite(f_value) && is.finite(numerator_df) && is.finite(denominator_df)
  ) {
    (f_value * numerator_df) / (f_value * numerator_df + denominator_df)
  } else {
    NA_real_
  }
  data.frame(
    model = model_name,
    term = term,
    test = test_name,
    statistic = column_value(statistic_name),
    approximate_f = f_value,
    numerator_df = numerator_df,
    denominator_df = denominator_df,
    p_value = column_value("Pr(>F)"),
    partial_eta_squared_approx = partial_eta_squared,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

fit_mancova <- function(model_data, outcomes, model_name) {
  required <- c(outcomes, predictor_terms)
  exclusion_log <- make_exclusion_log(model_data, required, model_name)
  complete <- stats::complete.cases(model_data[required])
  analysis_data <- droplevels(model_data[complete, , drop = FALSE])

  assert_condition(nrow(analysis_data) > 0L, sprintf("No complete rows for %s", model_name))
  assert_condition(nlevels(analysis_data$gender) >= 2L, sprintf("Gender has fewer than two levels in %s", model_name))
  assert_condition(nlevels(analysis_data$marital) >= 2L, sprintf("Marital status has fewer than two levels in %s", model_name))

  response <- paste0("cbind(", paste(outcomes, collapse = ", "), ")")
  full_formula <- stats::as.formula(paste(response, "~", paste(predictor_terms, collapse = " + ")))
  full_fit <- stats::lm(full_formula, data = analysis_data)

  multivariate_rows <- list()
  row_index <- 0L
  for (term in predictor_terms) {
    reduced_formula <- stats::update.formula(full_formula, paste(". ~ . -", term))
    reduced_fit <- stats::lm(reduced_formula, data = analysis_data)
    for (test_name in c("Pillai", "Wilks")) {
      comparison <- stats::anova(reduced_fit, full_fit, test = test_name)
      row_index <- row_index + 1L
      multivariate_rows[[row_index]] <- extract_comparison(comparison, term, test_name, model_name)
    }
  }
  multivariate_results <- do.call(rbind, multivariate_rows)

  univariate_rows <- list()
  row_index <- 0L
  for (outcome in outcomes) {
    univariate_formula <- stats::as.formula(paste(outcome, "~", paste(predictor_terms, collapse = " + ")))
    univariate_fit <- stats::lm(univariate_formula, data = analysis_data)
    marginal_tests <- stats::drop1(univariate_fit, test = "F")
    residual_df <- stats::df.residual(univariate_fit)
    for (term in predictor_terms) {
      if (!term %in% row.names(marginal_tests)) next
      f_value <- as.numeric(marginal_tests[term, "F value"])
      numerator_df <- as.numeric(marginal_tests[term, "Df"])
      partial_eta_squared <- if (is.finite(f_value)) {
        (f_value * numerator_df) / (f_value * numerator_df + residual_df)
      } else {
        NA_real_
      }
      row_index <- row_index + 1L
      univariate_rows[[row_index]] <- data.frame(
        model = model_name,
        outcome = outcome,
        term = term,
        numerator_df = numerator_df,
        denominator_df = residual_df,
        f_value = f_value,
        p_value = as.numeric(marginal_tests[term, "Pr(>F)"]),
        partial_eta_squared = partial_eta_squared,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  group_counts <- rbind(
    data.frame(model = model_name, factor = "gender", level = names(table(analysis_data$gender)), n = as.integer(table(analysis_data$gender))),
    data.frame(model = model_name, factor = "marital", level = names(table(analysis_data$marital)), n = as.integer(table(analysis_data$marital)))
  )

  list(
    n = nrow(analysis_data),
    multivariate = multivariate_results,
    univariate = do.call(rbind, univariate_rows),
    exclusions = exclusion_log,
    group_counts = group_counts,
    model_summary = capture.output(summary(stats::manova(full_fit), test = "Pillai"))
  )
}

outcome_model <- fit_mancova(model_data, outcome_variables, "psychological_outcomes")
ecs_model <- fit_mancova(model_data, ecs_variables, "ecs_awareness_action")

write_utf8_csv(
  round_numeric_columns(rbind(outcome_model$multivariate, ecs_model$multivariate), digits = 4L),
  file.path(output_directory, "04_multivariate_tests.csv")
)
write_utf8_csv(
  round_numeric_columns(rbind(outcome_model$univariate, ecs_model$univariate), digits = 4L),
  file.path(output_directory, "04_univariate_followups.csv")
)
write_utf8_csv(
  rbind(outcome_model$exclusions, ecs_model$exclusions),
  file.path(output_directory, "04_analysis_exclusions.csv")
)
write_utf8_csv(
  rbind(outcome_model$group_counts, ecs_model$group_counts),
  file.path(output_directory, "04_group_counts.csv")
)
write_utf8_text(
  c(
    "Psychological outcomes model (base-R sequential report; primary CSV uses partial Type-II comparisons)",
    outcome_model$model_summary,
    "",
    "ECS awareness/action model (base-R sequential report; primary CSV uses partial Type-II comparisons)",
    ecs_model$model_summary
  ),
  file.path(output_directory, "04_model_summaries.txt")
)
write_analysis_session_info(
  file.path(output_directory, "04_session_info.txt"),
  c(
    sprintf("Outcome MANCOVA complete cases: %d", outcome_model$n),
    sprintf("ECS MANCOVA complete cases: %d", ecs_model$n),
    "Fixed factors: binary gender and marital status.",
    "Covariates: centered age, education code, and income code.",
    "Location is unavailable in the current questionnaire and is omitted.",
    "The original HSCL-25 outcome is replaced by K6 total score."
  )
)

message(sprintf("MANCOVA complete: outcome n=%d, ECS n=%d", outcome_model$n, ecs_model$n))

