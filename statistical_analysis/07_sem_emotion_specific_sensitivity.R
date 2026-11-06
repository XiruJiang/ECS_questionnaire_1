# Supplemental path-model sensitivity analyses for seven emotion-specific ECS versions.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")
require_packages("lavaan")

output_directory <- ensure_directory(file.path(results_directory, "07_emotion_specific_sem"))
data <- load_analysis_data()
background <- prepare_background_variables(data)
analysis_data <- cbind(data, background[, setdiff(names(background), "participant_id"), drop = FALSE])
covariates <- active_covariates(background)
covariate_rhs <- paste(covariates, collapse = " + ")

control_variables <- c(
  "CERQ_adaptive_mean", "CERQ_maladaptive_mean", "ERQ_reappraisal_mean",
  "EAC_emotional_processing_mean"
)
outcomes <- c(
  "SWLS_life_satisfaction_mean", "SVS_vitality_mean",
  "PWBS_eudaimonic_wellbeing_mean", "K6_psychological_distress_total"
)

fit_rows <- list()
parameter_rows <- list()
summary_lines <- character(0)

for (scale_id in names(ecs_versions)[-1]) {
  version <- unname(ecs_versions[[scale_id]])
  awareness <- paste0("ECS_", version, "_awareness_mean")
  action <- paste0("ECS_", version, "_action_mean")
  required <- c(awareness, action, "PANAS_positive_affect_mean", control_variables, outcomes, covariates)
  complete <- stats::complete.cases(analysis_data[required])
  model_data <- analysis_data[complete, , drop = FALSE]

  model_lines <- c(
    paste0(action, " ~ a*", awareness, " + ", covariate_rhs),
    paste0("PANAS_positive_affect_mean ~ b*", action, " + er_adaptive*CERQ_adaptive_mean + er_maladaptive*CERQ_maladaptive_mean + er_reappraisal*ERQ_reappraisal_mean + er_processing*EAC_emotional_processing_mean + ", covariate_rhs),
    paste0("SWLS_life_satisfaction_mean ~ c1*PANAS_positive_affect_mean + ", covariate_rhs),
    paste0("SVS_vitality_mean ~ c2*PANAS_positive_affect_mean + ", covariate_rhs),
    paste0("PWBS_eudaimonic_wellbeing_mean ~ c3*PANAS_positive_affect_mean + direct_processing*EAC_emotional_processing_mean + ", covariate_rhs),
    paste0("K6_psychological_distress_total ~ c4*PANAS_positive_affect_mean + direct_maladaptive*CERQ_maladaptive_mean + ", covariate_rhs),
    "SWLS_life_satisfaction_mean ~~ SVS_vitality_mean + PWBS_eudaimonic_wellbeing_mean + K6_psychological_distress_total",
    "SVS_vitality_mean ~~ PWBS_eudaimonic_wellbeing_mean + K6_psychological_distress_total",
    "PWBS_eudaimonic_wellbeing_mean ~~ K6_psychological_distress_total",
    "indirect_life_satisfaction := a*b*c1",
    "indirect_vitality := a*b*c2",
    "indirect_eudaimonic := a*b*c3",
    "indirect_distress := a*b*c4"
  )
  model_syntax <- paste(model_lines, collapse = "\n")
  fit <- lavaan::sem(
    model_syntax, data = model_data, estimator = "MLR",
    meanstructure = TRUE, missing = "fiml", fixed.x = FALSE
  )
  fit_rows[[version]] <- lavaan_fit_row(fit, "emotion_specific_path_model", version)
  parameters <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  selected <- parameters[parameters$op %in% c("~", ":="), ]
  selected$emotion_version <- version
  parameter_rows[[version]] <- selected[, c("emotion_version", setdiff(names(selected), "emotion_version"))]
  summary_lines <- c(
    summary_lines,
    paste0("===== ", version, " ====="),
    model_lines,
    "",
    capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)),
    ""
  )
}

write_utf8_csv(round_numeric_columns(do.call(rbind, fit_rows), 4L), file.path(output_directory, "07_emotion_specific_fit_indices.csv"))
write_utf8_csv(round_numeric_columns(do.call(rbind, parameter_rows), 4L), file.path(output_directory, "07_emotion_specific_parameters.csv"))
write_utf8_text(summary_lines, file.path(output_directory, "07_emotion_specific_full_summaries.txt"))
write_analysis_session_info(
  file.path(output_directory, "07_session_info.txt"),
  c(
    "These are supplemental observed-score path models for the seven emotion-specific ECS versions.",
    "They replace the general awareness/action scores one version at a time, following the original sensitivity-analysis logic.",
    "They are not substitutes for the primary latent CFA and SEM models."
  )
)

message("Emotion-specific sensitivity models complete: seven versions.")

