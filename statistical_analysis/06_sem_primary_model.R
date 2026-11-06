# Latent SEM following the original awareness -> action -> positive affect model.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")
require_packages("lavaan")

output_directory <- ensure_directory(file.path(results_directory, "06_sem_primary"))
data <- load_analysis_data()
background <- prepare_background_variables(data)
analysis_data <- cbind(data, background[, setdiff(names(background), "participant_id"), drop = FALSE])

make_balanced_parcels <- function(data, items, parcel_count, prefix) {
  item_data <- as.data.frame(lapply(items, function(item) numeric_column(data[[item]], item)), check.names = FALSE)
  names(item_data) <- items
  one_factor_model <- paste0("parcel_factor =~ ", paste(items, collapse = " + "))
  loading_fit <- lavaan::cfa(one_factor_model, data = item_data, estimator = "MLR", missing = "fiml")
  parameters <- lavaan::parameterEstimates(loading_fit, standardized = TRUE)
  loadings <- parameters[parameters$op == "=~", c("rhs", "std.all")]
  loading_values <- stats::setNames(loadings$std.all, loadings$rhs)
  ordered_items <- names(sort(loading_values[items], decreasing = TRUE))
  snake <- rep(c(seq_len(parcel_count), rev(seq_len(parcel_count))), length.out = length(items))
  assignment <- stats::setNames(snake, ordered_items)

  parcel_data <- data.frame(row.names = seq_len(nrow(data)))
  assignment_rows <- list()
  for (parcel_index in seq_len(parcel_count)) {
    parcel_items <- names(assignment)[assignment == parcel_index]
    parcel_name <- paste0(prefix, "_parcel_", parcel_index)
    parcel_data[[parcel_name]] <- rowMeans(item_data[parcel_items], na.rm = TRUE)
    parcel_data[[parcel_name]][rowSums(!is.na(item_data[parcel_items])) != length(parcel_items)] <- NA_real_
    assignment_rows[[parcel_index]] <- data.frame(
      construct = prefix,
      parcel = parcel_name,
      item = parcel_items,
      standardized_loading_used_for_balance = unname(loading_values[parcel_items]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  list(data = parcel_data, assignment = do.call(rbind, assignment_rows))
}

pa_parcels <- make_balanced_parcels(analysis_data, make_item_names("D", 1:10), 5L, "PA")
eud_parcels <- make_balanced_parcels(analysis_data, c("G_01_rev", "G_02", "G_03", "G_04_rev", "G_05", "G_06_rev"), 3L, "EUD")
erq_parcels <- make_balanced_parcels(analysis_data, make_item_names("B", 1:6), 3L, "ERQ")
eac_parcels <- make_balanced_parcels(analysis_data, make_item_names("C", 1:8), 3L, "EAC")

analysis_data <- cbind(analysis_data, pa_parcels$data, eud_parcels$data, erq_parcels$data, eac_parcels$data)
parcel_assignments <- rbind(pa_parcels$assignment, eud_parcels$assignment, erq_parcels$assignment, eac_parcels$assignment)
write_utf8_csv(round_numeric_columns(parcel_assignments, 4L), file.path(output_directory, "06_parcel_assignments.csv"))

covariates <- active_covariates(background)
covariate_rhs <- paste(covariates, collapse = " + ")

measurement_model <- c(
  "awareness =~ I_02 + I_05 + I_08",
  "action =~ I_01 + I_03 + I_04 + I_06 + I_07 + I_09 + I_10",
  "positive_affect =~ PA_parcel_1 + PA_parcel_2 + PA_parcel_3 + PA_parcel_4 + PA_parcel_5",
  "life_satisfaction =~ E_01 + E_02 + E_03",
  "vitality =~ F_01 + F_02 + F_03",
  "eudaimonic =~ EUD_parcel_1 + EUD_parcel_2 + EUD_parcel_3",
  "distress =~ Q_01 + Q_02 + Q_03 + Q_04 + Q_05 + Q_06",
  "I_05 ~~ I_08"
)

outcome_covariances <- c(
  "life_satisfaction ~~ vitality + eudaimonic + distress",
  "vitality ~~ eudaimonic + distress",
  "eudaimonic ~~ distress"
)

model1_lines <- c(
  measurement_model,
  paste0("awareness ~ ", covariate_rhs),
  paste0("action ~ a*awareness + ", covariate_rhs),
  paste0("positive_affect ~ b*action + ", covariate_rhs),
  paste0("life_satisfaction ~ c1*positive_affect + ", covariate_rhs),
  paste0("vitality ~ c2*positive_affect + ", covariate_rhs),
  paste0("eudaimonic ~ c3*positive_affect + ", covariate_rhs),
  paste0("distress ~ c4*positive_affect + ", covariate_rhs),
  outcome_covariances,
  "indirect_life_satisfaction := a*b*c1",
  "indirect_vitality := a*b*c2",
  "indirect_eudaimonic := a*b*c3",
  "indirect_distress := a*b*c4"
)

control_measurement <- c(
  "adaptive_er =~ CERQ_putting_into_perspective_mean + CERQ_acceptance_mean + CERQ_positive_refocusing_mean + CERQ_refocus_on_planning_mean + CERQ_positive_reappraisal_mean",
  "maladaptive_er =~ CERQ_catastrophizing_mean + CERQ_other_blame_mean + CERQ_self_blame_mean + CERQ_rumination_mean",
  "reappraisal =~ ERQ_parcel_1 + ERQ_parcel_2 + ERQ_parcel_3",
  "emotional_processing =~ EAC_parcel_1 + EAC_parcel_2 + EAC_parcel_3"
)

model2_lines <- c(
  measurement_model,
  control_measurement,
  paste0("awareness ~ ", covariate_rhs),
  paste0("action ~ a*awareness + ", covariate_rhs),
  paste0("positive_affect ~ b*action + er_adaptive*adaptive_er + er_maladaptive*maladaptive_er + er_reappraisal*reappraisal + er_processing*emotional_processing + ", covariate_rhs),
  paste0("life_satisfaction ~ c1*positive_affect + ", covariate_rhs),
  paste0("vitality ~ c2*positive_affect + ", covariate_rhs),
  paste0("eudaimonic ~ c3*positive_affect + direct_processing*emotional_processing + ", covariate_rhs),
  paste0("distress ~ c4*positive_affect + direct_maladaptive*maladaptive_er + ", covariate_rhs),
  outcome_covariances,
  "indirect_life_satisfaction := a*b*c1",
  "indirect_vitality := a*b*c2",
  "indirect_eudaimonic := a*b*c3",
  "indirect_distress := a*b*c4",
  "adaptive_via_pa_life := er_adaptive*c1",
  "adaptive_via_pa_vitality := er_adaptive*c2",
  "adaptive_via_pa_eudaimonic := er_adaptive*c3",
  "adaptive_via_pa_distress := er_adaptive*c4",
  "maladaptive_via_pa_life := er_maladaptive*c1",
  "maladaptive_via_pa_vitality := er_maladaptive*c2",
  "maladaptive_via_pa_eudaimonic := er_maladaptive*c3",
  "maladaptive_via_pa_distress := er_maladaptive*c4"
)

model1_syntax <- paste(model1_lines, collapse = "\n")
model2_syntax <- paste(model2_lines, collapse = "\n")
write_utf8_text(model1_lines, file.path(output_directory, "06_model1_syntax.txt"))
write_utf8_text(model2_lines, file.path(output_directory, "06_model2_syntax.txt"))

all_model_variables <- unique(c(
  unlist(regmatches(paste(model2_lines, collapse = " "), gregexpr("[A-Za-z][A-Za-z0-9_]*", paste(model2_lines, collapse = " ")))),
  covariates
))
reserved_words <- c("awareness", "action", "positive_affect", "life_satisfaction", "vitality", "eudaimonic", "distress", "adaptive_er", "maladaptive_er", "reappraisal", "emotional_processing")
observed_candidates <- intersect(all_model_variables, names(analysis_data))
complete_variables <- unique(c(observed_candidates, covariates))
complete_rows <- stats::complete.cases(analysis_data[complete_variables])
sem_data <- analysis_data[complete_rows, , drop = FALSE]
exclusion_log <- data.frame(
  participant_id = analysis_data$participant_id[!complete_rows],
  exclusion_reason = "missing_or_ineligible_background_value_for_sem",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_utf8_csv(exclusion_log, file.path(output_directory, "06_sem_exclusions.csv"))

fit_sem_model <- function(model_syntax, model_name) {
  fit <- lavaan::sem(
    model_syntax, data = sem_data, estimator = "MLR", meanstructure = TRUE,
    missing = "fiml", fixed.x = FALSE
  )
  saveRDS(fit, file.path(output_directory, paste0("06_", model_name, "_fit.rds")))
  parameters <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  selected <- parameters[parameters$op %in% c("=~", "~", ":="), ]
  write_utf8_csv(
    round_numeric_columns(selected, 4L),
    file.path(output_directory, paste0("06_", model_name, "_parameters.csv"))
  )
  write_utf8_text(
    capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)),
    file.path(output_directory, paste0("06_", model_name, "_full_summary.txt"))
  )
  list(fit = fit, fit_row = lavaan_fit_row(fit, model_name, "primary_sem"))
}

model1 <- fit_sem_model(model1_syntax, "model1_sequential")
model2 <- fit_sem_model(model2_syntax, "model2_er_controls")
fit_results <- rbind(model1$fit_row, model2$fit_row)
write_utf8_csv(round_numeric_columns(fit_results, 4L), file.path(output_directory, "06_sem_fit_indices.csv"))

bootstrap_draws <- suppressWarnings(as.integer(Sys.getenv("SEM_BOOTSTRAP_DRAWS", "1000")))
if (is.na(bootstrap_draws) || bootstrap_draws < 0L) bootstrap_draws <- 1000L
bootstrap_lines <- c(sprintf("Bootstrap draws requested: %d", bootstrap_draws))

# Avoid leaving bootstrap confidence intervals from an earlier run when a
# later procedural check deliberately sets the draw count to zero.
bootstrap_output_paths <- file.path(
  output_directory,
  c(
    "06_model1_sequential_bootstrap_indirects.csv",
    "06_model2_er_controls_bootstrap_indirects.csv"
  )
)
unlink(bootstrap_output_paths[file.exists(bootstrap_output_paths)], force = TRUE)

if (bootstrap_draws > 0L) {
  bootstrap_models <- list(
    model1_sequential = model1_syntax,
    model2_er_controls = model2_syntax
  )
  for (model_name in names(bootstrap_models)) {
    syntax <- bootstrap_models[[model_name]]
    bootstrap_fit <- tryCatch(
      lavaan::sem(
        syntax, data = sem_data, estimator = "ML", meanstructure = TRUE,
        missing = "fiml", fixed.x = FALSE, se = "bootstrap",
        bootstrap = bootstrap_draws
      ),
      error = function(error) error
    )
    if (inherits(bootstrap_fit, "error")) {
      bootstrap_lines <- c(bootstrap_lines, sprintf("%s bootstrap failed: %s", model_name, conditionMessage(bootstrap_fit)))
    } else {
      bootstrap_parameters <- lavaan::parameterEstimates(
        bootstrap_fit, standardized = TRUE, ci = TRUE,
        boot.ci.type = "perc"
      )
      indirect <- bootstrap_parameters[bootstrap_parameters$op == ":=", ]
      write_utf8_csv(
        round_numeric_columns(indirect, 4L),
        file.path(output_directory, paste0("06_", model_name, "_bootstrap_indirects.csv"))
      )
      bootstrap_lines <- c(bootstrap_lines, sprintf("%s bootstrap completed", model_name))
    }
  }
}

write_utf8_text(bootstrap_lines, file.path(output_directory, "06_bootstrap_status.txt"))
write_analysis_session_info(
  file.path(output_directory, "06_session_info.txt"),
  c(
    sprintf("SEM complete cases: %d of %d", nrow(sem_data), nrow(analysis_data)),
    sprintf("Bootstrap draws: %d", bootstrap_draws),
    "Primary chain: ECS awareness -> ECS action -> positive affect -> four outcomes.",
    "Model 2 adds adaptive/maladaptive CERQ, ERQ reappraisal, and EAC emotional processing.",
    "EAC emotional processing substitutes for the original paper's integrative ER measure.",
    "K6 substitutes for the original paper's HSCL-25 internalizing-symptoms measure.",
    "Location is unavailable; age, binary gender, education, income, and marital-status dummies are controlled."
  )
)

message(sprintf("Primary SEM complete: n=%d; bootstrap draws=%d", nrow(sem_data), bootstrap_draws))

