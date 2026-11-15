# Exploratory factor analysis of the 10-item general Emotion Crafting Scale.
#
# The original paper fitted robust maximum-likelihood EFA models requesting
# one to four factors. Its substantive comparison focused on the one- and
# two-factor solutions; the three- and four-factor solutions were not
# identified. The current questionnaire already uses the final 10-item ECS,
# after the two cross-loading items from the original 12-item pool were
# removed. Therefore, this script explores the structure of these 10 items.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")
require_packages(c("lavaan", "psych"))

output_directory <- ensure_directory(file.path(results_directory, "05_efa_ecs"))
data <- load_analysis_data()

items <- make_item_names("I", 1:10)
awareness_items <- make_item_names("I", c(2, 5, 8))
action_items <- make_item_names("I", c(1, 3, 4, 6, 7, 9, 10))
missing_items <- setdiff(items, names(data))
assert_condition(
  length(missing_items) == 0L,
  sprintf("Missing general ECS items: %s", paste(missing_items, collapse = ", "))
)

analysis_data <- as.data.frame(
  lapply(items, function(item) numeric_column(data[[item]], item)),
  check.names = FALSE
)
names(analysis_data) <- items

assert_condition(
  all(vapply(analysis_data, function(values) all(is.na(values) | (values >= 1 & values <= 5)), logical(1))),
  "General ECS items contain values outside 1-5"
)

complete_rows <- stats::complete.cases(analysis_data)
assert_condition(sum(complete_rows) >= 100L, "Fewer than 100 complete ECS cases are available for EFA")
correlation_matrix <- stats::cor(analysis_data, use = "pairwise.complete.obs")
assert_condition(all(is.finite(correlation_matrix)), "ECS correlation matrix contains non-finite values")

# Supplemental factorability checks. These were not reported in the original
# paper, so they are diagnostics rather than factor-retention rules.
kmo_result <- psych::KMO(correlation_matrix)
bartlett_result <- psych::cortest.bartlett(
  correlation_matrix,
  n = sum(complete_rows)
)

kmo_table <- data.frame(
  item = c("overall", names(kmo_result$MSAi)),
  msa = c(unname(kmo_result$MSA), unname(kmo_result$MSAi)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
factorability_table <- data.frame(
  test = c("KMO_overall", "Bartlett_chisq", "Bartlett_df", "Bartlett_p_value"),
  value = c(
    unname(kmo_result$MSA),
    unname(bartlett_result$chisq),
    unname(bartlett_result$df),
    unname(bartlett_result$p.value)
  ),
  note = c(
    "Supplemental diagnostic; not reported in the original paper.",
    "Supplemental diagnostic; H0 is an identity correlation matrix.",
    "Degrees of freedom for Bartlett test.",
    "Supplemental diagnostic; not a factor-retention rule."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

failed_fit_row <- function(number_of_factors, error_message) {
  data.frame(
    scale = "general",
    model = paste0(number_of_factors, "_factor"),
    n = sum(complete_rows),
    converged = FALSE,
    chisq = NA_real_,
    df = NA_real_,
    chisq_df = NA_real_,
    p_value = NA_real_,
    cfi = NA_real_,
    tli = NA_real_,
    rmsea = NA_real_,
    rmsea_ci_lower = NA_real_,
    rmsea_ci_upper = NA_real_,
    srmr = NA_real_,
    aic = NA_real_,
    bic = NA_real_,
    requested_factors = number_of_factors,
    original_paper_role = if (number_of_factors <= 2L) "primary_comparison" else "attempted_but_not_identified_in_original",
    fit_interpretation = "not_available",
    error = error_message,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

interpret_fit <- function(cfi, tli, rmsea, srmr) {
  if (any(is.na(c(cfi, tli, rmsea, srmr)))) return("not_available")
  if (cfi >= 0.95 && tli >= 0.95 && rmsea <= 0.05 && srmr <= 0.05) {
    return("good_by_original_paper_thresholds")
  }
  if (cfi >= 0.90 && tli >= 0.90 && rmsea <= 0.08 && srmr <= 0.10) {
    return("acceptable_by_original_paper_thresholds")
  }
  "below_original_paper_thresholds"
}

fit_objects <- list()
fit_rows <- list()
loading_rows <- list()
factor_correlation_rows <- list()
lavaan_summary_method <- methods::selectMethod("summary", "lavaan")
summary_lines <- character(0)
diagnostic_lines <- c(
  "Primary EFA follows the original paper: robust maximum likelihood, 1-4 requested factors.",
  "The original paper did not state its rotation method. Geomin oblique rotation is used here to mimic standard Mplus EFA practice.",
  "The current questionnaire contains the final 10 ECS items, not the original pre-deletion 12-item pool.",
  "KMO and Bartlett tests are supplemental diagnostics and were not reported in the original paper.",
  ""
)

for (number_of_factors in 1:4) {
  fit <- tryCatch(
    suppressWarnings(
      lavaan::efa(
        data = analysis_data,
        ov.names = items,
        nfactors = number_of_factors,
        rotation = list(
          "geomin",
          geomin_epsilon = 0.01,
          rstarts = 50,
          row_weights = "none"
        ),
        estimator = "MLR",
        missing = "fiml",
        meanstructure = TRUE,
        information = "observed",
        mimic = "Mplus",
        output = "lavaan"
      )
    ),
    error = function(error) error
  )

  model_name <- paste0(number_of_factors, "_factor")
  if (inherits(fit, "error")) {
    fit_rows[[model_name]] <- failed_fit_row(number_of_factors, conditionMessage(fit))
    diagnostic_lines <- c(
      diagnostic_lines,
      sprintf("%s: estimation error: %s", model_name, conditionMessage(fit))
    )
    next
  }

  fit_objects[[model_name]] <- fit
  fit_row <- lavaan_fit_row(fit, model_name, "general")
  fit_row$requested_factors <- number_of_factors
  fit_row$original_paper_role <- if (number_of_factors <= 2L) {
    "primary_comparison"
  } else {
    "attempted_but_not_identified_in_original"
  }
  fit_row$fit_interpretation <- interpret_fit(
    fit_row$cfi, fit_row$tli, fit_row$rmsea, fit_row$srmr
  )
  fit_row$error <- ""
  fit_rows[[model_name]] <- fit_row

  standardized <- lavaan::standardizedSolution(fit)
  loadings <- standardized[
    standardized$op == "=~",
    c("lhs", "rhs", "est.std", "se", "z", "pvalue", "ci.lower", "ci.upper"),
    drop = FALSE
  ]
  names(loadings) <- c(
    "factor", "item", "standardized_loading", "se", "z", "p_value",
    "ci_lower", "ci_upper"
  )

  loading_matrix <- matrix(
    0,
    nrow = length(items),
    ncol = number_of_factors,
    dimnames = list(items, unique(loadings$factor))
  )
  for (row_index in seq_len(nrow(loadings))) {
    loading_matrix[loadings$item[[row_index]], loadings$factor[[row_index]]] <-
      loadings$standardized_loading[[row_index]]
  }
  factor_correlations <- tryCatch(
    lavaan::lavInspect(fit, "cor.lv"),
    error = function(error) diag(number_of_factors)
  )
  factor_correlations <- as.matrix(factor_correlations)
  communalities <- diag(loading_matrix %*% factor_correlations %*% t(loading_matrix))
  loadings$communality <- unname(communalities[loadings$item])
  loadings$requested_factors <- number_of_factors
  loading_rows[[model_name]] <- loadings[, c(
    "requested_factors", "factor", "item", "standardized_loading",
    "se", "z", "p_value", "ci_lower", "ci_upper", "communality"
  )]

  if (number_of_factors >= 2L) {
    factor_names <- colnames(factor_correlations)
    factor_pairs <- utils::combn(factor_names, 2L)
    factor_correlation_rows[[model_name]] <- data.frame(
      requested_factors = number_of_factors,
      factor_1 = factor_pairs[1, ],
      factor_2 = factor_pairs[2, ],
      correlation = vapply(
        seq_len(ncol(factor_pairs)),
        function(index) factor_correlations[
          factor_pairs[1, index], factor_pairs[2, index]
        ],
        numeric(1)
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  convergence <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(error) FALSE)
  post_check <- tryCatch(lavaan::lavInspect(fit, "post.check"), error = function(error) NA)
  diagnostic_lines <- c(
    diagnostic_lines,
    sprintf(
      "%s: converged=%s; admissible_post_check=%s; df=%s",
      model_name,
      convergence,
      post_check,
      format(fit_row$df)
    )
  )
  summary_lines <- c(
    summary_lines,
    paste0("===== General ECS: ", number_of_factors, "-factor EFA ====="),
    capture.output(
      print(
        lavaan_summary_method(
          fit,
          fit.measures = TRUE,
          standardized = TRUE
        )
      )
    ),
    ""
  )
}

fit_indices <- do.call(rbind, fit_rows)
row.names(fit_indices) <- NULL

all_loadings <- if (length(loading_rows)) {
  result <- do.call(rbind, loading_rows)
  row.names(result) <- NULL
  result
} else {
  data.frame()
}

all_factor_correlations <- if (length(factor_correlation_rows)) {
  result <- do.call(rbind, factor_correlation_rows)
  row.names(result) <- NULL
  result
} else {
  data.frame(
    requested_factors = integer(0), factor_1 = character(0),
    factor_2 = character(0), correlation = numeric(0)
  )
}

comparison_table <- data.frame(
  comparison = "one_factor_vs_two_factor",
  note = "Robust scaled chi-square difference test; original paper reported this comparison.",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
if (all(c("1_factor", "2_factor") %in% names(fit_objects))) {
  comparison <- tryCatch(
    suppressWarnings(
      lavaan::lavTestLRT(
        fit_objects[["1_factor"]],
        fit_objects[["2_factor"]],
        method = "satorra.bentler.2001"
      )
    ),
    error = function(error) error
  )
  if (inherits(comparison, "error")) {
    comparison_table$error <- conditionMessage(comparison)
  } else {
    comparison_data <- data.frame(model = row.names(comparison), comparison, check.names = FALSE)
    row.names(comparison_data) <- NULL
    comparison_table <- cbind(
      comparison = rep("one_factor_vs_two_factor", nrow(comparison_data)),
      comparison_data,
      note = rep(
        "Robust scaled chi-square difference test; original paper reported this comparison.",
        nrow(comparison_data)
      ),
      stringsAsFactors = FALSE
    )
  }
} else {
  comparison_table$error <- "One-factor or two-factor model was unavailable."
}

two_factor_item_summary <- data.frame()
if ("2_factor" %in% names(fit_objects)) {
  two_factor_loadings <- all_loadings[all_loadings$requested_factors == 2L, ]
  factor_names <- unique(two_factor_loadings$factor)
  loading_matrix <- matrix(
    NA_real_, nrow = length(items), ncol = 2L,
    dimnames = list(items, factor_names)
  )
  for (row_index in seq_len(nrow(two_factor_loadings))) {
    loading_matrix[
      two_factor_loadings$item[[row_index]],
      two_factor_loadings$factor[[row_index]]
    ] <- two_factor_loadings$standardized_loading[[row_index]]
  }

  direct_score <- mean(abs(loading_matrix[awareness_items, 1])) +
    mean(abs(loading_matrix[action_items, 2]))
  swapped_score <- mean(abs(loading_matrix[awareness_items, 2])) +
    mean(abs(loading_matrix[action_items, 1]))
  awareness_factor_index <- if (direct_score >= swapped_score) 1L else 2L
  action_factor_index <- if (awareness_factor_index == 1L) 2L else 1L

  awareness_sign <- sign(mean(loading_matrix[awareness_items, awareness_factor_index]))
  action_sign <- sign(mean(loading_matrix[action_items, action_factor_index]))
  if (awareness_sign == 0) awareness_sign <- 1
  if (action_sign == 0) action_sign <- 1

  awareness_loading <- loading_matrix[, awareness_factor_index] * awareness_sign
  action_loading <- loading_matrix[, action_factor_index] * action_sign
  expected_factor <- ifelse(items %in% awareness_items, "awareness", "action")
  primary_factor <- ifelse(
    abs(awareness_loading) >= abs(action_loading), "awareness", "action"
  )
  primary_loading <- ifelse(
    primary_factor == "awareness", awareness_loading, action_loading
  )
  secondary_loading <- ifelse(
    primary_factor == "awareness", action_loading, awareness_loading
  )
  two_factor_communalities <- vapply(
    items,
    function(item) unique(two_factor_loadings$communality[two_factor_loadings$item == item])[[1]],
    numeric(1)
  )

  two_factor_item_summary <- data.frame(
    item = items,
    expected_factor = expected_factor,
    awareness_loading = awareness_loading,
    action_loading = action_loading,
    primary_factor = primary_factor,
    primary_loading = primary_loading,
    secondary_loading = secondary_loading,
    loading_gap = abs(primary_loading) - abs(secondary_loading),
    cross_loading_abs_ge_0_30 =
      abs(awareness_loading) >= 0.30 & abs(action_loading) >= 0.30,
    matches_expected_factor = primary_factor == expected_factor,
    communality = two_factor_communalities,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

write_utf8_csv(
  round_numeric_columns(fit_indices, 4L),
  file.path(output_directory, "05_efa_fit_indices.csv")
)
write_utf8_csv(
  round_numeric_columns(all_loadings, 4L),
  file.path(output_directory, "05_efa_standardized_loadings.csv")
)
write_utf8_csv(
  round_numeric_columns(all_factor_correlations, 4L),
  file.path(output_directory, "05_efa_factor_correlations.csv")
)
write_utf8_csv(
  round_numeric_columns(comparison_table, 4L),
  file.path(output_directory, "05_efa_model_comparison.csv")
)
write_utf8_csv(
  round_numeric_columns(two_factor_item_summary, 4L),
  file.path(output_directory, "05_efa_two_factor_item_summary.csv")
)
write_utf8_csv(
  round_numeric_columns(kmo_table, 4L),
  file.path(output_directory, "05_efa_kmo_item_msa.csv")
)
write_utf8_csv(
  round_numeric_columns(factorability_table, 4L),
  file.path(output_directory, "05_efa_factorability_tests.csv")
)
write_utf8_text(diagnostic_lines, file.path(output_directory, "05_efa_diagnostics.txt"))
write_utf8_text(summary_lines, file.path(output_directory, "05_efa_full_summaries.txt"))
write_analysis_session_info(
  file.path(output_directory, "05_session_info.txt"),
  c(
    sprintf("Participants in input: %d", nrow(data)),
    sprintf("Complete ECS cases for KMO/Bartlett: %d", sum(complete_rows)),
    "Items: general ECS I_01-I_10.",
    "Estimator: MLR (maximum likelihood with robust standard errors).",
    "Requested factors: 1-4; primary comparison: 1 versus 2.",
    "Rotation: Geomin oblique, epsilon=.01, 50 random starts, row weights=none.",
    "Mplus mimic options: observed information and Mplus defaults where supported.",
    "Current instrument is the final 10-item ECS; the original EFA began with 12 items."
  )
)

message("EFA complete for the 10-item general ECS.")
