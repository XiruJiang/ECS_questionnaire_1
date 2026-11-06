# Reliability analysis: Cronbach alpha, McDonald omega, and item diagnostics.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")
require_packages("psych")

output_directory <- ensure_directory(file.path(results_directory, "02_reliability"))
data <- load_analysis_data()
item_sets <- reliability_item_sets()
all_items <- unique(unlist(item_sets, use.names = FALSE))
missing_items <- setdiff(all_items, names(data))
assert_condition(length(missing_items) == 0L, sprintf("Missing reliability items: %s", paste(missing_items, collapse = ", ")))

reliability_rows <- list()
item_rows <- list()

for (scale_name in names(item_sets)) {
  items <- item_sets[[scale_name]]
  item_data <- as.data.frame(lapply(data[items], numeric_column, variable_name = scale_name), check.names = FALSE)
  complete_n <- sum(stats::complete.cases(item_data))

  alpha_result <- suppressWarnings(suppressMessages(
    psych::alpha(item_data, check.keys = FALSE, warnings = FALSE, delete = FALSE)
  ))

  omega_total <- NA_real_
  omega_note <- ""
  if (length(items) >= 3L) {
    omega_result <- tryCatch(
      suppressWarnings(suppressMessages(
        psych::omega(item_data, nfactors = 1, plot = FALSE, flip = FALSE, warnings = FALSE)
      )),
      error = function(error) error
    )
    if (inherits(omega_result, "error")) {
      omega_note <- conditionMessage(omega_result)
    } else {
      omega_total <- unname(omega_result$omega.tot)
    }
  } else {
    omega_note <- "Omega not estimated for a two-item subscale."
  }

  reliability_rows[[scale_name]] <- data.frame(
    scale = scale_name,
    item_count = length(items),
    n_complete = complete_n,
    cronbach_alpha = unname(alpha_result$total$raw_alpha),
    standardized_alpha = unname(alpha_result$total$std.alpha),
    mcdonald_omega_total = omega_total,
    average_interitem_correlation = unname(alpha_result$total$average_r),
    note = omega_note,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  stats <- alpha_result$item.stats
  item_rows[[scale_name]] <- data.frame(
    scale = scale_name,
    item = row.names(stats),
    mean = stats$mean,
    sd = stats$sd,
    raw_item_total_r = stats$raw.r,
    standardized_item_total_r = stats$std.r,
    corrected_item_total_r = stats$r.drop,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

reliability_summary <- do.call(rbind, reliability_rows)
row.names(reliability_summary) <- NULL
reliability_summary <- round_numeric_columns(reliability_summary, digits = 3L)

item_diagnostics <- do.call(rbind, item_rows)
row.names(item_diagnostics) <- NULL
item_diagnostics <- round_numeric_columns(item_diagnostics, digits = 3L)

write_utf8_csv(reliability_summary, file.path(output_directory, "02_reliability_summary.csv"))
write_utf8_csv(item_diagnostics, file.path(output_directory, "02_item_diagnostics.csv"))

write_analysis_session_info(
  file.path(output_directory, "02_session_info.txt"),
  c(
    sprintf("Participants: %d", nrow(data)),
    sprintf("Reliability scales: %d", length(item_sets)),
    "McDonald omega is psych::omega total omega with one general factor.",
    "Two-item subscales report alpha but not omega."
  )
)

message(sprintf("Reliability analysis complete: %d scales/subscales", length(item_sets)))

