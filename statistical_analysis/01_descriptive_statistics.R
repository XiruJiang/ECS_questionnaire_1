# Descriptive statistics and sample-flow tables.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")

output_directory <- ensure_directory(file.path(results_directory, "01_descriptive"))
data <- load_analysis_data()
scores <- score_variables()
missing_scores <- setdiff(scores, names(data))
assert_condition(length(missing_scores) == 0L, sprintf("Missing score columns: %s", paste(missing_scores, collapse = ", ")))

describe_vector <- function(values, variable_name) {
  numeric_values <- numeric_column(values, variable_name)
  present <- numeric_values[!is.na(numeric_values)]
  n <- length(present)
  mean_value <- mean(present)
  sd_value <- stats::sd(present)
  centered <- present - mean_value
  skewness <- if (n >= 3L && sd_value > 0) mean(centered^3) / sd_value^3 else NA_real_
  kurtosis_excess <- if (n >= 4L && sd_value > 0) mean(centered^4) / sd_value^4 - 3 else NA_real_
  data.frame(
    variable = variable_name,
    n = n,
    missing = length(numeric_values) - n,
    mean = mean_value,
    sd = sd_value,
    median = stats::median(present),
    minimum = min(present),
    maximum = max(present),
    skewness = skewness,
    excess_kurtosis = kurtosis_excess,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

score_descriptives <- do.call(
  rbind,
  lapply(scores, function(variable_name) describe_vector(data[[variable_name]], variable_name))
)
score_descriptives <- round_numeric_columns(score_descriptives, digits = 2L)
write_utf8_csv(score_descriptives, file.path(output_directory, "01_scale_score_descriptives.csv"))

age_descriptives <- describe_vector(data$age, "age")
age_descriptives <- round_numeric_columns(age_descriptives, digits = 2L)
write_utf8_csv(age_descriptives, file.path(output_directory, "01_age_descriptives.csv"))

demographic_levels <- list(
  gender = c(
    "\u7537\u6027", "\u5973\u6027", "\u305D\u306E\u4ED6", "\u56DE\u7B54\u3057\u306A\u3044"
  ),
  education = c(
    "\u4E2D\u5B66\u30FB\u9AD8\u6821", "\u5C02\u4FEE\u30FB\u9AD8\u5C02\u30FB\u77ED\u5927",
    "\u56DB\u5E74\u5236\u5927\u5B66", "\u5927\u5B66\u9662", "\u305D\u306E\u4ED6",
    "\u56DE\u7B54\u3057\u306A\u3044"
  ),
  marital_status = c(
    "\u65E2\u5A5A\uFF0F\u540C\u5C45\u4E2D", "\u72EC\u8EAB",
    "\u4EA4\u969B\u76F8\u624B\u304C\u3044\u308B", "\u5225\u5C45\uFF0F\u96E2\u5A5A",
    "\u6B7B\u5225", "\u56DE\u7B54\u3057\u306A\u3044"
  ),
  income = c(
    "100\u4E07\u5186\u672A\u6E80", "100\uFF5E200\u4E07\u5186\u672A\u6E80",
    "200\uFF5E300\u4E07\u5186\u672A\u6E80", "300\uFF5E400\u4E07\u5186\u672A\u6E80",
    "400\uFF5E500\u4E07\u5186\u672A\u6E80", "500\uFF5E600\u4E07\u5186\u672A\u6E80",
    "600\uFF5E700\u4E07\u5186\u672A\u6E80", "700\uFF5E800\u4E07\u5186\u672A\u6E80",
    "800\uFF5E900\u4E07\u5186\u672A\u6E80", "900\uFF5E1000\u4E07\u5186\u672A\u6E80",
    "1000\u4E07\u5186\u4EE5\u4E0A", "\u56DE\u7B54\u3057\u306A\u3044"
  )
)
frequency_rows <- list()
for (variable_name in names(demographic_levels)) {
  values <- as.character(data[[variable_name]])
  expected_levels <- demographic_levels[[variable_name]]
  unexpected_levels <- setdiff(unique(values[!is.na(values)]), expected_levels)
  assert_condition(
    length(unexpected_levels) == 0L,
    sprintf(
      "Unexpected categories in %s: %s",
      variable_name,
      paste(unexpected_levels, collapse = ", ")
    )
  )
  assert_condition(
    !anyNA(values),
    sprintf("Missing raw labels in required demographic variable: %s", variable_name)
  )
  counts <- table(factor(values, levels = expected_levels), useNA = "no")
  frequency_rows[[variable_name]] <- data.frame(
    variable = variable_name,
    category = expected_levels,
    n = as.integer(counts),
    percent = round(100 * as.integer(counts) / length(values), 1),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
demographic_frequencies <- do.call(rbind, frequency_rows)
row.names(demographic_frequencies) <- NULL
write_utf8_csv(demographic_frequencies, file.path(output_directory, "01_demographic_frequencies.csv"))

merged_file <- file.path(preprocessing_directory, "01_merged_raw_data.csv")
exclusion_file <- file.path(preprocessing_directory, "02_preprocessing_exclusions.csv")
merged_n <- nrow(read_utf8_csv(merged_file, all_character = TRUE))
excluded_n <- nrow(read_utf8_csv(exclusion_file, all_character = TRUE))
sample_flow <- data.frame(
  stage = c("merged_responses", "excluded_attention_check", "analysis_sample"),
  n = c(merged_n, excluded_n, nrow(data)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
assert_condition(merged_n - excluded_n == nrow(data), "Sample-flow counts do not reconcile")
write_utf8_csv(sample_flow, file.path(output_directory, "01_sample_flow.csv"))

write_analysis_session_info(
  file.path(output_directory, "01_session_info.txt"),
  c(sprintf("Participants: %d", nrow(data)), sprintf("Score variables: %d", length(scores)))
)

message(sprintf("Descriptive statistics complete: %d participants, %d score variables", nrow(data), length(scores)))

