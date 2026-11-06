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

demographic_variables <- c("gender", "education", "marital_status", "income")
frequency_rows <- list()
for (variable_name in demographic_variables) {
  values <- as.character(data[[variable_name]])
  counts <- table(values, useNA = "ifany")
  frequency_rows[[variable_name]] <- data.frame(
    variable = variable_name,
    category = names(counts),
    n = as.integer(counts),
    percent = round(100 * as.integer(counts) / length(values), 1),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
demographic_frequencies <- do.call(rbind, frequency_rows)
row.names(demographic_frequencies) <- NULL
write_utf8_csv(demographic_frequencies, file.path(output_directory, "01_demographic_frequencies.csv"))

merged_file <- file.path(preprocessing_directory, "merged_raw_data.csv")
exclusion_file <- file.path(preprocessing_directory, "preprocessing_exclusions.csv")
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

