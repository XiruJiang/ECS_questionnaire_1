# Pearson correlations for ECS validity and psychological outcomes.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")

output_directory <- ensure_directory(file.path(results_directory, "03_correlations"))
data <- load_analysis_data()
variables <- key_correlation_variables
missing_variables <- setdiff(variables, names(data))
assert_condition(length(missing_variables) == 0L, sprintf("Missing correlation variables: %s", paste(missing_variables, collapse = ", ")))

numeric_data <- as.data.frame(
  lapply(variables, function(variable_name) numeric_column(data[[variable_name]], variable_name)),
  check.names = FALSE
)
names(numeric_data) <- variables

variable_count <- length(variables)
r_matrix <- matrix(NA_real_, variable_count, variable_count, dimnames = list(variables, variables))
p_matrix <- matrix(NA_real_, variable_count, variable_count, dimnames = list(variables, variables))
n_matrix <- matrix(NA_integer_, variable_count, variable_count, dimnames = list(variables, variables))
long_rows <- list()
row_index <- 0L

for (i in seq_len(variable_count)) {
  for (j in i:variable_count) {
    x <- numeric_data[[i]]
    y <- numeric_data[[j]]
    complete <- stats::complete.cases(x, y)
    n_complete <- sum(complete)
    if (i == j) {
      r_value <- 1
      p_value <- 0
    } else {
      test <- stats::cor.test(x[complete], y[complete], method = "pearson")
      r_value <- unname(test$estimate)
      p_value <- test$p.value
    }
    r_matrix[i, j] <- r_matrix[j, i] <- r_value
    p_matrix[i, j] <- p_matrix[j, i] <- p_value
    n_matrix[i, j] <- n_matrix[j, i] <- n_complete

    if (i < j) {
      row_index <- row_index + 1L
      long_rows[[row_index]] <- data.frame(
        variable_1 = variables[[i]],
        variable_2 = variables[[j]],
        n = n_complete,
        r = r_value,
        p_value = p_value,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
}

correlation_long <- do.call(rbind, long_rows)
correlation_long$p_fdr <- stats::p.adjust(correlation_long$p_value, method = "BH")
correlation_long$absolute_r <- abs(correlation_long$r)
correlation_long$effect_size <- cut(
  correlation_long$absolute_r,
  breaks = c(-Inf, 0.10, 0.30, 0.50, Inf),
  labels = c("negligible", "small", "medium", "large"),
  right = FALSE
)

matrix_to_data_frame <- function(matrix_value, row_label) {
  result <- data.frame(variable = row.names(matrix_value), matrix_value, check.names = FALSE)
  names(result)[[1]] <- row_label
  row.names(result) <- NULL
  result
}

write_utf8_csv(
  round_numeric_columns(matrix_to_data_frame(r_matrix, "variable"), digits = 3L),
  file.path(output_directory, "03_correlation_coefficients.csv")
)
write_utf8_csv(
  round_numeric_columns(matrix_to_data_frame(p_matrix, "variable"), digits = 4L),
  file.path(output_directory, "03_correlation_p_values.csv")
)
write_utf8_csv(
  matrix_to_data_frame(n_matrix, "variable"),
  file.path(output_directory, "03_pairwise_sample_sizes.csv")
)
write_utf8_csv(
  round_numeric_columns(correlation_long, digits = 4L),
  file.path(output_directory, "03_correlation_long.csv")
)

write_analysis_session_info(
  file.path(output_directory, "03_session_info.txt"),
  c(
    sprintf("Participants: %d", nrow(data)),
    sprintf("Variables: %d", length(variables)),
    "Pearson correlations; pairwise complete observations.",
    "Both unadjusted p values and Benjamini-Hochberg FDR values are provided."
  )
)

message(sprintf("Correlation analysis complete: %d variables", length(variables)))
