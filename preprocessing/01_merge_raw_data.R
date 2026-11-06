# Questionnaire CSV merger
#
# Default input:  project demodata/ (beside this script or one level above)
# Default output: merged_raw_data.csv
#
# Usage:
#   Rscript 01_merge_raw_data.R
#   Rscript 01_merge_raw_data.R path/to/demodata path/to/output.csv
#   Rscript 01_merge_raw_data.R --exclude-reference
#
# All CSV files are included by default. Use --exclude-reference only when
# the original reference CSV should be omitted from the merged dataset.

options(stringsAsFactors = FALSE)

get_script_directory <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command, value = TRUE)

  if (length(file_argument) == 1L) {
    script_path <- sub("^--file=", "", file_argument)
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_directory <- get_script_directory()
arguments <- commandArgs(trailingOnly = TRUE)
exclude_reference <- "--exclude-reference" %in% arguments
positional_arguments <- arguments[!grepl("^--", arguments)]

input_directory <- if (length(positional_arguments) >= 1L) {
  normalizePath(positional_arguments[[1]], winslash = "/", mustWork = TRUE)
} else {
  local_input_directory <- file.path(script_directory, "demodata")
  parent_input_directory <- file.path(dirname(script_directory), "demodata")

  if (dir.exists(local_input_directory)) {
    local_input_directory
  } else {
    parent_input_directory
  }
}

output_file <- if (length(positional_arguments) >= 2L) {
  normalizePath(positional_arguments[[2]], winslash = "/", mustWork = FALSE)
} else {
  file.path(script_directory, "merged_raw_data.csv")
}

reference_file <- "861d80fb-0691-4b23-a6bd-65346e13bb43.csv"
excluded_files <- if (exclude_reference) reference_file else character(0)

expected_item_counts <- c(
  A = 19L, B = 6L, C = 8L, D = 10L, E = 3L, F = 3L,
  G = 6L, H = 32L, I = 10L, J = 10L, K = 10L, L = 10L,
  M = 10L, N = 10L, O = 10L, P = 10L, Q = 6L
)

minimum_scores <- c(
  A = 1L, B = 1L, C = 1L, D = 1L, E = 1L, F = 1L,
  G = 1L, H = 1L, I = 1L, J = 1L, K = 1L, L = 1L,
  M = 1L, N = 1L, O = 1L, P = 1L, Q = 0L
)

maximum_scores <- c(
  A = 5L, B = 7L, C = 4L, D = 6L, E = 7L, F = 7L,
  G = 7L, H = 7L, I = 5L, J = 5L, K = 5L, L = 5L,
  M = 5L, N = 5L, O = 5L, P = 5L, Q = 4L
)

demographic_columns <- c(
  "age", "gender", "education", "marital_status", "income"
)

make_item_names <- function(scale_id, item_count) {
  sprintf("%s_%02d", scale_id, seq_len(item_count))
}

expected_items_by_scale <- Map(
  make_item_names,
  names(expected_item_counts),
  as.integer(expected_item_counts)
)
names(expected_items_by_scale) <- names(expected_item_counts)
expected_item_columns <- unlist(expected_items_by_scale, use.names = FALSE)

assert_condition <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

extract_json_string <- function(json_text, key) {
  pattern <- paste0(
    '"', key, '"\\s*:\\s*"((?:\\\\.|[^"\\\\])*)"'
  )
  match <- regexec(pattern, json_text, perl = TRUE)
  pieces <- regmatches(json_text, match)[[1]]

  if (length(pieces) < 2L) {
    stop(sprintf("Demographic field '%s' is missing from response JSON", key),
         call. = FALSE)
  }

  value <- pieces[[2]]
  value <- gsub("\\\\/", "/", value, fixed = TRUE)
  value <- gsub("\\\\n", "\n", value, fixed = TRUE)
  value <- gsub("\\\\r", "\r", value, fixed = TRUE)
  value <- gsub("\\\\t", "\t", value, fixed = TRUE)
  value <- gsub('\\\\"', '"', value, fixed = TRUE)
  value <- gsub("\\\\\\\\", "\\", value, fixed = TRUE)
  value
}

parse_demographics <- function(json_text) {
  values <- vapply(
    demographic_columns,
    function(key) extract_json_string(json_text, key),
    character(1)
  )
  as.list(values)
}

parse_integer_array <- function(json_text) {
  text <- trimws(json_text)
  assert_condition(
    grepl("^\\[.*\\]$", text),
    sprintf("Invalid integer-array JSON: %s", text)
  )

  inside <- substr(text, 2L, nchar(text) - 1L)
  if (!nzchar(trimws(inside))) return(integer(0))

  parts <- trimws(strsplit(inside, ",", fixed = TRUE)[[1]])
  assert_condition(
    all(grepl("^-?[0-9]+$", parts)),
    sprintf("Non-integer value in JSON array: %s", text)
  )
  as.integer(parts)
}

parse_item_response <- function(json_text) {
  pattern <- '"([A-Q]_[0-9]{2})"\\s*:\\s*(-?[0-9]+)'
  locations <- gregexpr(pattern, json_text, perl = TRUE)
  tokens <- regmatches(json_text, locations)[[1]]

  assert_condition(
    length(tokens) > 0L && !identical(tokens, character(0)),
    sprintf("No questionnaire items found in response JSON: %s", json_text)
  )

  item_names <- sub(pattern, "\\1", tokens, perl = TRUE)
  item_values <- as.integer(sub(pattern, "\\2", tokens, perl = TRUE))
  stats::setNames(item_values, item_names)
}

read_questionnaire_csv <- function(file_path) {
  utils::read.csv(
    file_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(0),
    quote = '"',
    comment.char = "",
    encoding = "UTF-8"
  )
}

quote_csv_field <- function(value) {
  if (length(value) == 0L || is.na(value)) value <- ""
  text <- enc2utf8(as.character(value))
  text <- gsub('"', '""', text, fixed = TRUE, useBytes = TRUE)
  paste0('"', text, '"')
}

write_utf8_csv <- function(data, file_path) {
  lines <- character(nrow(data) + 1L)
  lines[[1]] <- paste(
    vapply(names(data), quote_csv_field, character(1)),
    collapse = ","
  )

  for (row_index in seq_len(nrow(data))) {
    lines[[row_index + 1L]] <- paste(
      vapply(
        data[row_index, , drop = FALSE],
        function(column) quote_csv_field(column[[1]]),
        character(1)
      ),
      collapse = ","
    )
  }

  connection <- base::file(file_path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeLines(lines, connection, sep = "\r\n", useBytes = TRUE)
}

process_participant <- function(file_path) {
  file_name <- basename(file_path)
  data <- read_questionnaire_csv(file_path)

  assert_condition(nrow(data) == 19L, sprintf("Expected 19 rows, found %d", nrow(data)))
  assert_condition(!anyDuplicated(names(data)), "Duplicate column names found")

  required_metadata <- c(
    "participant_id", "section", "scale_id", "scale_position",
    "trial_index", "question_order", "response"
  )
  missing_metadata <- setdiff(required_metadata, names(data))
  assert_condition(
    length(missing_metadata) == 0L,
    sprintf("Missing metadata columns: %s", paste(missing_metadata, collapse = ", "))
  )

  missing_item_columns <- setdiff(expected_item_columns, names(data))
  assert_condition(
    length(missing_item_columns) == 0L,
    sprintf("Missing item columns: %s", paste(missing_item_columns, collapse = ", "))
  )

  participant_ids <- unique(data$participant_id[nzchar(data$participant_id)])
  assert_condition(length(participant_ids) == 1L, "Expected exactly one participant_id")
  participant_id <- participant_ids[[1]]
  expected_participant_id <- tools::file_path_sans_ext(file_name)
  assert_condition(
    identical(participant_id, expected_participant_id),
    sprintf("participant_id does not match filename: %s", participant_id)
  )

  trial_index <- suppressWarnings(as.integer(data$trial_index))
  assert_condition(
    identical(trial_index, 0:18),
    "trial_index must be the sequence 0 through 18"
  )

  demographic_rows <- data[data$section == "demographics", , drop = FALSE]
  assert_condition(nrow(demographic_rows) == 1L, "Expected one demographics row")
  demographics <- parse_demographics(demographic_rows$response[[1]])

  age <- suppressWarnings(as.integer(demographics$age))
  assert_condition(!is.na(age) && age >= 18L && age <= 120L, "Age is outside 18-120")
  demographic_text <- unlist(
    demographics[c("gender", "education", "marital_status", "income")],
    use.names = FALSE
  )
  assert_condition(
    all(nzchar(demographic_text)),
    "One or more demographic categories are blank"
  )

  scale_rows <- data[data$section == "scale", , drop = FALSE]
  assert_condition(nrow(scale_rows) == 17L, "Expected 17 scale rows")
  assert_condition(!anyDuplicated(scale_rows$scale_id), "Duplicate scale_id found")
  assert_condition(
    setequal(scale_rows$scale_id, names(expected_item_counts)),
    "The set of scale_id values is incomplete or unexpected"
  )

  scale_positions <- suppressWarnings(as.integer(scale_rows$scale_position))
  assert_condition(
    identical(scale_positions, seq_len(17L)),
    "scale_position must be the sequence 1 through 17"
  )

  record <- list(
    participant_id = participant_id,
    source_file = file_name,
    age = age,
    gender = demographics$gender,
    education = demographics$education,
    marital_status = demographics$marital_status,
    income = demographics$income
  )

  item_values <- integer(length(expected_item_columns))
  names(item_values) <- expected_item_columns
  panas_original_item_order <- NULL

  for (scale_id in names(expected_item_counts)) {
    row <- scale_rows[scale_rows$scale_id == scale_id, , drop = FALSE]
    expected_scale_items <- expected_items_by_scale[[scale_id]]
    response_values <- parse_item_response(row$response[[1]])

    assert_condition(
      identical(sort(names(response_values)), sort(expected_scale_items)),
      sprintf("%s response JSON has missing or unexpected item keys", scale_id)
    )

    question_order <- parse_integer_array(row$question_order[[1]])
    expected_zero_based_order <- 0:(expected_item_counts[[scale_id]] - 1L)
    assert_condition(
      identical(sort(question_order), expected_zero_based_order),
      sprintf("%s question_order is not a valid permutation", scale_id)
    )

    response_order <- sprintf("%s_%02d", scale_id, question_order + 1L)
    assert_condition(
      identical(names(response_values), response_order),
      sprintf("%s response keys do not match question_order", scale_id)
    )

    if (scale_id == "D") panas_original_item_order <- response_order

    for (item_name in expected_scale_items) {
      raw_value <- row[[item_name]][[1]]
      assert_condition(nzchar(raw_value), sprintf("%s is blank", item_name))
      assert_condition(
        grepl("^-?[0-9]+$", raw_value),
        sprintf("%s is not an integer", item_name)
      )

      value <- as.integer(raw_value)
      assert_condition(
        value >= minimum_scores[[scale_id]] && value <= maximum_scores[[scale_id]],
        sprintf("%s=%d is outside %d-%d", item_name, value,
                minimum_scores[[scale_id]], maximum_scores[[scale_id]])
      )
      assert_condition(
        identical(unname(response_values[[item_name]]), value),
        sprintf("%s differs between response JSON and item column", item_name)
      )

      populated_rows <- sum(nzchar(data[[item_name]]))
      assert_condition(
        populated_rows == 1L,
        sprintf("%s must be populated in exactly one row", item_name)
      )
      item_values[[item_name]] <- value
    }
  }

  for (item_name in expected_item_columns) {
    record[[item_name]] <- unname(item_values[[item_name]])
  }

  record$attention_A_13_pass <- item_values[["A_13"]] == 5L
  record$attention_H_13_pass <- item_values[["H_13"]] == 7L
  record$attention_H_23_pass <- item_values[["H_23"]] == 1L
  record$attention_all_pass <- all(c(
    record$attention_A_13_pass,
    record$attention_H_13_pass,
    record$attention_H_23_pass
  ))
  record$original_scale_order <- paste(scale_rows$scale_id, collapse = "|")
  record$PANAS_original_item_order <- paste(panas_original_item_order, collapse = "|")

  as.data.frame(record, check.names = FALSE, stringsAsFactors = FALSE)
}

assert_condition(dir.exists(input_directory), sprintf("Input directory not found: %s", input_directory))

csv_files <- sort(list.files(
  input_directory,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
))

csv_files <- csv_files[!basename(csv_files) %in% excluded_files]
assert_condition(length(csv_files) > 0L, "No input CSV files found after exclusions")

results <- lapply(csv_files, function(file_path) {
  tryCatch(
    list(data = process_participant(file_path), error = NULL),
    error = function(error) {
      list(
        data = NULL,
        error = sprintf("%s: %s", basename(file_path), conditionMessage(error))
      )
    }
  )
})

problems <- vapply(
  results,
  function(result) if (is.null(result$error)) "" else result$error,
  character(1)
)
problems <- problems[nzchar(problems)]

if (length(problems) > 0L) {
  shown <- head(problems, 20L)
  remaining <- length(problems) - length(shown)
  message_text <- paste(shown, collapse = "\n")
  if (remaining > 0L) {
    message_text <- paste0(message_text, sprintf("\n...and %d more error(s)", remaining))
  }
  stop(sprintf("Integration stopped because validation failed:\n%s", message_text), call. = FALSE)
}

merged_data <- do.call(rbind, lapply(results, `[[`, "data"))
row.names(merged_data) <- NULL

assert_condition(
  !anyDuplicated(merged_data$participant_id),
  "Duplicate participant_id values found across files"
)
assert_condition(
  nrow(merged_data) == length(csv_files),
  "Merged row count does not match input file count"
)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
temporary_output <- tempfile(
  pattern = "merged_raw_data_",
  tmpdir = dirname(output_file),
  fileext = ".csv"
)
on.exit(unlink(temporary_output, force = TRUE), add = TRUE)

write_utf8_csv(merged_data, temporary_output)

copied <- file.copy(temporary_output, output_file, overwrite = TRUE)
assert_condition(copied, sprintf("Could not write output file: %s", output_file))

verification <- utils::read.csv(
  output_file,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  colClasses = "character",
  na.strings = character(0),
  encoding = "UTF-8"
)

assert_condition(nrow(verification) == nrow(merged_data), "Saved output row count mismatch")
assert_condition(identical(names(verification), names(merged_data)), "Saved output columns mismatch")
assert_condition(!anyDuplicated(verification$participant_id), "Saved output contains duplicate participant_id")
for (column_name in demographic_columns) {
  assert_condition(
    identical(verification[[column_name]], as.character(merged_data[[column_name]])),
    sprintf("Saved output text mismatch in %s", column_name)
  )
}

temporary_removed <- unlink(temporary_output, force = TRUE)
assert_condition(
  identical(temporary_removed, 0L) && !file.exists(temporary_output),
  sprintf("Could not remove temporary output file: %s", temporary_output)
)

cat(sprintf("Input directory: %s\n", normalizePath(input_directory, winslash = "/")))
cat(sprintf("Excluded files: %s\n", if (length(excluded_files)) paste(excluded_files, collapse = ", ") else "none"))
cat(sprintf("Participants merged: %d\n", nrow(merged_data)))
cat(sprintf("Output columns: %d (173 questionnaire items)\n", ncol(merged_data)))
cat(sprintf("All attention checks passed: %d/%d\n", sum(merged_data$attention_all_pass), nrow(merged_data)))
cat("PANAS columns normalized to D_01-D_10: yes\n")
cat(sprintf("Output file: %s\n", normalizePath(output_file, winslash = "/", mustWork = TRUE)))
