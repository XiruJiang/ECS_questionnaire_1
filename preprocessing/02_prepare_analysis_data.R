# Questionnaire data preprocessing
#
# Default input:          merged_raw_data.csv
# Default prepared data:  prepared_analysis_data.csv
# Default exclusion log:  preprocessing_exclusions.csv
# Default codebook:       preprocessing_codebook.csv
#
# Usage:
#   Rscript 02_prepare_analysis_data.R
#   Rscript 02_prepare_analysis_data.R input.csv prepared.csv exclusions.csv codebook.csv
#
# This script deliberately does not calculate scale/subscale scores. Those
# belong in 03_compute_scale_scores.R so that data cleaning and psychometric
# scoring remain independently auditable.

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

input_file <- if (length(arguments) >= 1L) {
  normalizePath(arguments[[1]], winslash = "/", mustWork = TRUE)
} else {
  file.path(script_directory, "merged_raw_data.csv")
}

prepared_output_file <- if (length(arguments) >= 2L) {
  normalizePath(arguments[[2]], winslash = "/", mustWork = FALSE)
} else {
  file.path(script_directory, "prepared_analysis_data.csv")
}

exclusion_output_file <- if (length(arguments) >= 3L) {
  normalizePath(arguments[[3]], winslash = "/", mustWork = FALSE)
} else {
  file.path(script_directory, "preprocessing_exclusions.csv")
}

codebook_output_file <- if (length(arguments) >= 4L) {
  normalizePath(arguments[[4]], winslash = "/", mustWork = FALSE)
} else {
  file.path(script_directory, "preprocessing_codebook.csv")
}

assert_condition <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_condition(file.exists(input_file), sprintf("Input file not found: %s", input_file))

output_paths <- c(prepared_output_file, exclusion_output_file, codebook_output_file)
assert_condition(
  length(unique(tolower(output_paths))) == length(output_paths),
  "Prepared data, exclusion log, and codebook must use different paths"
)
assert_condition(
  !tolower(input_file) %in% tolower(output_paths),
  "An output path must not overwrite the merged raw input"
)

read_utf8_csv <- function(file_path, all_character = TRUE) {
  utils::read.csv(
    file_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = if (all_character) "character" else NA,
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

  if (nrow(data) > 0L) {
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
  }

  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  temporary_file <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(file_path)), "_"),
    tmpdir = tempdir(),
    fileext = ".csv"
  )
  on.exit(unlink(temporary_file, force = TRUE), add = TRUE)

  connection <- base::file(temporary_file, open = "wb")
  writeLines(lines, connection, sep = "\r\n", useBytes = TRUE)
  close(connection)

  copied <- file.copy(temporary_file, file_path, overwrite = TRUE)
  assert_condition(copied, sprintf("Could not write output file: %s", file_path))
}

parse_integer_column <- function(values, column_name) {
  text <- trimws(as.character(values))
  valid <- nzchar(text) & grepl("^-?[0-9]+$", text)
  assert_condition(
    all(valid),
    sprintf("%s contains blank or non-integer values", column_name)
  )
  as.integer(text)
}

parse_logical_column <- function(values, column_name) {
  text <- tolower(trimws(as.character(values)))
  result <- rep(NA, length(text))
  result[text %in% c("true", "t", "1")] <- TRUE
  result[text %in% c("false", "f", "0")] <- FALSE
  assert_condition(
    !anyNA(result),
    sprintf("%s contains values that are not TRUE/FALSE", column_name)
  )
  result
}

make_item_names <- function(scale_id, item_count) {
  sprintf("%s_%02d", scale_id, seq_len(item_count))
}

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

expected_items_by_scale <- Map(
  make_item_names,
  names(expected_item_counts),
  as.integer(expected_item_counts)
)
names(expected_items_by_scale) <- names(expected_item_counts)
expected_item_columns <- unlist(expected_items_by_scale, use.names = FALSE)

required_columns <- c(
  "participant_id", "source_file", "age", "gender", "education",
  "marital_status", "income", expected_item_columns,
  "attention_A_13_pass", "attention_H_13_pass",
  "attention_H_23_pass", "attention_all_pass"
)

raw_data <- read_utf8_csv(input_file, all_character = TRUE)

assert_condition(nrow(raw_data) > 0L, "Input contains no participant rows")
assert_condition(!anyDuplicated(names(raw_data)), "Input contains duplicate column names")
missing_columns <- setdiff(required_columns, names(raw_data))
assert_condition(
  length(missing_columns) == 0L,
  sprintf("Input is missing required columns: %s", paste(missing_columns, collapse = ", "))
)
assert_condition(
  all(nzchar(raw_data$participant_id)),
  "participant_id contains blank values"
)
assert_condition(
  !anyDuplicated(raw_data$participant_id),
  "participant_id contains duplicates"
)

raw_data$age <- parse_integer_column(raw_data$age, "age")
assert_condition(
  all(raw_data$age >= 18L & raw_data$age <= 120L),
  "age contains values outside 18-120"
)

for (scale_id in names(expected_items_by_scale)) {
  for (item_name in expected_items_by_scale[[scale_id]]) {
    raw_data[[item_name]] <- parse_integer_column(raw_data[[item_name]], item_name)
    assert_condition(
      all(
        raw_data[[item_name]] >= minimum_scores[[scale_id]] &
          raw_data[[item_name]] <= maximum_scores[[scale_id]]
      ),
      sprintf(
        "%s contains values outside %d-%d",
        item_name,
        minimum_scores[[scale_id]],
        maximum_scores[[scale_id]]
      )
    )
  }
}

# Recompute attention checks from item responses instead of trusting flags.
attention_A_13_pass <- raw_data$A_13 == 5L
attention_H_13_pass <- raw_data$H_13 == 7L
attention_H_23_pass <- raw_data$H_23 == 1L
attention_all_pass <- attention_A_13_pass & attention_H_13_pass & attention_H_23_pass

stored_attention <- list(
  attention_A_13_pass = parse_logical_column(
    raw_data$attention_A_13_pass, "attention_A_13_pass"
  ),
  attention_H_13_pass = parse_logical_column(
    raw_data$attention_H_13_pass, "attention_H_13_pass"
  ),
  attention_H_23_pass = parse_logical_column(
    raw_data$attention_H_23_pass, "attention_H_23_pass"
  ),
  attention_all_pass = parse_logical_column(
    raw_data$attention_all_pass, "attention_all_pass"
  )
)

recomputed_attention <- list(
  attention_A_13_pass = attention_A_13_pass,
  attention_H_13_pass = attention_H_13_pass,
  attention_H_23_pass = attention_H_23_pass,
  attention_all_pass = attention_all_pass
)

for (flag_name in names(recomputed_attention)) {
  assert_condition(
    identical(stored_attention[[flag_name]], recomputed_attention[[flag_name]]),
    sprintf("Stored %s does not match its item response", flag_name)
  )
  raw_data[[flag_name]] <- recomputed_attention[[flag_name]]
}

failed_attention_items <- vapply(
  seq_len(nrow(raw_data)),
  function(row_index) {
    failed <- c(
      if (!attention_A_13_pass[[row_index]]) "A_13" else character(0),
      if (!attention_H_13_pass[[row_index]]) "H_13" else character(0),
      if (!attention_H_23_pass[[row_index]]) "H_23" else character(0)
    )
    paste(failed, collapse = "|")
  },
  character(1)
)

excluded_rows <- !attention_all_pass
exclusion_log <- data.frame(
  participant_id = raw_data$participant_id[excluded_rows],
  source_file = raw_data$source_file[excluded_rows],
  exclusion_reason = rep("failed_attention_check", sum(excluded_rows)),
  failed_attention_items = failed_attention_items[excluded_rows],
  attention_A_13_pass = attention_A_13_pass[excluded_rows],
  attention_H_13_pass = attention_H_13_pass[excluded_rows],
  attention_H_23_pass = attention_H_23_pass[excluded_rows],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

prepared_data <- raw_data[attention_all_pass, , drop = FALSE]
row.names(prepared_data) <- NULL

# Demographic coding policy:
# - Original Japanese labels are always retained.
# - Numeric codes are derived columns only.
# - Nominal codes are identifiers, not quantities.
# - "回答しない" is NA in analysis codes but remains visible in raw labels.
# - Education "その他" is not ordinal and is therefore NA in education_code.
gender_map <- stats::setNames(
  c(1L, 2L, 3L, NA_integer_),
  c(
    "\u7537\u6027",
    "\u5973\u6027",
    "\u305d\u306e\u4ed6",
    "\u56de\u7b54\u3057\u306a\u3044"
  )
)

education_map <- stats::setNames(
  c(1L, 2L, 3L, 4L, NA_integer_, NA_integer_),
  c(
    "\u4e2d\u5b66\u30fb\u9ad8\u6821",
    "\u5c02\u4fee\u30fb\u9ad8\u5c02\u30fb\u77ed\u5927",
    "\u56db\u5e74\u5236\u5927\u5b66",
    "\u5927\u5b66\u9662",
    "\u305d\u306e\u4ed6",
    "\u56de\u7b54\u3057\u306a\u3044"
  )
)

marital_status_map <- stats::setNames(
  c(1L, 2L, 3L, 4L, 5L, NA_integer_),
  c(
    "\u65e2\u5a5a\uff0f\u540c\u5c45\u4e2d",
    "\u72ec\u8eab",
    "\u4ea4\u969b\u76f8\u624b\u304c\u3044\u308b",
    "\u5225\u5c45\uff0f\u96e2\u5a5a",
    "\u6b7b\u5225",
    "\u56de\u7b54\u3057\u306a\u3044"
  )
)

income_map <- stats::setNames(
  c(1:11, NA_integer_),
  c(
    "100\u4e07\u5186\u672a\u6e80",
    "100\uff5e200\u4e07\u5186\u672a\u6e80",
    "200\uff5e300\u4e07\u5186\u672a\u6e80",
    "300\uff5e400\u4e07\u5186\u672a\u6e80",
    "400\uff5e500\u4e07\u5186\u672a\u6e80",
    "500\uff5e600\u4e07\u5186\u672a\u6e80",
    "600\uff5e700\u4e07\u5186\u672a\u6e80",
    "700\uff5e800\u4e07\u5186\u672a\u6e80",
    "800\uff5e900\u4e07\u5186\u672a\u6e80",
    "900\uff5e1000\u4e07\u5186\u672a\u6e80",
    "1000\u4e07\u5186\u4ee5\u4e0a",
    "\u56de\u7b54\u3057\u306a\u3044"
  )
)

validate_categories <- function(values, mapping, variable_name) {
  unexpected <- setdiff(unique(values), names(mapping))
  assert_condition(
    length(unexpected) == 0L,
    sprintf(
      "%s contains unexpected categories: %s",
      variable_name,
      paste(unexpected, collapse = ", ")
    )
  )
}

validate_categories(prepared_data$gender, gender_map, "gender")
validate_categories(prepared_data$education, education_map, "education")
validate_categories(
  prepared_data$marital_status, marital_status_map, "marital_status"
)
validate_categories(prepared_data$income, income_map, "income")

demographic_codes <- data.frame(
  gender_code = unname(gender_map[prepared_data$gender]),
  education_code = unname(education_map[prepared_data$education]),
  marital_status_code = unname(
    marital_status_map[prepared_data$marital_status]
  ),
  income_code = unname(income_map[prepared_data$income]),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

income_column_index <- match("income", names(prepared_data))
prepared_data <- cbind(
  prepared_data[, seq_len(income_column_index), drop = FALSE],
  demographic_codes,
  prepared_data[, -seq_len(income_column_index), drop = FALSE],
  stringsAsFactors = FALSE
)

# Confirmed reverse-scored items. Raw columns are preserved; reversed versions
# are appended with the suffix "_rev".
#
# PWBS selection (G): negatively worded items in the present 1=disagree to
# 7=agree response direction.
# Source: Japanese PWBS-42 materials and original scoring convention
# https://mhpn.m.u-tokyo.ac.jp/rs/pwb-42
#   G_01, G_04, G_06 -> 8 - raw
#
# ECR-GO (H): current questionnaire positions mapped to the reverse-keyed
# retained items reported by Nakao & Kato (2004). H_13 and H_23 are attention
# checks, not scale items.
# Source: Table 1, https://doi.org/10.15017/3567
#   H_03, H_11, H_16, H_19, H_22, H_25, H_27, H_29 -> 8 - raw
reverse_items <- c(
  "G_01", "G_04", "G_06",
  "H_03", "H_11", "H_16", "H_19", "H_22", "H_25", "H_27", "H_29"
)

for (item_name in reverse_items) {
  reversed_name <- paste0(item_name, "_rev")
  prepared_data[[reversed_name]] <- 8L - prepared_data[[item_name]]

  assert_condition(
    all(prepared_data[[reversed_name]] >= 1L & prepared_data[[reversed_name]] <= 7L),
    sprintf("Reverse-scored %s is outside 1-7", reversed_name)
  )
  assert_condition(
    identical(8L - prepared_data[[reversed_name]], prepared_data[[item_name]]),
    sprintf("Reverse-scoring verification failed for %s", item_name)
  )
}

make_codebook_rows <- function(variable_name, mapping, measurement_level, note) {
  data.frame(
    source_variable = variable_name,
    raw_value = names(mapping),
    analysis_variable = paste0(variable_name, "_code"),
    code = unname(mapping),
    measurement_level = measurement_level,
    note = note,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

codebook <- rbind(
  make_codebook_rows(
    "gender", gender_map, "nominal",
    "Codes identify categories; use factor/dummy coding in statistical models."
  ),
  make_codebook_rows(
    "education", education_map, "ordinal",
    "1-4 indicate order only; Other and No response are NA in the analysis code."
  ),
  make_codebook_rows(
    "marital_status", marital_status_map, "nominal",
    "Codes identify categories; use factor/dummy coding in statistical models."
  ),
  make_codebook_rows(
    "income", income_map, "ordinal",
    "1-11 indicate ordered income bands; No response is NA in the analysis code."
  )
)

reverse_codebook <- data.frame(
  source_variable = reverse_items,
  raw_value = rep("1-7", length(reverse_items)),
  analysis_variable = paste0(reverse_items, "_rev"),
  code = rep(NA_integer_, length(reverse_items)),
  measurement_level = rep("scale item", length(reverse_items)),
  note = rep("Reverse score = 8 - raw response.", length(reverse_items)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
codebook <- rbind(codebook, reverse_codebook)

assert_condition(
  nrow(prepared_data) + nrow(exclusion_log) == nrow(raw_data),
  "Included and excluded row counts do not reconcile to input"
)
assert_condition(
  all(prepared_data$attention_all_pass),
  "Prepared data still contains a failed attention check"
)
assert_condition(
  identical(prepared_data$participant_id, raw_data$participant_id[attention_all_pass]),
  "Participant order changed unexpectedly during filtering"
)

write_utf8_csv(prepared_data, prepared_output_file)
write_utf8_csv(exclusion_log, exclusion_output_file)
write_utf8_csv(codebook, codebook_output_file)

# Read back saved files to catch encoding, row-count, and column-order errors.
prepared_verification <- read_utf8_csv(prepared_output_file, all_character = TRUE)
exclusion_verification <- read_utf8_csv(exclusion_output_file, all_character = TRUE)
codebook_verification <- read_utf8_csv(codebook_output_file, all_character = TRUE)

assert_condition(
  nrow(prepared_verification) == nrow(prepared_data),
  "Saved prepared-data row count mismatch"
)
assert_condition(
  identical(names(prepared_verification), names(prepared_data)),
  "Saved prepared-data column order mismatch"
)
assert_condition(
  identical(prepared_verification$participant_id, prepared_data$participant_id),
  "Saved prepared-data participant IDs mismatch"
)
for (column_name in c("gender", "education", "marital_status", "income")) {
  assert_condition(
    identical(
      prepared_verification[[column_name]],
      as.character(prepared_data[[column_name]])
    ),
    sprintf("Saved Japanese text mismatch in %s", column_name)
  )
}
assert_condition(
  nrow(exclusion_verification) == nrow(exclusion_log),
  "Saved exclusion-log row count mismatch"
)
assert_condition(
  nrow(codebook_verification) == nrow(codebook),
  "Saved codebook row count mismatch"
)

cat(sprintf("Input file: %s\n", normalizePath(input_file, winslash = "/")))
cat(sprintf("Participants before preprocessing: %d\n", nrow(raw_data)))
cat(sprintf("Excluded for failed attention checks: %d\n", nrow(exclusion_log)))
cat(sprintf("Participants retained: %d\n", nrow(prepared_data)))
cat(sprintf("Reverse-scored item columns added: %d\n", length(reverse_items)))
cat("Raw demographic labels and raw questionnaire items preserved: yes\n")
cat("Scale/subscale scores calculated: no (reserved for 03_compute_scale_scores.R)\n")
cat(sprintf(
  "Prepared data: %s\n",
  normalizePath(prepared_output_file, winslash = "/", mustWork = TRUE)
))
cat(sprintf(
  "Exclusion log: %s\n",
  normalizePath(exclusion_output_file, winslash = "/", mustWork = TRUE)
))
cat(sprintf(
  "Codebook: %s\n",
  normalizePath(codebook_output_file, winslash = "/", mustWork = TRUE)
))
