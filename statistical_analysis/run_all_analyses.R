# Run all statistical analyses in order.
# Usage:
#   Rscript run_all_analyses.R
#   Rscript run_all_analyses.R --quick
# --quick uses 50 bootstrap draws for a fast procedural test. Final reporting
# should use the default 1000 draws.

get_script_directory <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command, value = TRUE)
  if (length(file_argument) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

analysis_directory <- get_script_directory()
arguments <- commandArgs(trailingOnly = TRUE)
quick_mode <- "--quick" %in% arguments

if (quick_mode) {
  Sys.setenv(SEM_BOOTSTRAP_DRAWS = "50")
} else if (!nzchar(Sys.getenv("SEM_BOOTSTRAP_DRAWS"))) {
  Sys.setenv(SEM_BOOTSTRAP_DRAWS = "1000")
}

scripts <- c(
  "01_descriptive_statistics.R",
  "02_reliability_analysis.R",
  "03_correlation_analysis.R",
  "04_mancova.R",
  "05_cfa_ecs.R",
  "06_sem_primary_model.R",
  "07_sem_emotion_specific_sensitivity.R"
)

run_started <- Sys.time()
log_lines <- c(
  sprintf("Run started: %s", format(run_started, "%Y-%m-%d %H:%M:%S %Z")),
  sprintf("Quick mode: %s", quick_mode),
  sprintf("SEM bootstrap draws: %s", Sys.getenv("SEM_BOOTSTRAP_DRAWS"))
)

for (script_name in scripts) {
  script_path <- file.path(analysis_directory, script_name)
  message("Running ", script_name)
  script_started <- Sys.time()
  sys.source(script_path, envir = new.env(parent = globalenv()))
  elapsed <- as.numeric(difftime(Sys.time(), script_started, units = "secs"))
  log_lines <- c(log_lines, sprintf("%s: completed in %.1f seconds", script_name, elapsed))
}

run_finished <- Sys.time()
log_lines <- c(
  log_lines,
  sprintf("Run finished: %s", format(run_finished, "%Y-%m-%d %H:%M:%S %Z")),
  sprintf("Total seconds: %.1f", as.numeric(difftime(run_finished, run_started, units = "secs"))),
  "",
  capture.output(sessionInfo())
)

results_directory <- file.path(analysis_directory, "results")
dir.create(results_directory, recursive = TRUE, showWarnings = FALSE)
connection <- base::file(file.path(results_directory, "run_all_log.txt"), open = "wb")
writeLines(enc2utf8(log_lines), connection, sep = "\r\n", useBytes = TRUE)
close(connection)

message("All analyses completed.")

