# Install only the packages required for reliability analysis, CFA, and SEM.

required_packages <- c("psych", "lavaan")

# R does not add the per-user package library to .libPaths() until the
# directory exists. Create it explicitly so installation never falls back to
# the protected Program Files library on Windows.
user_library <- Sys.getenv("R_LIBS_USER")
if (!nzchar(user_library)) {
  user_library <- file.path(path.expand("~"), "R", "win-library", paste(R.version$major, R.version$minor, sep = "."))
}
dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(user_library)) {
  stop("Could not create the user R library: ", user_library, call. = FALSE)
}
.libPaths(c(user_library, .libPaths()))

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) == 0L) {
  message("Required packages are already installed: ", paste(required_packages, collapse = ", "))
} else {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(
    missing,
    lib = user_library,
    repos = "https://cloud.r-project.org",
    dependencies = TRUE
  )
}

still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(still_missing) > 0L) {
  stop("Installation failed for: ", paste(still_missing, collapse = ", "), call. = FALSE)
}

message("Package check passed.")

