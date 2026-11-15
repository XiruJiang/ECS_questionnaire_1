# Generate thesis-ready path diagrams corresponding to Figures 1-3.
# Figure 1: hypothesized model.
# Figure 2: primary latent SEM, with Model 1 / Model 2 coefficients.
# Figure 3: seven emotion-specific sensitivity models.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
analysis_script_directory <- if (length(file_argument) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_argument), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(analysis_script_directory, "_analysis_utils.R"), encoding = "UTF-8")

output_directory <- ensure_directory(file.path(results_directory, "08_sem_figures"))
primary_directory <- file.path(results_directory, "06_sem_primary")
emotion_directory <- file.path(results_directory, "07_emotion_specific_sem")

model1_file <- file.path(primary_directory, "06_model1_sequential_parameters.csv")
model2_file <- file.path(primary_directory, "06_model2_er_controls_parameters.csv")
emotion_file <- file.path(emotion_directory, "07_emotion_specific_parameters.csv")
fit_file <- file.path(emotion_directory, "07_emotion_specific_fit_indices.csv")

model1 <- read_utf8_csv(model1_file, all_character = FALSE)
model2 <- read_utf8_csv(model2_file, all_character = FALSE)
emotion_parameters <- read_utf8_csv(emotion_file, all_character = FALSE)
emotion_fit <- read_utf8_csv(fit_file, all_character = FALSE)

required_parameter_columns <- c("lhs", "op", "rhs", "label", "pvalue", "std.all")
for (data_name in c("model1", "model2", "emotion_parameters")) {
  current_data <- get(data_name)
  missing_columns <- setdiff(required_parameter_columns, names(current_data))
  assert_condition(
    length(missing_columns) == 0L,
    sprintf("%s is missing: %s", data_name, paste(missing_columns, collapse = ", "))
  )
}
assert_condition("emotion_version" %in% names(emotion_parameters), "Emotion parameters have no emotion_version column")

significance_stars <- function(p_value) {
  if (length(p_value) == 0L || is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  ""
}

path_row <- function(parameters, label) {
  row <- parameters[parameters$op == "~" & parameters$label == label, , drop = FALSE]
  assert_condition(nrow(row) == 1L, sprintf("Expected one path labelled %s; found %d", label, nrow(row)))
  row
}

format_path <- function(parameters, label) {
  row <- path_row(parameters, label)
  value <- as.numeric(row$std.all[[1]])
  p_value <- as.numeric(row$pvalue[[1]])
  paste0(sprintf("%.2f", value), significance_stars(p_value))
}

format_path_pair <- function(label) {
  paste0(format_path(model1, label), " / ", format_path(model2, label))
}

node_palette <- list(
  attachment = "#E8EEF8",
  awareness = "#DDEBF7",
  action = "#D9EAD3",
  affect = "#FFF2CC",
  outcome = "#FCE4D6",
  border = "#374151",
  arrow = "#374151",
  positive = "#2F6B4F",
  negative = "#9C3D3D"
)

draw_box <- function(x, y, width, height, label, fill, cex = 0.92, border = node_palette$border) {
  graphics::rect(
    x - width / 2,
    y - height / 2,
    x + width / 2,
    y + height / 2,
    col = fill,
    border = border,
    lwd = 1.5
  )
  graphics::text(x, y, labels = label, cex = cex, col = "#111827", family = "sans")
}

draw_arrow <- function(x0, y0, x1, y1, label = NULL, label_x = NULL, label_y = NULL, cex = 0.82) {
  graphics::arrows(
    x0, y0, x1, y1,
    length = 0.08,
    angle = 22,
    code = 2,
    lwd = 1.5,
    col = node_palette$arrow
  )
  if (!is.null(label)) {
    if (is.null(label_x)) label_x <- (x0 + x1) / 2
    if (is.null(label_y)) label_y <- (y0 + y1) / 2 + 0.035
    graphics::text(
      label_x, label_y, labels = label,
      cex = cex, font = 2, col = "#111827", family = "sans"
    )
  }
}

draw_double_arrow <- function(x0, y0, x1, y1) {
  graphics::arrows(
    x0, y0, x1, y1,
    length = 0.07,
    angle = 22,
    code = 3,
    lwd = 1.1,
    col = "#6B7280"
  )
}

open_pdf_device <- function(path, width, height) {
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(path, width = width, height = height, family = "sans")
  } else {
    grDevices::pdf(path, width = width, height = height, family = "sans", useDingbats = FALSE)
  }
}

render_figure <- function(stem, width, height, draw_function) {
  png_path <- file.path(output_directory, paste0(stem, ".png"))
  pdf_path <- file.path(output_directory, paste0(stem, ".pdf"))

  grDevices::png(
    filename = png_path,
    width = round(width * 180),
    height = round(height * 180),
    res = 180,
    bg = "white"
  )
  draw_function()
  grDevices::dev.off()

  open_pdf_device(pdf_path, width, height)
  draw_function()
  grDevices::dev.off()

  c(png = png_path, pdf = pdf_path)
}

draw_theoretical_model <- function() {
  graphics::par(mar = c(2.2, 1.2, 3.2, 1.2), family = "sans", xpd = NA)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::title(
    main = "Figure 1. Hypothesized structural model",
    cex.main = 1.25,
    font.main = 2,
    col.main = "#111827"
  )

  draw_box(0.09, 0.66, 0.16, 0.13, "Attachment\nanxiety", node_palette$attachment)
  draw_box(0.09, 0.34, 0.16, 0.13, "Intimacy\navoidance", node_palette$attachment)
  draw_box(0.30, 0.50, 0.15, 0.13, "EC\nAwareness", node_palette$awareness)
  draw_box(0.49, 0.50, 0.14, 0.13, "EC\nAction", node_palette$action)
  draw_box(0.67, 0.50, 0.15, 0.13, "Positive\naffect", node_palette$affect)

  outcome_y <- c(0.78, 0.59, 0.40, 0.21)
  outcome_labels <- c("Life\nsatisfaction", "Vitality", "Eudaimonic\nwell-being", "Psychological\ndistress")
  for (index in seq_along(outcome_y)) {
    draw_box(0.89, outcome_y[[index]], 0.18, 0.115, outcome_labels[[index]], node_palette$outcome, cex = 0.82)
  }

  draw_arrow(0.17, 0.64, 0.225, 0.55)
  draw_arrow(0.17, 0.36, 0.225, 0.45)
  draw_double_arrow(0.09, 0.59, 0.09, 0.41)
  draw_arrow(0.375, 0.50, 0.42, 0.50)
  draw_arrow(0.56, 0.50, 0.595, 0.50)
  for (index in seq_along(outcome_y)) {
    draw_arrow(0.745, 0.50, 0.80, outcome_y[[index]])
  }

  graphics::mtext(
    "Background characteristics are included as covariates but omitted for clarity.",
    side = 1, line = 0.4, cex = 0.78, col = "#4B5563"
  )
}

primary_diagnostics_file <- file.path(primary_directory, "06_sem_diagnostics.txt")
primary_diagnostics <- if (file.exists(primary_diagnostics_file)) {
  readLines(primary_diagnostics_file, warn = FALSE, encoding = "UTF-8")
} else {
  character(0)
}
primary_admissible <- !any(grepl("admissible_post_check=FALSE", primary_diagnostics, fixed = TRUE))

draw_primary_results <- function() {
  graphics::par(mar = c(4.0, 1.2, 3.3, 1.2), family = "sans", xpd = NA)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::title(
    main = "Figure 2. Primary structural model",
    cex.main = 1.25,
    font.main = 2,
    col.main = "#111827"
  )

  draw_box(0.09, 0.68, 0.16, 0.13, "Attachment\nanxiety", node_palette$attachment)
  draw_box(0.09, 0.32, 0.16, 0.13, "Intimacy\navoidance", node_palette$attachment)
  draw_box(0.30, 0.50, 0.15, 0.13, "EC\nAwareness", node_palette$awareness)
  draw_box(0.49, 0.50, 0.14, 0.13, "EC\nAction", node_palette$action)
  draw_box(0.67, 0.50, 0.15, 0.13, "Positive\naffect", node_palette$affect)

  outcome_y <- c(0.78, 0.59, 0.40, 0.21)
  outcome_labels <- c("Life\nsatisfaction", "Vitality", "Eudaimonic\nwell-being", "Psychological\ndistress")
  outcome_paths <- c("c1", "c2", "c3", "c4")
  for (index in seq_along(outcome_y)) {
    draw_box(0.89, outcome_y[[index]], 0.18, 0.115, outcome_labels[[index]], node_palette$outcome, cex = 0.82)
  }

  draw_arrow(0.17, 0.66, 0.225, 0.55, format_path_pair("d1"), 0.205, 0.665, 0.78)
  draw_arrow(0.17, 0.34, 0.225, 0.45, format_path_pair("d2"), 0.205, 0.335, 0.78)
  draw_double_arrow(0.09, 0.61, 0.09, 0.39)
  draw_arrow(0.375, 0.50, 0.42, 0.50, format_path_pair("a"), 0.397, 0.57, 0.78)
  draw_arrow(0.56, 0.50, 0.595, 0.50, format_path_pair("b"), 0.578, 0.57, 0.78)
  for (index in seq_along(outcome_y)) {
    label_y <- (0.50 + outcome_y[[index]]) / 2
    draw_arrow(
      0.745, 0.50, 0.80, outcome_y[[index]],
      format_path_pair(outcome_paths[[index]]),
      0.775,
      label_y + ifelse(index <= 2, 0.025, -0.025),
      0.72
    )
  }

  graphics::mtext(
    "Standardized coefficients: Model 1 / Model 2. Other ER measures and background covariates are omitted.",
    side = 1, line = 0.4, cex = 0.74, col = "#4B5563"
  )
  graphics::mtext(
    "* p < .05; ** p < .01; *** p < .001",
    side = 1, line = 1.45, cex = 0.72, col = "#4B5563"
  )
  if (!primary_admissible) {
    graphics::mtext(
      "Current-data warning: model post-check is not admissible; do not report these coefficients as final results.",
      side = 1, line = 2.4, cex = 0.72, col = "#B42318", font = 2
    )
  }
}

emotion_names <- c(
  happiness = "Happiness",
  satisfaction = "Satisfaction",
  enthusiasm = "Enthusiasm",
  pride = "Pride",
  fascination = "Fascination",
  being_loved = "Being loved",
  feeling_energetic = "Feeling energetic"
)

draw_emotion_panel <- function(version) {
  current <- emotion_parameters[emotion_parameters$emotion_version == version, , drop = FALSE]
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::title(main = unname(emotion_names[[version]]), cex.main = 0.95, font.main = 2, line = 0.5)

  draw_box(0.08, 0.68, 0.13, 0.13, "ECR-A", node_palette$attachment, cex = 0.70)
  draw_box(0.08, 0.32, 0.13, 0.13, "ECR-V", node_palette$attachment, cex = 0.70)
  draw_box(0.29, 0.50, 0.12, 0.13, "AW", node_palette$awareness, cex = 0.74)
  draw_box(0.48, 0.50, 0.12, 0.13, "AC", node_palette$action, cex = 0.74)
  draw_box(0.67, 0.50, 0.12, 0.13, "PA", node_palette$affect, cex = 0.74)

  draw_arrow(0.145, 0.66, 0.23, 0.55, format_path(current, "d1"), 0.19, 0.68, 0.60)
  draw_arrow(0.145, 0.34, 0.23, 0.45, format_path(current, "d2"), 0.19, 0.32, 0.60)
  draw_arrow(0.35, 0.50, 0.42, 0.50, format_path(current, "a"), 0.385, 0.59, 0.62)
  draw_arrow(0.54, 0.50, 0.61, 0.50, format_path(current, "b"), 0.575, 0.59, 0.62)

  outcome_y <- c(0.79, 0.60, 0.40, 0.21)
  outcome_abbreviations <- c("LS", "VIT", "EWB", "DIST")
  outcome_paths <- c("c1", "c2", "c3", "c4")
  for (index in seq_along(outcome_y)) {
    draw_box(0.90, outcome_y[[index]], 0.15, 0.10, outcome_abbreviations[[index]], node_palette$outcome, cex = 0.62)
    draw_arrow(
      0.73, 0.50, 0.825, outcome_y[[index]],
      format_path(current, outcome_paths[[index]]),
      0.79,
      (0.50 + outcome_y[[index]]) / 2 + ifelse(index <= 2, 0.02, -0.02),
      0.54
    )
  }

  fit_row <- emotion_fit[emotion_fit$scale == version, , drop = FALSE]
  if (nrow(fit_row) == 1L) {
    graphics::mtext(
      sprintf("CFI = %.3f; RMSEA = %.3f", as.numeric(fit_row$cfi), as.numeric(fit_row$rmsea)),
      side = 1, line = -0.2, cex = 0.58, col = "#4B5563"
    )
  }
}

draw_emotion_results <- function() {
  graphics::par(
    mfrow = c(4, 2),
    mar = c(2.0, 0.5, 2.2, 0.5),
    oma = c(2.8, 0.6, 3.0, 0.6),
    family = "sans",
    xpd = NA
  )
  for (version in names(emotion_names)) draw_emotion_panel(version)

  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::text(0.08, 0.78, "Abbreviations", adj = 0, font = 2, cex = 0.9)
  graphics::text(
    0.08, 0.64,
    "ECR-A: attachment anxiety\nECR-V: intimacy avoidance\nAW: awareness   AC: action   PA: positive affect\nLS: life satisfaction   VIT: vitality\nEWB: eudaimonic well-being   DIST: psychological distress",
    adj = c(0, 1), cex = 0.72, col = "#374151", family = "sans"
  )
  graphics::text(
    0.08, 0.25,
    "Standardized coefficients\n* p < .05; ** p < .01; *** p < .001\nBackground covariates and other ER measures are omitted.",
    adj = c(0, 1), cex = 0.72, col = "#374151", family = "sans"
  )

  graphics::mtext(
    "Figure 3. Emotion-specific structural models",
    side = 3, outer = TRUE, line = 1.15, cex = 1.25, font = 2, col = "#111827"
  )
  graphics::mtext(
    "Generated from the current analysis output; verify formal-data diagnostics before reporting.",
    side = 1, outer = TRUE, line = 1.0, cex = 0.72, col = "#4B5563"
  )
}

figure1_paths <- render_figure("Figure_1_hypothesized_model", 12, 6.5, draw_theoretical_model)
figure2_paths <- render_figure("Figure_2_primary_sem_results", 12, 6.8, draw_primary_results)
figure3_paths <- render_figure("Figure_3_emotion_specific_models", 14, 12, draw_emotion_results)

caption_lines <- c(
  "Figure 1. Hypothesized structural model.",
  "ECR-GO attachment anxiety and intimacy avoidance predict EC awareness, which predicts EC action, positive affect, and four psychological outcomes.",
  "",
  "Figure 2. Primary structural model.",
  "Standardized coefficients before and after the slash are from Model 1 and Model 2, respectively. Model 2 includes other emotion-regulation measures. Background covariates, measurement loadings, and outcome residual covariances are omitted for clarity.",
  "",
  "Figure 3. Emotion-specific structural models.",
  "Each panel replaces general EC awareness/action with one emotion-specific version while retaining ECR-GO attachment anxiety and intimacy avoidance upstream.",
  "",
  "Significance: * p < .05; ** p < .01; *** p < .001.",
  "The current figures are generated from the current analysis output. Re-run after formal data collection and verify all model diagnostics before thesis reporting."
)
write_utf8_text(caption_lines, file.path(output_directory, "08_sem_figure_captions.txt"))

write_analysis_session_info(
  file.path(output_directory, "08_session_info.txt"),
  c(
    "Figures generated: hypothesized model, primary SEM, seven emotion-specific models.",
    "Primary coefficients are standardized estimates shown as Model 1 / Model 2.",
    sprintf("Primary SEM admissible at generation time: %s", primary_admissible),
    "Output formats: PNG (180 dpi) and PDF (vector)."
  )
)

for (path in c(figure1_paths, figure2_paths, figure3_paths)) {
  assert_condition(file.exists(path) && file.info(path)$size > 0L, sprintf("Figure output missing or empty: %s", path))
}

message("SEM Figures 1-3 generated in PNG and PDF formats.")
