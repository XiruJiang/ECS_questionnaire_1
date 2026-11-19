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

# Unicode escapes keep Japanese labels stable even when R starts in the C locale.
figure_font_family <- if (.Platform$OS.type == "windows") "Yu Gothic" else "sans"
jp <- list(
  figure1_title = "\u56F31  \u4EEE\u8AAC\u69CB\u9020\u30E2\u30C7\u30EB",
  figure2_title = "\u56F32  \u4E3B\u8981\u69CB\u9020\u30E2\u30C7\u30EB",
  figure3_title = "\u56F33  \u611F\u60C5\u5225\u69CB\u9020\u30E2\u30C7\u30EB\uFF08\u7D71\u5408\u56F3\uFF09",
  attachment_anxiety = "\u898B\u6368\u3066\u3089\u308C\n\u4E0D\u5B89",
  intimacy_avoidance = "\u89AA\u5BC6\u6027\n\u56DE\u907F",
  awareness = "EC\n\u6C17\u3065\u304D",
  action = "EC\n\u884C\u52D5",
  positive_affect = "\u30DD\u30B8\u30C6\u30A3\u30D6\n\u611F\u60C5",
  life_satisfaction = "\u4EBA\u751F\n\u6E80\u8DB3\u5EA6",
  vitality = "\u4E3B\u89B3\u7684\n\u6D3B\u529B",
  eudaimonic = "\u5FC3\u7406\u7684\n\u5145\u5B9F\u611F",
  distress = "\u5FC3\u7406\u7684\n\u82E6\u75DB",
  theoretical_note = "\u80CC\u666F\u5C5E\u6027\u306F\u5171\u5909\u91CF\u3068\u3057\u3066\u30E2\u30C7\u30EB\u306B\u542B\u307E\u308C\u308B\u304C\u3001\u56F3\u3067\u306F\u7701\u7565\u3057\u3066\u3044\u308B\u3002",
  hypothesis_direction_note = "+: \u6B63\u306E\u95A2\u9023\u3001-: \u8CA0\u306E\u95A2\u9023\uFF08\u4EEE\u8AAC\uFF09",
  coefficient_note = "\u6A19\u6E96\u5316\u4FC2\u6570\uFF1A\u30E2\u30C7\u30EB1 \uFF0F \u30E2\u30C7\u30EB2\u3002\u53CC\u65B9\u5411\u77E2\u5370\u306F\u76F8\u95A2\u307E\u305F\u306F\u6B8B\u5DEE\u76F8\u95A2\u3092\u793A\u3059\u3002",
  inadmissible_warning = "\u73FE\u5728\u306E\u30C7\u30FC\u30BF\u3067\u306F\u30E2\u30C7\u30EB\u304C\u4E0D\u9069\u89E3\u306E\u305F\u3081\u3001\u6700\u7D42\u7D50\u679C\u3068\u3057\u3066\u5831\u544A\u3057\u306A\u3044\u3053\u3068\u3002",
  panel_note_title = "\u6CE8",
  panel_note = "\u5404\u30D1\u30CD\u30EB\u306F\u611F\u60C5\u5225ECS\u306E\u30E2\u30C7\u30EB\u3092\u793A\u3059\u3002\n\u6570\u5024\u306F\u6A19\u6E96\u5316\u4FC2\u6570\u3002\n\u53CC\u65B9\u5411\u77E2\u5370\u306F\u76F8\u95A2\u307E\u305F\u306F\u6B8B\u5DEE\u76F8\u95A2\u3092\u793A\u3059\u3002",
  generated_note = "\u73FE\u5728\u306E\u5206\u6790\u7D50\u679C\u304B\u3089\u4F5C\u6210\u3002\u6B63\u5F0F\u30C7\u30FC\u30BF\u3067\u306F\u30E2\u30C7\u30EB\u8A3A\u65AD\u3092\u78BA\u8A8D\u3057\u3066\u304B\u3089\u5831\u544A\u3059\u308B\u3053\u3068\u3002"
)

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

covariance_row <- function(parameters, lhs, rhs) {
  row <- parameters[
    parameters$op == "~~" &
      ((parameters$lhs == lhs & parameters$rhs == rhs) |
        (parameters$lhs == rhs & parameters$rhs == lhs)),
    , drop = FALSE
  ]
  assert_condition(
    nrow(row) == 1L,
    sprintf("Expected one covariance between %s and %s; found %d", lhs, rhs, nrow(row))
  )
  row
}

format_covariance <- function(parameters, lhs, rhs) {
  row <- covariance_row(parameters, lhs, rhs)
  value <- as.numeric(row$std.all[[1]])
  p_value <- as.numeric(row$pvalue[[1]])
  paste0(sprintf("%.2f", value), significance_stars(p_value))
}

format_covariance_pair <- function(lhs, rhs) {
  paste0(
    format_covariance(model1, lhs, rhs),
    " / ",
    format_covariance(model2, lhs, rhs)
  )
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
  graphics::text(x, y, labels = label, cex = cex, col = "#111827", family = figure_font_family)
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
      cex = cex, font = 2, col = "#111827", family = figure_font_family
    )
  }
}

draw_double_arrow <- function(
  x0, y0, x1, y1, label = NULL,
  label_x = NULL, label_y = NULL, cex = 0.72
) {
  graphics::arrows(
    x0, y0, x1, y1,
    length = 0.07,
    angle = 22,
    code = 3,
    lwd = 1.1,
    col = "#6B7280"
  )
  if (!is.null(label)) {
    if (is.null(label_x)) label_x <- (x0 + x1) / 2
    if (is.null(label_y)) label_y <- (y0 + y1) / 2
    graphics::text(
      label_x, label_y, labels = label,
      cex = cex, font = 2, col = "#4B5563", family = figure_font_family
    )
  }
}

draw_curved_double_arrow <- function(
  x, y0, y1, bulge, label,
  cex = 0.58, label_offset = 0.006, label_y_offset = 0
) {
  t <- seq(0, 1, length.out = 60)
  control_x <- x + bulge
  control_y <- (y0 + y1) / 2
  curve_x <- (1 - t)^2 * x + 2 * (1 - t) * t * control_x + t^2 * x
  curve_y <- (1 - t)^2 * y0 + 2 * (1 - t) * t * control_y + t^2 * y1
  graphics::lines(curve_x, curve_y, lwd = 1.0, col = "#6B7280")
  graphics::arrows(
    curve_x[[3]], curve_y[[3]], curve_x[[1]], curve_y[[1]],
    length = 0.055, angle = 22, code = 2, lwd = 1.0, col = "#6B7280"
  )
  last <- length(curve_x)
  graphics::arrows(
    curve_x[[last - 2]], curve_y[[last - 2]], curve_x[[last]], curve_y[[last]],
    length = 0.055, angle = 22, code = 2, lwd = 1.0, col = "#6B7280"
  )
  midpoint <- ceiling(last / 2)
  graphics::text(
    curve_x[[midpoint]] + label_offset,
    curve_y[[midpoint]] + label_y_offset,
    labels = label,
    cex = cex, font = 2, col = "#4B5563", family = figure_font_family
  )
}

open_pdf_device <- function(path, width, height) {
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(path, width = width, height = height, family = figure_font_family)
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
    bg = "white",
    type = if (capabilities("cairo")) "cairo" else "windows"
  )
  draw_function()
  grDevices::dev.off()

  open_pdf_device(pdf_path, width, height)
  draw_function()
  grDevices::dev.off()

  c(png = png_path, pdf = pdf_path)
}

draw_theoretical_model <- function() {
  graphics::par(mar = c(3.0, 1.2, 3.2, 1.2), family = figure_font_family, xpd = NA)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::title(
    main = jp$figure1_title,
    cex.main = 1.25,
    font.main = 2,
    col.main = "#111827"
  )

  draw_box(0.09, 0.66, 0.16, 0.13, jp$attachment_anxiety, node_palette$attachment)
  draw_box(0.09, 0.34, 0.16, 0.13, jp$intimacy_avoidance, node_palette$attachment)
  draw_box(0.30, 0.50, 0.15, 0.13, jp$awareness, node_palette$awareness)
  draw_box(0.49, 0.50, 0.14, 0.13, jp$action, node_palette$action)
  draw_box(0.67, 0.50, 0.15, 0.13, jp$positive_affect, node_palette$affect)

  outcome_y <- c(0.78, 0.59, 0.40, 0.21)
  outcome_labels <- c(jp$life_satisfaction, jp$vitality, jp$eudaimonic, jp$distress)
  for (index in seq_along(outcome_y)) {
    draw_box(0.89, outcome_y[[index]], 0.18, 0.115, outcome_labels[[index]], node_palette$outcome, cex = 0.82)
  }

  draw_arrow(0.17, 0.64, 0.225, 0.55, "-", 0.205, 0.68, 0.90)
  draw_arrow(0.17, 0.36, 0.225, 0.45, "-", 0.205, 0.32, 0.90)
  draw_double_arrow(0.09, 0.59, 0.09, 0.41)
  draw_arrow(0.375, 0.50, 0.42, 0.50, "+", 0.397, 0.57, 0.90)
  draw_arrow(0.56, 0.50, 0.595, 0.50, "+", 0.578, 0.57, 0.90)
  for (index in seq_along(outcome_y)) {
    label_y <- (0.50 + outcome_y[[index]]) / 2
    draw_arrow(
      0.745, 0.50, 0.80, outcome_y[[index]],
      ifelse(index <= 3, "+", "-"),
      0.775,
      label_y + ifelse(index <= 2, 0.025, -0.025),
      0.86
    )
  }

  graphics::mtext(
    jp$theoretical_note,
    side = 1, line = 0.4, cex = 0.78, col = "#4B5563"
  )
  graphics::mtext(
    jp$hypothesis_direction_note,
    side = 1, line = 1.35, cex = 0.76, col = "#4B5563"
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
  graphics::par(mar = c(4.0, 1.2, 3.3, 1.2), family = figure_font_family, xpd = NA)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1.24), ylim = c(0, 1))
  graphics::title(
    main = jp$figure2_title,
    cex.main = 1.25,
    font.main = 2,
    col.main = "#111827"
  )

  draw_box(0.09, 0.68, 0.16, 0.13, jp$attachment_anxiety, node_palette$attachment)
  draw_box(0.09, 0.32, 0.16, 0.13, jp$intimacy_avoidance, node_palette$attachment)
  draw_box(0.30, 0.50, 0.15, 0.13, jp$awareness, node_palette$awareness)
  draw_box(0.49, 0.50, 0.14, 0.13, jp$action, node_palette$action)
  draw_box(0.67, 0.50, 0.15, 0.13, jp$positive_affect, node_palette$affect)

  outcome_y <- c(0.78, 0.59, 0.40, 0.21)
  outcome_labels <- c(jp$life_satisfaction, jp$vitality, jp$eudaimonic, jp$distress)
  outcome_paths <- c("c1", "c2", "c3", "c4")
  for (index in seq_along(outcome_y)) {
    draw_box(0.89, outcome_y[[index]], 0.18, 0.115, outcome_labels[[index]], node_palette$outcome, cex = 0.82)
  }

  draw_arrow(0.17, 0.66, 0.225, 0.55, format_path_pair("d1"), 0.205, 0.665, 0.78)
  draw_arrow(0.17, 0.34, 0.225, 0.45, format_path_pair("d2"), 0.205, 0.335, 0.78)
  draw_double_arrow(
    0.09, 0.61, 0.09, 0.39,
    format_covariance_pair("attachment_anxiety", "intimacy_avoidance"),
    0.035, 0.50, 0.68
  )
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

  primary_outcome_covariances <- list(
    c(1, 2, 0.040, "life_satisfaction", "vitality", 0.000),
    c(2, 3, 0.040, "vitality", "eudaimonic", 0.035),
    c(3, 4, 0.040, "eudaimonic", "distress", 0.000),
    c(1, 3, 0.200, "life_satisfaction", "eudaimonic", 0.000),
    c(2, 4, 0.200, "vitality", "distress", -0.005),
    c(1, 4, 0.360, "life_satisfaction", "distress", -0.035)
  )
  for (specification in primary_outcome_covariances) {
    first <- as.integer(specification[[1]])
    second <- as.integer(specification[[2]])
    draw_curved_double_arrow(
      0.985,
      outcome_y[[first]],
      outcome_y[[second]],
      as.numeric(specification[[3]]),
      format_covariance_pair(specification[[4]], specification[[5]]),
      cex = 0.46,
      label_offset = 0.004,
      label_y_offset = as.numeric(specification[[6]])
    )
  }

  graphics::mtext(
    jp$coefficient_note,
    side = 1, line = 0.4, cex = 0.74, col = "#4B5563"
  )
  graphics::mtext(
    "* p < .05; ** p < .01; *** p < .001",
    side = 1, line = 1.45, cex = 0.72, col = "#4B5563"
  )
  if (!primary_admissible) {
    graphics::mtext(
      jp$inadmissible_warning,
      side = 1, line = 2.4, cex = 0.72, col = "#B42318", font = 2
    )
  }
}

emotion_names <- c(
  happiness = "\u5E78\u798F",
  satisfaction = "\u6E80\u8DB3",
  being_loved = "\u611B\u3055\u308C\u3066\u3044\u308B\u611F\u899A",
  fascination = "\u5F37\u3044\u95A2\u5FC3",
  enthusiasm = "\u71B1\u610F",
  feeling_energetic = "\u30A8\u30CD\u30EB\u30AE\u30C3\u30B7\u30E5",
  pride = "\u8A87\u308A"
)
emotion_codes <- stats::setNames(letters[seq_along(emotion_names)], names(emotion_names))
emotion_superscripts <- stats::setNames(
  c("\u1D43", "\u1D47", "\u1D9C", "\u1D48", "\u1D49", "\u1DA0", "\u1D4D"),
  names(emotion_names)
)

format_standardized_value <- function(value) {
  sub("^(-?)0\\.", "\\1.", sprintf("%.2f", as.numeric(value)))
}

emotion_path_rows <- function(label, significant_only = FALSE) {
  rows <- emotion_parameters[
    emotion_parameters$op == "~" & emotion_parameters$label == label,
    , drop = FALSE
  ]
  rows <- rows[match(names(emotion_names), rows$emotion_version), , drop = FALSE]
  rows <- rows[!is.na(rows$emotion_version), , drop = FALSE]
  if (significant_only) {
    rows <- rows[!is.na(rows$pvalue) & rows$pvalue < 0.05, , drop = FALSE]
  }
  rows
}

format_emotion_coefficients <- function(
  label, significant_only = FALSE, per_line = 4L,
  include_superscript = FALSE
) {
  rows <- emotion_path_rows(label, significant_only = significant_only)
  if (nrow(rows) == 0L) return("NS")
  if (include_superscript) {
    plotmath_pieces <- vapply(seq_len(nrow(rows)), function(index) {
      coefficient <- paste0(
        format_standardized_value(rows$std.all[[index]]),
        significance_stars(as.numeric(rows$pvalue[[index]]))
      )
      sprintf(
        "\"%s\"^%s",
        coefficient,
        emotion_codes[[rows$emotion_version[[index]]]]
      )
    }, character(1))
    return(parse(text = paste(plotmath_pieces, collapse = " * \" / \" * "))[[1]])
  }
  pieces <- vapply(seq_len(nrow(rows)), function(index) {
    coefficient <- paste0(
      format_standardized_value(rows$std.all[[index]]),
      significance_stars(as.numeric(rows$pvalue[[index]]))
    )
    coefficient
  }, character(1))
  groups <- ceiling(seq_along(pieces) / per_line)
  paste(vapply(split(pieces, groups), paste, collapse = " / ", FUN.VALUE = character(1)), collapse = "\n")
}

format_rows_range <- function(rows) {
  assert_condition(nrow(rows) > 0L, "Cannot format an empty coefficient range")
  values <- as.numeric(rows$std.all)
  lower <- min(values, na.rm = TRUE)
  upper <- max(values, na.rm = TRUE)
  lower_row <- rows[which.min(values), , drop = FALSE]
  upper_row <- rows[which.max(values), , drop = FALSE]
  paste0(
    format_standardized_value(lower),
    significance_stars(as.numeric(lower_row$pvalue[[1]])),
    "; ",
    format_standardized_value(upper),
    significance_stars(as.numeric(upper_row$pvalue[[1]]))
  )
}

format_emotion_path_range <- function(label) {
  format_rows_range(emotion_path_rows(label))
}

format_emotion_covariance_range <- function(lhs, rhs) {
  rows <- emotion_parameters[
    emotion_parameters$op == "~~" &
      ((emotion_parameters$lhs == lhs & emotion_parameters$rhs == rhs) |
        (emotion_parameters$lhs == rhs & emotion_parameters$rhs == lhs)),
    , drop = FALSE
  ]
  rows <- rows[match(names(emotion_names), rows$emotion_version), , drop = FALSE]
  rows <- rows[!is.na(rows$emotion_version), , drop = FALSE]
  assert_condition(nrow(rows) == length(emotion_names), sprintf("Expected seven covariance rows for %s and %s", lhs, rhs))
  format_rows_range(rows)
}

draw_curved_arrow <- function(
  x0, y0, x1, y1, control_x, control_y, label,
  label_x, label_y, cex = 0.52,
  line_color = "#2563A6", text_color = "#1D4F80", line_type = 2
) {
  t <- seq(0, 1, length.out = 70)
  curve_x <- (1 - t)^2 * x0 + 2 * (1 - t) * t * control_x + t^2 * x1
  curve_y <- (1 - t)^2 * y0 + 2 * (1 - t) * t * control_y + t^2 * y1
  graphics::lines(curve_x, curve_y, lwd = 1.35, lty = line_type, col = line_color)
  last <- length(curve_x)
  graphics::arrows(
    curve_x[[last - 3]], curve_y[[last - 3]], curve_x[[last]], curve_y[[last]],
    length = 0.065, angle = 22, code = 2, lwd = 1.35, col = line_color
  )
  graphics::text(
    label_x, label_y, labels = label,
    cex = cex, font = 2, col = text_color, family = figure_font_family
  )
}

draw_emotion_results <- function() {
  graphics::par(mar = c(7.6, 1.0, 3.5, 1.0), family = figure_font_family, xpd = NA)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1.26), ylim = c(0, 1))
  graphics::title(main = jp$figure3_title, cex.main = 1.22, font.main = 2, col.main = "#111827")

  draw_box(0.09, 0.68, 0.16, 0.13, jp$attachment_anxiety, node_palette$attachment, cex = 0.82)
  draw_box(0.09, 0.32, 0.16, 0.13, jp$intimacy_avoidance, node_palette$attachment, cex = 0.82)
  draw_box(0.30, 0.50, 0.15, 0.13, jp$awareness, node_palette$awareness, cex = 0.82)
  draw_box(0.49, 0.50, 0.14, 0.13, jp$action, node_palette$action, cex = 0.84)
  draw_box(0.67, 0.50, 0.15, 0.13, jp$positive_affect, node_palette$affect, cex = 0.80)

  outcome_y <- c(0.80, 0.60, 0.40, 0.20)
  outcome_labels <- c(jp$life_satisfaction, jp$vitality, jp$eudaimonic, jp$distress)
  outcome_paths <- c("c1", "c2", "c3", "c4")
  for (index in seq_along(outcome_y)) {
    draw_box(0.91, outcome_y[[index]], 0.18, 0.105, outcome_labels[[index]], node_palette$outcome, cex = 0.72)
  }

  draw_arrow(0.17, 0.66, 0.225, 0.55, format_emotion_coefficients("d1"), 0.195, 0.72, 0.46)
  draw_arrow(0.17, 0.34, 0.225, 0.45, format_emotion_coefficients("d2"), 0.195, 0.27, 0.46)
  draw_double_arrow(
    0.09, 0.61, 0.09, 0.39,
    format_emotion_covariance_range(
      "ECRGO_attachment_anxiety_mean",
      "ECRGO_avoidance_of_intimacy_mean"
    ),
    0.042, 0.50, 0.62
  )
  draw_arrow(0.375, 0.50, 0.42, 0.50, format_emotion_coefficients("a"), 0.397, 0.66, 0.46)
  draw_arrow(0.56, 0.50, 0.595, 0.50, format_emotion_coefficients("b"), 0.578, 0.66, 0.46)

  for (index in seq_along(outcome_y)) {
    draw_arrow(
      0.745, 0.50, 0.82, outcome_y[[index]],
      format_emotion_path_range(outcome_paths[[index]]),
      0.785,
      (0.50 + outcome_y[[index]]) / 2 + ifelse(index <= 2, 0.025, -0.025),
      0.62
    )
  }

  direct_specs <- list(
    aw_pa = c(0.375, 0.545, 0.595, 0.545, 0.485, 0.78, 0.485, 0.765),
    aw_ls = c(0.30, 0.565, 0.82, 0.80, 0.53, 0.92, 0.53, 0.865),
    ac_ls = c(0.49, 0.565, 0.82, 0.80, 0.67, 0.89, 0.68, 0.835),
    aw_vit = c(0.375, 0.545, 0.82, 0.60, 0.58, 0.73, 0.58, 0.70),
    ac_vit = c(0.56, 0.545, 0.82, 0.60, 0.69, 0.70, 0.69, 0.68),
    aw_eud = c(0.30, 0.435, 0.82, 0.40, 0.55, 0.27, 0.54, 0.285),
    ac_eud = c(0.49, 0.435, 0.82, 0.40, 0.66, 0.31, 0.67, 0.305),
    aw_dis = c(0.30, 0.435, 0.82, 0.20, 0.54, 0.06, 0.53, 0.085),
    ac_dis = c(0.49, 0.435, 0.82, 0.20, 0.66, 0.11, 0.67, 0.115)
  )
  original_figure3_direct_paths <- c("aw_pa", "aw_ls", "aw_vit", "ac_vit")
  for (label in names(direct_specs)) {
    significant_rows <- emotion_path_rows(label, significant_only = TRUE)
    show_nonsignificant <- label %in% original_figure3_direct_paths && nrow(significant_rows) == 0L
    if (nrow(significant_rows) == 0L && !show_nonsignificant) next
    specification <- as.numeric(direct_specs[[label]])
    path_label <- if (show_nonsignificant) {
      "NS"
    } else {
      format_emotion_coefficients(
        label, significant_only = TRUE, per_line = 3L,
        include_superscript = TRUE
      )
    }
    draw_curved_arrow(
      specification[[1]], specification[[2]], specification[[3]], specification[[4]],
      specification[[5]], specification[[6]],
      path_label,
      specification[[7]], specification[[8]], cex = 0.58,
      line_color = if (show_nonsignificant) "#9CA3AF" else "#2563A6",
      text_color = if (show_nonsignificant) "#6B7280" else "#1D4F80",
      line_type = if (show_nonsignificant) 3 else 2
    )
  }

  emotion_outcome_covariances <- list(
    c(1, 2, 0.040, "SWLS_life_satisfaction_mean", "SVS_vitality_mean", 0.000),
    c(2, 3, 0.040, "SVS_vitality_mean", "PWBS_eudaimonic_wellbeing_mean", 0.035),
    c(3, 4, 0.040, "PWBS_eudaimonic_wellbeing_mean", "K6_psychological_distress_total", 0.000),
    c(1, 3, 0.200, "SWLS_life_satisfaction_mean", "PWBS_eudaimonic_wellbeing_mean", 0.000),
    c(2, 4, 0.200, "SVS_vitality_mean", "K6_psychological_distress_total", -0.005),
    c(1, 4, 0.360, "SWLS_life_satisfaction_mean", "K6_psychological_distress_total", -0.035)
  )
  for (specification in emotion_outcome_covariances) {
    first <- as.integer(specification[[1]])
    second <- as.integer(specification[[2]])
    draw_curved_double_arrow(
      1.005,
      outcome_y[[first]],
      outcome_y[[second]],
      as.numeric(specification[[3]]),
      format_emotion_covariance_range(specification[[4]], specification[[5]]),
      cex = 0.44,
      label_offset = 0.004,
      label_y_offset = as.numeric(specification[[6]])
    )
  }

  cfi_range <- range(as.numeric(emotion_fit$cfi), na.rm = TRUE)
  rmsea_range <- range(as.numeric(emotion_fit$rmsea), na.rm = TRUE)
  graphics::mtext(
    "a = \u5E78\u798F, b = \u6E80\u8DB3, c = \u611B\u3055\u308C\u3066\u3044\u308B\u611F\u899A, d = \u5F37\u3044\u95A2\u5FC3, e = \u71B1\u610F, f = \u30A8\u30CD\u30EB\u30AE\u30C3\u30B7\u30E5, g = \u8A87\u308A\uFF08\u672C\u7D71\u5408\u56F3\u3067\u88DC\u8A18\uFF09",
    side = 1, line = 0.4, cex = 0.68, col = "#374151"
  )
  graphics::mtext(
    "\u9752\u8272\u7834\u7DDA = \u6709\u610F\u306A\u8FFD\u52A0\u76F4\u63A5\u52B9\u679C\u3001\u7070\u8272\u70B9\u7DDA = \u539F\u8457\u3067\u691C\u8A0E\u3055\u308C\u305F\u304C\u73FE\u5728\u306E\u30C7\u30FC\u30BF\u3067\u306F\u975E\u6709\u610F\uFF08NS\uFF09\u306E\u7D4C\u8DEF\u3002",
    side = 1, line = 1.45, cex = 0.68, col = "#1D4F80"
  )
  graphics::mtext(
    "\u4E3B\u8981\u7D4C\u8DEF\u306E / \u306F7\u611F\u60C5\u306E\u4FC2\u6570\u3092\u9806\u306B\u533A\u5207\u308A\u3001\u76F4\u63A5\u52B9\u679C\u306E\u4E0A\u4ED8\u304D\u6587\u5B57\u306F\u5BFE\u5FDC\u3059\u308B\u611F\u60C5\u3092\u793A\u3059\u3002",
    side = 1, line = 2.5, cex = 0.66, col = "#4B5563"
  )
  graphics::mtext(
    "\u30DD\u30B8\u30C6\u30A3\u30D6\u611F\u60C5\u4EE5\u964D\u306E\u7D4C\u8DEF\u3068\u6B8B\u5DEE\u76F8\u95A2\u306E ; \u306F7\u30E2\u30C7\u30EB\u306E\u6700\u5C0F\u5024\u3068\u6700\u5927\u5024\u3092\u793A\u3059\u3002 * p < .05; ** p < .01; *** p < .001",
    side = 1, line = 3.55, cex = 0.66, col = "#4B5563"
  )
  graphics::mtext(
    sprintf("CFI = %.3f - %.3f; RMSEA = %.3f - %.3f", cfi_range[[1]], cfi_range[[2]], rmsea_range[[1]], rmsea_range[[2]]),
    side = 1, line = 4.6, cex = 0.66, col = "#4B5563"
  )
  graphics::mtext(jp$generated_note, side = 1, line = 5.65, cex = 0.64, col = "#4B5563")
}

figure1_paths <- render_figure("Figure_1_hypothesized_model", 12, 6.5, draw_theoretical_model)
figure2_paths <- render_figure("Figure_2_primary_sem_results", 14, 6.8, draw_primary_results)
figure3_paths <- render_figure("Figure_3_emotion_specific_models", 16, 8.5, draw_emotion_results)

caption_lines <- c(
  "\u56F31\u3000\u4EEE\u8AAC\u69CB\u9020\u30E2\u30C7\u30EB\u3002",
  "ECR-GO\u306E\u898B\u6368\u3066\u3089\u308C\u4E0D\u5B89\u3068\u89AA\u5BC6\u6027\u56DE\u907F\u304CEC\u306E\u6C17\u3065\u304D\u3092\u4E88\u6E2C\u3057\u3001\u305D\u308C\u304CEC\u884C\u52D5\u3001\u30DD\u30B8\u30C6\u30A3\u30D6\u611F\u60C5\u30014\u3064\u306E\u5FC3\u7406\u7684\u7D50\u679C\u3092\u4E88\u6E2C\u3059\u308B\u3002",
  "\u77E2\u5370\u4E0A\u306E+\uFF0F-\u306F\u4EEE\u8AAC\u4E0A\u306E\u95A2\u9023\u65B9\u5411\u3092\u793A\u3059\u3002",
  "",
  "\u56F32\u3000\u4E3B\u8981\u69CB\u9020\u30E2\u30C7\u30EB\u3002",
  "\u659C\u7DDA\u306E\u524D\u5F8C\u306F\u305D\u308C\u305E\u308C\u30E2\u30C7\u30EB1\u3068\u30E2\u30C7\u30EB2\u306E\u6A19\u6E96\u5316\u4FC2\u6570\u3092\u793A\u3059\u3002\u30E2\u30C7\u30EB2\u306F\u4ED6\u306E\u611F\u60C5\u8ABF\u6574\u5C3A\u5EA6\u3092\u542B\u3080\u3002\u53CC\u65B9\u5411\u77E2\u5370\u306F\u76F8\u95A2\u307E\u305F\u306F\u6B8B\u5DEE\u76F8\u95A2\u3092\u793A\u3059\u3002",
  "",
  "\u56F33\u3000\u611F\u60C5\u5225\u69CB\u9020\u30E2\u30C7\u30EB\uFF08\u7D71\u5408\u56F3\uFF09\u3002",
  "7\u7A2E\u985E\u306E\u611F\u60C5\u5225EC\u30E2\u30C7\u30EB\u3092\u4E00\u3064\u306E\u56F3\u306B\u7D71\u5408\u3057\u305F\u3002ECR-GO\u306E\u898B\u6368\u3066\u3089\u308C\u4E0D\u5B89\u3068\u89AA\u5BC6\u6027\u56DE\u907F\u3092\u4E0A\u6D41\u306B\u7F6E\u304D\u3001\u539F\u8457\u306E\u611F\u60C5\u5225\u611F\u5EA6\u5206\u6790\u306B\u5023\u3044\u3001\u6C17\u3065\u304D\u304A\u3088\u3073\u884C\u52D5\u304B\u3089\u5F8C\u7D9A\u5909\u6570\u3078\u306E\u76F4\u63A5\u52B9\u679C\u3082\u691C\u8A0E\u3057\u305F\u3002\u9752\u8272\u306E\u7834\u7DDA\u306F\u73FE\u5728\u306E\u30C7\u30FC\u30BF\u3067\u6709\u610F\u3067\u3042\u3063\u305F\u8FFD\u52A0\u76F4\u63A5\u52B9\u679C\u3001\u7070\u8272\u306E\u70B9\u7DDA\u306F\u539F\u8457\u3067\u5831\u544A\u3055\u308C\u305F\u7A2E\u985E\u306E\u7D4C\u8DEF\u306E\u3046\u3061\u73FE\u5728\u306E\u30C7\u30FC\u30BF\u3067\u306F\u6709\u610F\u3067\u306A\u304B\u3063\u305F\u7D4C\u8DEF\uFF08NS\uFF09\u3092\u793A\u3059\u3002",
  "\u4E3B\u8981\u7D4C\u8DEF\u306E7\u3064\u306E\u4FC2\u6570\u306F a\u2013g \u306E\u56FA\u5B9A\u9806\u3067 / \u3067\u533A\u5207\u3063\u305F\u3002\u8FFD\u52A0\u76F4\u63A5\u52B9\u679C\u306E\u4FC2\u6570\u53F3\u4E0A\u306E a\u2013g \u306F\u3001\u305D\u306E\u4FC2\u6570\u304C\u3069\u306E\u611F\u60C5\u5225\u30E2\u30C7\u30EB\u306B\u5BFE\u5FDC\u3059\u308B\u304B\u3092\u793A\u3059\u3002\u539F\u8457\u3067\u6587\u5B57\u306E\u306A\u3044\u8A87\u308A\u306B\u306F\u3001\u672C\u7D71\u5408\u56F3\u3067 g \u3092\u88DC\u8A18\u3057\u305F\u3002\u30DD\u30B8\u30C6\u30A3\u30D6\u611F\u60C5\u4EE5\u964D\u306E\u7D4C\u8DEF\u3068\u7D50\u679C\u5909\u6570\u9593\u306E\u6B8B\u5DEE\u76F8\u95A2\u306B\u3064\u3044\u3066\u306F\u30017\u30E2\u30C7\u30EB\u306E\u6700\u5C0F\u5024\u3068\u6700\u5927\u5024\u3092 ; \u3067\u533A\u5207\u3063\u3066\u793A\u3059\u3002",
  "",
  "\u6709\u610F\u6C34\u6E96\uFF1A* p < .05; ** p < .01; *** p < .001\u3002",
  "\u73FE\u5728\u306E\u56F3\u306F\u73FE\u5728\u306E\u5206\u6790\u7D50\u679C\u304B\u3089\u4F5C\u6210\u3057\u305F\u3002\u6B63\u5F0F\u30C7\u30FC\u30BF\u53CE\u96C6\u5F8C\u306B\u518D\u5B9F\u884C\u3057\u3001\u8AD6\u6587\u3067\u5831\u544A\u3059\u308B\u524D\u306B\u3059\u3079\u3066\u306E\u30E2\u30C7\u30EB\u8A3A\u65AD\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u3002"
)
write_utf8_text(caption_lines, file.path(output_directory, "08_sem_figure_captions.txt"))

write_analysis_session_info(
  file.path(output_directory, "08_session_info.txt"),
  c(
    "Figures generated: hypothesized model, primary SEM, and one merged figure summarizing seven emotion-specific models.",
    "Primary coefficients are standardized estimates shown as Model 1 / Model 2.",
    sprintf("Primary SEM admissible at generation time: %s", primary_admissible),
    "Output formats: PNG (180 dpi) and PDF (vector)."
  )
)

for (path in c(figure1_paths, figure2_paths, figure3_paths)) {
  assert_condition(file.exists(path) && file.info(path)$size > 0L, sprintf("Figure output missing or empty: %s", path))
}

message("SEM Figures 1-3 generated in PNG and PDF formats.")
