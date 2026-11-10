# Figures S23-S29 and Tables S1-S3: manuscript-aligned revision
# Authoritative nutrient inputs: percentage changes displayed in main Tables 2-4.
# Authoritative land-cover inputs: percentage changes displayed in main Table 5.
# Base R only; no package installation, inferential tests, or WRTDS model refits.
# See README.md for use and prepare_inputs_from_manuscript.py for extraction.

script_directory <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)))
  }
  frames <- sys.frames()
  ofiles <- lapply(frames, function(x) x$ofile)
  ofiles <- Filter(function(x) is.character(x) && length(x) == 1L, ofiles)
  if (length(ofiles)) return(dirname(normalizePath(ofiles[[length(ofiles)]], mustWork = TRUE)))
  getwd()
}

SCRIPT_DIR <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Statistical Analysis/S23_S29_S1_S3_Revised_Code/S23_S29_S1_S3_Revised_Code"
CONFIG <- list(
  input_dir = file.path(SCRIPT_DIR, "inputs"),
  output_dir = file.path(SCRIPT_DIR, "outputs"),
  sites = c("Rockledge", "Sanford", "Cow Creek", "Jacksonville"),
  nutrients = c("TKN", "TN", "TP"),
  from_wy = c(Rockledge = 2004L, Sanford = 2004L, "Cow Creek" = 2004L, Jacksonville = 2004L),
  to_wy = c(Rockledge = 2019L, Sanford = 2024L, "Cow Creek" = 2024L, Jacksonville = 2024L),
  land_variables = c("Water", "Developed", "Barren Land", "Forest", "Agriculture", "Wetlands"),
  pca_land_variables = c("Water", "Developed", "Forest", "Wetlands"),
  land_labels = c(Water = "Open water", Developed = "Developed", "Barren Land" = "Barren land",
                  Forest = "Forest", Agriculture = "Agriculture composite", Wetlands = "Wetlands"),
  formats = c("pdf", "png", "tiff"),
  dpi = 600L,
  font_family = "sans",
  pointsize = 10.5
)

read_input <- function(path, required) {
  if (!file.exists(path)) stop("Missing input: ", path, call. = FALSE)
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  absent <- setdiff(required, names(x))
  if (length(absent)) stop("Missing columns in ", basename(path), ": ", paste(absent, collapse = ", "))
  x
}

as_numeric_checked <- function(x, label, allow_missing = FALSE) {
  original_missing <- is.na(x) | trimws(as.character(x)) == ""
  y <- suppressWarnings(as.numeric(x))
  if (any(!original_missing & !is.finite(y))) stop("Non-numerical or nonfinite value in ", label)
  if (!allow_missing && any(!is.finite(y))) stop("Missing required value in ", label)
  y
}

correlation_rows <- function(x, nutrient, config = CONFIG) {
  targets <- c("Concentration_change_pct", "Flux_change_pct")
  target_labels <- c(paste(nutrient, "concentration change (%)"),
                     paste(nutrient, "exploratory flux change (%)"))
  rows <- list()
  for (j in seq_along(targets)) for (lc in config$land_variables) {
    keep <- is.finite(x[[lc]]) & is.finite(x[[targets[j]]])
    xx <- x[[lc]][keep]; yy <- x[[targets[j]]][keep]
    nn <- length(xx)
    defined <- nn >= 3L && sd(xx) > 0 && sd(yy) > 0
    rr <- if (defined) unname(cor(xx, yy, method = "pearson")) else NA_real_
    rows[[length(rows) + 1L]] <- data.frame(
      LandCover = unname(config$land_labels[lc]), Target = target_labels[j],
      r = rr, N = nn, Variable = lc, TargetKey = targets[j],
      stringsAsFactors = FALSE, check.names = FALSE)
  }
  ans <- do.call(rbind, rows)
  # Keep concentration first; sort within target by decreasing absolute r.
  ans <- ans[order(match(ans$TargetKey, targets), -abs(ans$r), match(ans$Variable, config$land_variables)), ]
  rownames(ans) <- NULL
  ans
}

fit_descriptive_pca <- function(x, config = CONFIG) {
  variable_names <- c(config$pca_land_variables, "Concentration_change_pct", "Flux_change_pct")
  mat <- as.matrix(x[, variable_names, drop = FALSE])
  storage.mode(mat) <- "double"
  rownames(mat) <- x$Site
  if (any(!is.finite(mat))) stop("PCA requires all four sites and the six documented variables; no imputation is used")
  if (any(apply(mat, 2, sd) <= 0)) stop("A PCA variable is constant")
  pc <- prcomp(mat, center = TRUE, scale. = TRUE,
               rank. = min(nrow(mat) - 1L, ncol(mat)))
  # Centering four spatial units gives at most three nonzero components.
  keep <- seq_len(ncol(pc$rotation))
  keep <- keep[pc$sdev[keep] > pc$sdev[1] * sqrt(.Machine$double.eps)]
  if (length(keep) < 2L) stop("Fewer than two nonzero PCs; a two-axis biplot is not defined")
  rotation <- pc$rotation[, keep, drop = FALSE]
  scores <- pc$x[, keep, drop = FALSE]
  sdev <- pc$sdev[keep]
  # Reproducible sign convention; PCA axis signs otherwise have no meaning.
  for (j in seq_along(keep)) {
    anchor <- which.max(abs(rotation[, j]))
    if (rotation[anchor, j] < 0) {
      rotation[, j] <- -rotation[, j]
      scores[, j] <- -scores[, j]
    }
  }
  colnames(rotation) <- colnames(scores) <- paste0("PC", seq_along(keep))
  names(sdev) <- colnames(rotation)
  eigenvalues <- sdev^2
  variable_correlations <- sweep(rotation, 2, sdev, "*")
  standardized_scores <- sweep(scores, 2, sdev, "/")
  list(raw = mat, center = pc$center, scale = pc$scale,
       standardized = scale(mat, center = pc$center, scale = pc$scale),
       rotation = rotation, scores = scores, sdev = sdev,
       variable_correlations = variable_correlations,
       standardized_scores = standardized_scores,
       eigenvalues = eigenvalues,
       variance_pct = 100 * eigenvalues / sum(eigenvalues), rank = length(keep))
}

load_analysis <- function(input_dir = CONFIG$input_dir, config = CONFIG) {
  nutrient_file <- file.path(input_dir, "nutrient_changes_main_tables_2_4.csv")
  land_file <- file.path(input_dir, "land_cover_change_1999_2024.csv")
  land <- read_input(land_file, c("Site", config$land_variables))
  changes <- read_input(nutrient_file,
                       c("Site", "Constituent", "From_WY", "To_WY",
                         "Concentration_change_pct", "Flux_change_pct", "Source_table"))
  land$Site <- trimws(land$Site)
  changes$Site <- trimws(changes$Site)
  changes$Constituent <- trimws(changes$Constituent)
  if (anyDuplicated(land$Site) || !setequal(land$Site, config$sites)) {
    stop("Land-cover input must contain the four focal sites exactly once; exclude the full watershed")
  }
  land <- land[match(config$sites, land$Site), , drop = FALSE]
  for (v in config$land_variables) land[[v]] <- as_numeric_checked(land[[v]], v, TRUE)
  for (v in c("From_WY", "To_WY", "Concentration_change_pct", "Flux_change_pct")) {
    changes[[v]] <- as_numeric_checked(changes[[v]], v)
  }
  key <- paste(changes$Site, changes$Constituent, sep = "|")
  expected <- as.vector(outer(config$sites, config$nutrients, paste, sep = "|"))
  if (anyDuplicated(key) || !setequal(key, expected)) {
    stop("Nutrient input must contain exactly one principal comparison per site and constituent")
  }
  if (any(changes$From_WY != unname(config$from_wy[changes$Site])) ||
      any(changes$To_WY != unname(config$to_wy[changes$Site]))) {
    stop("Comparison windows differ from main Tables 2-4: Rockledge WY2004-2019; other sites WY2004-2024")
  }
  expected_table <- setNames(paste("Table", 2:4), config$nutrients)
  if (any(changes$Source_table != unname(expected_table[changes$Constituent]))) {
    stop("Constituent/table provenance mismatch")
  }
  data <- tables <- pcas <- list()
  for (nutrient in config$nutrients) {
    z <- changes[changes$Constituent == nutrient, , drop = FALSE]
    z <- z[match(config$sites, z$Site), , drop = FALSE]
    z <- cbind(land, z[, setdiff(names(z), "Site"), drop = FALSE])
    data[[nutrient]] <- z
    tables[[nutrient]] <- correlation_rows(z, nutrient, config)
    pcas[[nutrient]] <- fit_descriptive_pca(z, config)
  }
  ans <- list(land = land, changes = changes, data = data, tables = tables, pcas = pcas,
              input_files = c(nutrient_file, land_file))
  validate_analysis(ans, config)
  ans
}

validate_analysis <- function(a, config = CONFIG) {
  for (nutrient in config$nutrients) {
    tab <- a$tables[[nutrient]]; pc <- a$pcas[[nutrient]]
    stopifnot(nrow(tab) == 12L, all(tab$N %in% 3:4),
              all(abs(tab$r[is.finite(tab$r)]) <= 1 + 1e-12),
              identical(rownames(pc$scores), config$sites),
              pc$rank <= length(config$sites) - 1L,
              abs(sum(pc$variance_pct) - 100) < 1e-9,
              max(abs(colMeans(pc$standardized))) < 1e-10,
              max(abs(apply(pc$standardized, 2, sd) - 1)) < 1e-10,
              max(abs(pc$variable_correlations)) <= 1 + 1e-10,
              max(abs(pc$scores %*% t(pc$rotation) - pc$standardized)) < 1e-9)
    # A variable-PC correlation is not the unscaled eigenvector coefficient.
    stopifnot(max(abs(cor(pc$raw, pc$scores) - pc$variable_correlations)) < 1e-10)
  }
  invisible(TRUE)
}

write_csv <- function(x, path) {
  old <- options(digits = 17)
  on.exit(options(old), add = TRUE)
  write.table(x, path, sep = ",", row.names = FALSE, col.names = TRUE,
              quote = TRUE, na = "", fileEncoding = "UTF-8")
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

shared_table_note <- function() {
  paste("Land-cover percentage changes compare 1999 and 2024.",
        "Concentration and flux changes are WRTDS flow-normalized estimates.",
        "Nutrient comparisons are WY2004-WY2019 at Rockledge and WY2004-WY2024 elsewhere.",
        "N denotes matched spatial units, not nutrient observations.",
        "Jacksonville barren land and agriculture-composite changes are unavailable; those pairs use N = 3.",
        "Other pairs use N = 4. Missing values are not replaced with zero.",
        "Coefficients describe supplied point estimates; model uncertainty is not propagated.",
        "Flux metrics depend on assigned gauges and are not validated HUC-12 exports.",
        "No p-values, significance symbols, or causal interpretations are provided.")
}

write_correlation_table <- function(tab, nutrient, number, output_dir) {
  display <- tab[, c("LandCover", "Target", "r", "N"), drop = FALSE]
  names(display)[1] <- "Land cover"
  stem <- paste0("Table_S", number, "_", nutrient, "_Pearson_r")
  write_csv(display, file.path(output_dir, paste0(stem, ".csv")))
  pretty <- display
  pretty$r <- ifelse(is.finite(pretty$r), sprintf("%.3f", pretty$r), "NA")
  rows <- apply(pretty, 1, function(x) paste0("<tr><td>", paste(html_escape(x), collapse = "</td><td>"), "</td></tr>"))
  title <- paste0("Table S", number, ". Descriptive land-cover associations with ", nutrient, " percentage changes")
  content <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<style>body{font:11pt 'Times New Roman',serif;color:#000;background:#fff;max-width:900px;margin:30px auto}table{border-collapse:collapse;width:100%}th,td{padding:5px 9px;text-align:left}thead{border-top:2px solid #000;border-bottom:1px solid #000}tbody{border-bottom:2px solid #000}td:nth-child(3),td:nth-child(4){text-align:right}p{line-height:1.3}</style></head><body>",
    paste0("<p><strong>", html_escape(title), "</strong></p><table><thead><tr><th>",
           paste(html_escape(names(pretty)), collapse = "</th><th>"), "</th></tr></thead><tbody>"),
    rows, "</tbody></table>", paste0("<p>", html_escape(shared_table_note()), "</p></body></html>"))
  writeLines(content, file.path(output_dir, paste0(stem, ".html")), useBytes = TRUE)
}

export_pca <- function(pc, nutrient, output_dir) {
  eigen <- data.frame(Component = names(pc$sdev), Eigenvalue = pc$eigenvalues,
                      Variance_pct = pc$variance_pct, Cumulative_pct = cumsum(pc$variance_pct))
  write_csv(eigen, file.path(output_dir, paste0(nutrient, "_PCA_Eigenvalues.csv")))
  for (key in c("rotation", "variable_correlations")) {
    z <- data.frame(Variable = rownames(pc[[key]]), pc[[key]], row.names = NULL, check.names = FALSE)
    write_csv(z, file.path(output_dir, paste0(nutrient, "_PCA_", key, ".csv")))
  }
  for (key in c("scores", "standardized_scores")) {
    z <- data.frame(Site = rownames(pc[[key]]), pc[[key]], row.names = NULL, check.names = FALSE)
    write_csv(z, file.path(output_dir, paste0(nutrient, "_PCA_", key, ".csv")))
  }
  write_csv(data.frame(Variable = names(pc$center), Center = pc$center, Sample_SD = pc$scale),
            file.path(output_dir, paste0(nutrient, "_PCA_Standardization.csv")))
}

figure_device <- function(path, format, width, height, config = CONFIG) {
  if (format == "pdf") {
    pdf(path, width = width, height = height, pointsize = config$pointsize,
        family = "Helvetica", bg = "white", useDingbats = FALSE, onefile = FALSE)
  } else {
    args <- list(filename = path, width = width, height = height, units = "in",
                 res = config$dpi, pointsize = config$pointsize, bg = "white")
    if (capabilities("cairo")) args$type <- "cairo"
    if (format == "png") {
      do.call(png, args)
    } else if (format == "tiff") {
      args$compression <- "lzw"
      do.call(tiff, args)
    } else {
      stop("Unsupported figure format: ", format)
    }
  }
}

save_figure <- function(stem, draw, width, height, output_dir, config = CONFIG) {
  paths <- character()
  for (format in config$formats) {
    path <- file.path(output_dir, paste0(stem, ".", format))
    figure_device(path, format, width, height, config)
    tryCatch(draw(), finally = dev.off())
    if (!file.exists(path) || file.info(path)$size <= 0) stop("Figure was not written: ", path)
    paths <- c(paths, path)
  }
  paths
}

spread_arrow_labels <- function(tips) {
  theta <- atan2(tips[, 2], tips[, 1])
  # Keep near-collinear concentration/flux labels separate without rescaling arrows.
  for (iteration in 1:160) for (i in seq_len(length(theta) - 1L)) for (j in (i + 1L):length(theta)) {
    difference <- atan2(sin(theta[j] - theta[i]), cos(theta[j] - theta[i]))
    if (abs(difference) < 0.28) {
      direction <- if (difference < 0) -1 else 1
      delta <- (0.28 - abs(difference)) / 2
      theta[i] <- theta[i] - direction * delta
      theta[j] <- theta[j] + direction * delta
    }
  }
  cbind(1.70 * cos(theta), 1.70 * sin(theta))
}

draw_biplot_panel <- function(pc, nutrient, panel, config = CONFIG) {
  # Constituents and interpretation belong in the manuscript caption.
  par(cex = 1, mar = c(3.6, 3.8, 0.9, 0.7), mgp = c(2.1, 0.6, 0), tcl = -0.25,
      family = config$font_family, col.axis = "black", col.lab = "black", las = 1)
  plot(NA, xlim = c(-2.12, 2.12), ylim = c(-2.12, 2.12), asp = 1,
       xlab = sprintf("PC1 (%.1f%%)", pc$variance_pct[1]),
       ylab = sprintf("PC2 (%.1f%%)", pc$variance_pct[2]),
       cex.lab = 0.94, cex.axis = 0.83, bty = "l", xaxs = "i", yaxs = "i")
  abline(h = 0, v = 0, col = "grey65", lty = 3, lwd = 0.7)
  theta <- seq(0, 2 * pi, length.out = 361)
  lines(cos(theta), sin(theta), col = "grey70", lwd = 0.7)
  tips <- pc$variable_correlations[, 1:2, drop = FALSE]
  lab <- spread_arrow_labels(tips)
  segments(tips[, 1], tips[, 2], lab[, 1] * 0.91, lab[, 2] * 0.91,
           col = "grey50", lty = 3, lwd = 0.6)
  arrows(0, 0, tips[, 1], tips[, 2], length = 0.055, angle = 22, lwd = 0.9, col = "black")
  text(lab[, 1], lab[, 2], labels = c("W", "D", "F", "WL", "C", "Fx"), cex = 0.91)
  points(pc$standardized_scores[, 1], pc$standardized_scores[, 2],
         pch = c(21, 22, 24, 23), bg = "white", col = "black", lwd = 1.1, cex = 1.35)
  mtext(panel, side = 3, line = 0.10, adj = 0, font = 2, cex = 1.02)
}

draw_biplot_key <- function(config = CONFIG) {
  par(cex = 1, mar = c(0.7, 1.0, 0.9, 0.5), family = config$font_family)
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  text(0, 0.99, "D", adj = c(0, 1), font = 2, cex = 1.02)
  legend(x = 0, y = 0.91, legend = config$sites, pch = c(21, 22, 24, 23),
         pt.bg = "white", col = "black", pt.cex = 1.25, cex = 0.92,
         y.intersp = 1.10, bty = "n", x.intersp = 0.85)
  key <- c("W   Open water", "D   Developed", "F   Forest", "WL  Wetlands",
           "C   Concentration change (%)", "Fx  Exploratory flux change (%)")
  text(0.02, seq(0.48, 0.13, length.out = length(key)), key, adj = c(0, 0.5), cex = 0.88)
}

draw_combined_biplot <- function(a, config = CONFIG) {
  layout(matrix(1:4, nrow = 2, byrow = TRUE))
  par(oma = c(0.2, 0.3, 0.2, 0.1))
  for (i in seq_along(config$nutrients)) {
    n <- config$nutrients[i]
    draw_biplot_panel(a$pcas[[n]], n, LETTERS[i], config)
  }
  draw_biplot_key(config)
}

draw_scree <- function(pc, nutrient, config = CONFIG) {
  # No figure title, subtitle, or methodological note inside the artwork.
  par(cex = 1, mar = c(3.8, 4.0, 0.8, 0.8), mgp = c(2.3, 0.65, 0), tcl = -0.25,
      family = config$font_family, las = 1)
  pos <- barplot(pc$variance_pct, names.arg = names(pc$sdev), col = "grey75", border = "black",
                 ylim = c(0, min(100, max(pc$variance_pct) * 1.22)),
                 ylab = "Variance explained (%)", xlab = "Principal component",
                 cex.names = 0.95, cex.axis = 0.90, cex.lab = 0.98, axes = FALSE)
  axis(2, las = 1, cex.axis = 0.90); box(bty = "l")
  text(pos, pc$variance_pct, sprintf("%.1f%%", pc$variance_pct), pos = 3, offset = 0.35, cex = 0.96)
}

draw_correlation_panels <- function(tab, nutrient, config = CONFIG) {
  layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(1, 0.20))
  par(oma = c(0.2, 0.1, 0.2, 0.1))
  y <- rev(seq_along(config$land_variables))
  for (j in 1:2) {
    target <- c("Concentration_change_pct", "Flux_change_pct")[j]
    z <- tab[tab$TargetKey == target, , drop = FALSE]
    z <- z[match(config$land_variables, z$Variable), , drop = FALSE]
    par(cex = 1, mar = c(0.25, 7.1, 0.9, 0.3), family = config$font_family, xpd = FALSE)
    plot.new(); plot.window(xlim = c(0, 1), ylim = c(0.4, length(y) + 0.6), xaxs = "i", yaxs = "i")
    for (i in seq_along(y)) {
      rr <- z$r[i]
      shade <- if (is.finite(rr)) (rr + 1) / 2 else 1
      rect(0, y[i] - 0.45, 1, y[i] + 0.45, col = gray(max(0, min(1, shade))), border = "black", lwd = 0.65)
      ink <- if (shade < 0.47) "white" else "black"
      text(0.5, y[i] + 0.13, if (is.finite(rr)) sprintf("%+.3f", rr) else "NA", col = ink, cex = 1.02)
      text(0.5, y[i] - 0.17, paste0("N = ", z$N[i]), col = ink, cex = 0.78)
    }
    plot_labels <- unname(config$land_labels[config$land_variables])
    plot_labels[config$land_variables == "Agriculture"] <- "Agriculture\ncomposite"
    axis(2, at = y, labels = plot_labels,
         tick = FALSE, las = 1, cex.axis = 0.88, line = -0.25)
    mtext(LETTERS[j], side = 3, line = 0.10, adj = 0, font = 2, cex = 1.02)
  }
  par(cex = 1, mar = c(1.6, 3.3, 0.15, 3.3), family = config$font_family)
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  breaks <- seq(0, 1, length.out = 257)
  rect(breaks[-257], 0.20, breaks[-1], 0.65, col = gray((breaks[-257] + breaks[-1]) / 2), border = NA)
  rect(0, 0.20, 1, 0.65, border = "black", lwd = 0.6)
  axis(1, at = c(0, 0.5, 1), labels = c("-1", "0", "+1"), pos = 0.20, tcl = -0.25, cex.axis = 0.82)
  text(0.5, 0.90, "Pearson r", cex = 0.85)
}

write_captions <- function(output_dir) {
  shared <- paste("Land-cover variables are percentage changes between 1999 and 2024.",
                  "Concentration and flux changes are WRTDS flow-normalized estimates.",
                  "Nutrient comparisons are WY2004-WY2019 at Rockledge and WY2004-WY2024 at Sanford, Cow Creek, and Jacksonville.",
                  "All outputs are descriptive; flux metrics are conditional on assigned gauges, not validated HUC-12 exports.")
  captions <- c(
    "Shared coverage and interpretation note for Figures S23-S29 and Tables S1-S3:", shared, "",
    paste("Figure S23. Descriptive PCA for (A) TKN, (B) TN, and (C) TP at four focal sites; (D) symbol and variable key.",
          "Each PCA uses open water, developed cover, forest, wetlands, concentration change, and exploratory flux change,",
          "centered and divided by their sample standard deviations. Points show PC scores divided by the corresponding PC standard deviation;",
          "arrows show variable-PC correlations. The circle has radius one and is not a confidence region.",
          "Dotted connectors link arrow tips to separated labels without changing arrow magnitudes.",
          "Axis labels report variance explained. Barren land and agriculture composite are excluded from PCA because their Jacksonville values are unavailable.",
          "PCA axis signs use a reproducible convention and have no intrinsic directional meaning."), "")
  for (i in 1:3) {
    nutrient <- c("TKN", "TN", "TP")[i]
    scree <- c(24, 26, 28)[i]; heat <- c(25, 27, 29)[i]
    captions <- c(captions,
      paste0("Figure S", scree, ". Scree plot for the ", nutrient,
        " PCA in Figure S23. Bars and labels show percentage variance explained by each nonzero principal component. Four centered sites support at most three nonzero components. Six variables were standardized; components are descriptive, not a validated latent structure."), "",
      paste0("Figure S", heat, ". Descriptive Pearson correlations between endpoint land-cover percentage changes and ", nutrient,
        " (A) flow-normalized concentration and (B) exploratory flow-normalized flux percentage changes. Each cell displays r and the number of matched spatial units, N. Both panels use the same signed grayscale from -1 (black) to +1 (white). Missing Jacksonville barren-land and agriculture-composite values are omitted pairwise, not replaced with zero. No inferential significance is implied."), "",
      paste0("Table S", i, ". Descriptive Pearson correlation coefficients between endpoint land-cover percentage changes and ", nutrient,
        " concentration and exploratory flux percentage changes."), shared_table_note(), "")
  }
  writeLines(captions, file.path(output_dir, "Captions_and_shared_notes.txt"), useBytes = TRUE)
}

main <- function(input_dir = CONFIG$input_dir, output_dir = CONFIG$output_dir, config = CONFIG) {
  input_dir <- normalizePath(input_dir, mustWork = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  if (identical(input_dir, output_dir)) stop("Input and output folders must differ")
  a <- load_analysis(input_dir, config)
  table_dir <- file.path(output_dir, "tables")
  data_dir <- file.path(output_dir, "data_and_pca")
  figure_dir <- file.path(output_dir, "figures")
  for (d in c(table_dir, data_dir, figure_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(config$nutrients)) {
    nutrient <- config$nutrients[i]
    write_correlation_table(a$tables[[nutrient]], nutrient, i, table_dir)
    write_csv(a$data[[nutrient]], file.path(data_dir, paste0(nutrient, "_AnalysisInputs.csv")))
    export_pca(a$pcas[[nutrient]], nutrient, data_dir)
  }
  figures <- save_figure("Figure_S23_Combined_PCA", function() draw_combined_biplot(a, config),
                         7.2, 6.8, figure_dir, config)
  for (i in seq_along(config$nutrients)) {
    nutrient <- config$nutrients[i]
    scree <- c(24, 26, 28)[i]; heat <- c(25, 27, 29)[i]
    figures <- c(figures,
      save_figure(paste0("Figure_S", scree, "_", nutrient, "_Scree"),
                  function() draw_scree(a$pcas[[nutrient]], nutrient, config),
                  4.8, 3.3, figure_dir, config),
      save_figure(paste0("Figure_S", heat, "_", nutrient, "_Correlations"),
                  function() draw_correlation_panels(a$tables[[nutrient]], nutrient, config),
                  7.2, 4.7, figure_dir, config))
  }
  write_captions(output_dir)
  write_csv(data.frame(File = basename(a$input_files), MD5 = unname(tools::md5sum(a$input_files))),
            file.path(output_dir, "Input_checksums.csv"))
  manifest <- file.path(input_dir, "source_manifest.json")
  if (file.exists(manifest)) file.copy(manifest, file.path(output_dir, "Source_manuscript_manifest.json"), overwrite = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "R_sessionInfo.txt"))
  saveRDS(a, file.path(output_dir, "Descriptive_analysis_objects.rds"))
  writeLines(c("Analysis validated: four sites; 12 nutrient input rows; 12 correlation rows per nutrient.",
               "N = 3 for barren land/agriculture composite; N = 4 for the other land-cover classes.",
               "PCA uses six variables and at most three nonzero components; centering, scaling, reconstruction, and variable correlations checked.",
               "The nutrient inputs are displayed main-table point estimates; their uncertainty is not propagated here.",
               "This script does not refit WRTDS or validate source-station assignments or discharge representativeness."),
             file.path(output_dir, "Run_notes.txt"))
  message("Completed Figures S23-S29 and Tables S1-S3: ", output_dir)
  invisible(a)
}

if (!isTRUE(getOption("supplement.revision.skip_run", FALSE))) {
  args <- commandArgs(trailingOnly = TRUE)
  # Arguments are used only in Rscript; source() uses the configured folders above.
  is_rscript <- any(grepl("^--file=", commandArgs(trailingOnly = FALSE)))
  if (is_rscript && length(args)) {
    if (length(args) > 2L) stop("Usage: Rscript Revise_S23_S29_S1_S3.R [input_folder] [output_folder]")
    CONFIG$input_dir <- args[1]
    if (length(args) == 2L) CONFIG$output_dir <- args[2]
  }
  main(CONFIG$input_dir, CONFIG$output_dir, CONFIG)
}

