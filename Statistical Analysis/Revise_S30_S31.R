# ============================================================
# Figures S30-S31: post-cleaning nutrient-record diagnostics
# Base R only. No package installation or model refitting.
# Blank Remark = unflagged; <, U, ND = working-file left censoring.
# Censored Value entries are display references, not verified lab limits.
# Boxes summarize UNFLAGGED entries only. No substitution or imputation.
# ============================================================

# Usually leave NULL and open this file in RStudio, then click Source.
# If pasting into a console, you may set the extracted package folder here.
# Example: PROJECT_DIR <- "C:/Users/ma23ch/.../S30_S31_Revised_Code"
PROJECT_DIR <- NULL

CONFIG <- list(
  sites = c("Rockledge", "Sanford", "Cow Creek", "Jacksonville"),
  site_colors = c("Rockledge" = "#0072B2", "Sanford" = "#D55E00",
                  "Cow Creek" = "#009E73", "Jacksonville" = "#CC79A7"),
  nutrients = c("TKN", "TN", "TP"),
  start_date = as.Date("1999-01-01"), end_date = as.Date("2024-12-31"),
  censor_codes = c("<", "U", "ND"),
  dpi = 600L, pointsize = 11, axis_points = 12, panel_points = 13,
  formats = c("pdf", "png", "tiff"), font_family = "sans"
)

source_directory <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) return(dirname(normalizePath(sub("^--file=", "", arg[1]))))
  paths <- lapply(sys.frames(), function(x) x$ofile)
  paths <- Filter(function(x) is.character(x) && length(x) == 1L, paths)
  if (length(paths)) return(dirname(normalizePath(paths[[length(paths)]])))
  getwd()
}

locate_project <- function(explicit = NULL) {
  candidates <- unique(c(explicit, source_directory(), getwd(),
                         file.path(getwd(), "S30_S31_Revised_Code"),
                         file.path(getwd(), "S30_S31_Revised_Code", "S30_S31_Revised_Code")))
  for (p in candidates) {
    if (file.exists(file.path(p, "inputs", "file_manifest.csv"))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  if (interactive()) {
    message("Select file_manifest.csv inside the extracted S30-S31 package's inputs folder.")
    selected <- file.choose()
    if (basename(selected) == "file_manifest.csv" && basename(dirname(selected)) == "inputs") {
      return(normalizePath(dirname(dirname(selected)), winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot find inputs/file_manifest.csv. Extract the full package and Source this R file, or set PROJECT_DIR.")
}

parse_dates <- function(x) {
  out <- as.Date(x, format = "%m/%d/%Y")
  missing <- is.na(out)
  if (any(missing)) out[missing] <- as.Date(x[missing], format = "%Y-%m-%d")
  if (anyNA(out)) stop("An invalid/unsupported nutrient date requires review")
  out
}

read_series <- function(input_dir, config = CONFIG) {
  manifest <- read.csv(file.path(input_dir, "file_manifest.csv"),
                        stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("Site", "Nutrient", "Filename", "Expected_n", "Expected_censored", "First_date", "Last_date")
  if (!all(needed %in% names(manifest))) stop("Incomplete file manifest")
  keys <- paste(manifest$Site, manifest$Nutrient, sep = "|")
  expected <- as.vector(outer(config$sites, config$nutrients, paste, sep = "|"))
  if (nrow(manifest) != 12L || anyDuplicated(keys) || !setequal(keys, expected)) {
    stop("Manifest must identify the 12 site-nutrient series exactly once")
  }
  result <- list()
  for (i in seq_len(nrow(manifest))) {
    m <- manifest[i, ]; path <- file.path(input_dir, m$Filename)
    raw <- read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
                    check.names = FALSE, na.strings = "", fileEncoding = "UTF-8-BOM")
    names(raw) <- trimws(names(raw))
    if (!all(c("Date", "Remark", "Value") %in% names(raw))) stop("Required columns missing: ", path)
    remark <- raw$Remark; remark[is.na(remark)] <- ""
    qualifier <- toupper(trimws(remark))
    if (any(!qualifier %in% c("", config$censor_codes))) stop("Unknown qualifier requires review: ", path)
    date <- parse_dates(trimws(raw$Date))
    value <- suppressWarnings(as.numeric(raw$Value))
    if (any(!is.finite(value) | value <= 0)) stop("Invalid/nonpositive entry requires review: ", path)
    if (any(date < config$start_date | date > config$end_date)) stop("Date outside documented coverage: ", path)
    if (anyDuplicated(date)) stop("Duplicate dates require review; no additional averaging: ", path)
    z <- data.frame(
      Site = m$Site, Nutrient = m$Nutrient, Date = date, Value = value,
      Censored = qualifier %in% config$censor_codes, Remark_normalized = qualifier,
      Date_raw = raw$Date, Remark_raw = remark, Value_raw = raw$Value,
      Source_file = m$Filename, Source_row = seq_len(nrow(raw)),
      Input_md5 = unname(tools::md5sum(path)), stringsAsFactors = FALSE)
    z <- z[order(z$Date), ]; rownames(z) <- NULL
    if (nrow(z) != m$Expected_n || sum(z$Censored) != m$Expected_censored) {
      stop("Counts differ from the supplied Table S4 snapshot: ", path)
    }
    if (as.character(min(z$Date)) != m$First_date || as.character(max(z$Date)) != m$Last_date) {
      stop("Record endpoints differ from the supplied snapshot: ", path)
    }
    result[[paste(m$Site, m$Nutrient, sep = "|")]] <- z
  }
  result
}

get_series <- function(records, site, nutrient) records[[paste(site, nutrient, sep = "|")]]

box_statistics <- function(z) {
  x <- z$Value[!z$Censored]
  if (!length(x)) stop("No unflagged entries; the conditional box is undefined")
  q <- unname(quantile(x, c(0.25, 0.50, 0.75), type = 7))
  spread <- q[3] - q[1]
  lo <- min(x[x >= q[1] - 1.5 * spread])
  hi <- max(x[x <= q[3] + 1.5 * spread])
  c(N_unflagged = length(x), Q1 = q[1], Median = q[2], Q3 = q[3],
    Lower_whisker = lo, Upper_whisker = hi,
    N_outside_whiskers = sum(x < lo | x > hi))
}

axis_spec <- function(records, nutrient, config = CONFIG) {
  maximum <- max(unlist(lapply(config$sites, function(site) get_series(records, site, nutrient)$Value)))
  padded <- maximum * 1.03; target <- padded / 6
  power <- 10^floor(log10(target))
  candidates <- c(1, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10)
  step <- candidates[which(candidates >= target / power)[1]] * power
  upper <- ceiling(padded / step) * step
  list(lower = -0.04 * upper, upper = upper, ticks = seq(0, upper, by = step))
}

horizontal_jitter <- function(source_rows, amplitude) {
  amplitude * (2 * ((source_rows * 0.6180339887498949) %% 1) - 1)
}

plot_theme <- function(mar, config = CONFIG) {
  par(cex = 1, mar = mar, mgp = c(2.65, 0.6, 0), tcl = -0.25,
      family = config$font_family, cex.axis = 1, cex.lab = config$axis_points / config$pointsize,
      font.axis = 1, font.lab = 1, col.axis = "black", col.lab = "black", las = 1)
}

draw_key <- function(config = CONFIG) {
  par(cex = 1, mar = rep(0, 4), family = config$font_family)
  plot.new()
  legend("center", legend = c("Unflagged entry", "Left-censored entry"),
         pch = c(16, 6), col = c("grey40", "black"), pt.cex = c(0.55, 0.65),
         pt.lwd = 0.7, cex = 1, ncol = 2, bty = "n", x.intersp = 0.6)
}

draw_s30 <- function(records, config = CONFIG) {
  layout(matrix(c(rep(1, 3), 2:13), nrow = 5, byrow = TRUE), heights = c(0.26, rep(1, 4)))
  par(oma = c(2.1, 0, 0.1, 0.1))
  draw_key(config)
  date_ticks <- as.Date(c("2000-01-01", "2012-01-01", "2024-01-01"))
  for (i in seq_along(config$sites)) for (j in seq_along(config$nutrients)) {
    site <- config$sites[i]; nutrient <- config$nutrients[j]
    site_color <- unname(config$site_colors[site])
    z <- get_series(records, site, nutrient); a <- axis_spec(records, nutrient, config)
    plot_theme(c(1.9, 4.8, 1.7, 0.5), config)
    plot(as.numeric(z$Date), z$Value, type = "n", axes = FALSE, ann = FALSE,
         xlim = as.numeric(c(config$start_date, config$end_date)),
         ylim = c(a$lower, a$upper), xaxs = "i", yaxs = "i")
    axis(1, at = as.numeric(date_ticks), labels = FALSE)
    if (i == length(config$sites)) {
      mtext(format(date_ticks, "%Y"), side = 1, line = 0.60,
            at = as.numeric(date_ticks), adj = c(0, 0.5, 1), cex = 1)
    }
    axis(2, at = a$ticks, labels = format(a$ticks, trim = TRUE, scientific = FALSE), las = 1)
    box(bty = "l")
    points(as.numeric(z$Date[!z$Censored]), z$Value[!z$Censored],
           pch = 16, cex = 0.35, col = adjustcolor(site_color, alpha.f = 0.75))
    points(as.numeric(z$Date[z$Censored]), z$Value[z$Censored], pch = 6, cex = 0.60, lwd = 0.65, col = site_color)
    mtext(bquote(.(nutrient)~(plain(mg)~plain(L)^{-1})), side = 2, line = 2.65, cex = config$axis_points / config$pointsize)
    if (j == 1L) mtext(site, side = 2, line = 3.95, cex = config$axis_points / config$pointsize)
    mtext(LETTERS[(i - 1L) * 3L + j], side = 3, line = 0.45, adj = 0,
          font = 2, cex = config$panel_points / config$pointsize)
    mtext(paste0("n=", nrow(z), "; c=", sum(z$Censored)), side = 3, line = 0.45, adj = 1, cex = 1)
  }
  mtext("Year", side = 1, outer = TRUE, line = 0.55, cex = config$axis_points / config$pointsize)
}

draw_one_box <- function(center, b, site_color) {
  segments(center, b["Lower_whisker"], center, b["Upper_whisker"], lwd = 0.9, col = site_color)
  segments(center - 0.10, b["Lower_whisker"], center + 0.10, b["Lower_whisker"], lwd = 0.9, col = site_color)
  segments(center - 0.10, b["Upper_whisker"], center + 0.10, b["Upper_whisker"], lwd = 0.9, col = site_color)
  rect(center - 0.22, b["Q1"], center + 0.22, b["Q3"],
       col = adjustcolor(site_color, alpha.f = 0.14), border = site_color, lwd = 0.95)
  segments(center - 0.22, b["Median"], center + 0.22, b["Median"], lwd = 1.2)
}

draw_s31 <- function(records, config = CONFIG) {
  layout(matrix(1:4, ncol = 1), heights = c(0.18, 1, 1, 1))
  par(oma = c(0.1, 0, 0.1, 0.1))
  draw_key(config)
  for (j in seq_along(config$nutrients)) {
    nutrient <- config$nutrients[j]; a <- axis_spec(records, nutrient, config)
    plot_theme(c(4.1, 4.8, 1.7, 0.7), config)
    plot(NA, type = "n", axes = FALSE, ann = FALSE, xlim = c(0.45, 4.55),
         ylim = c(a$lower, a$upper), xaxs = "i", yaxs = "i")
    axis(2, at = a$ticks, labels = format(a$ticks, trim = TRUE, scientific = FALSE), las = 1)
    labels <- character()
    for (i in seq_along(config$sites)) {
      site <- config$sites[i]; z <- get_series(records, site, nutrient)
      site_color <- unname(config$site_colors[site])
      b <- box_statistics(z); center <- i - 0.10
      draw_one_box(center, b, site_color)
      unflagged <- z[!z$Censored, ]; flagged <- z[z$Censored, ]
      points(center + horizontal_jitter(unflagged$Source_row, 0.16), unflagged$Value,
             pch = 16, cex = 0.35, col = adjustcolor(site_color, alpha.f = 0.50))
      points(i + 0.30 + horizontal_jitter(flagged$Source_row, 0.035), flagged$Value,
             pch = 6, cex = 0.60, lwd = 0.65, col = site_color)
      labels[i] <- paste0(site, "\nn=", nrow(z), "; c=", sum(z$Censored))
    }
    axis(1, at = 1:4, labels = labels, las = 1, cex.axis = 1)
    box(bty = "l")
    mtext(bquote(.(nutrient)~(plain(mg)~plain(L)^{-1})), side = 2, line = 2.65, cex = config$axis_points / config$pointsize)
    mtext(LETTERS[j], side = 3, line = 0.45, adj = 0, font = 2, cex = config$panel_points / config$pointsize)
  }
}

save_figure <- function(stem, draw, width, height, output_dir, config = CONFIG) {
  for (format in config$formats) {
    path <- file.path(output_dir, paste0(stem, ".", format))
    if (format == "pdf") {
      pdf(path, width = width, height = height, pointsize = config$pointsize,
           family = "Helvetica", useDingbats = FALSE, bg = "white", onefile = FALSE, colormodel = "srgb")
    } else {
      args <- list(filename = path, width = width, height = height, units = "in",
                   res = config$dpi, pointsize = config$pointsize, bg = "white")
      if (capabilities("cairo")) args$type <- "cairo"
      if (format == "png") {
        do.call(png, args)
      } else if (format == "tiff") {
        args$compression <- "lzw"; do.call(tiff, args)
      } else stop("Unknown output format: ", format)
    }
    tryCatch(draw(), finally = dev.off())
    if (!file.exists(path) || file.info(path)$size <= 0) stop("Figure was not created: ", path)
  }
}

write_outputs <- function(records, output_dir, config = CONFIG) {
  all_records <- do.call(rbind, records)
  rownames(all_records) <- NULL
  write.csv(all_records, file.path(output_dir, "plotting_records.csv"), row.names = FALSE, na = "")
  audits <- list()
  for (site in config$sites) for (nutrient in config$nutrients) {
    z <- get_series(records, site, nutrient); b <- box_statistics(z)
    audits[[length(audits) + 1L]] <- data.frame(
      Site = site, Nutrient = nutrient, N_total = nrow(z), N_censored = sum(z$Censored),
      Percent_censored = 100 * mean(z$Censored), First_date = as.character(min(z$Date)),
      Last_date = as.character(max(z$Date)), as.list(b), check.names = FALSE)
  }
  audits <- do.call(rbind, audits)
  write.csv(audits, file.path(output_dir, "series_and_boxplot_audit.csv"), row.names = FALSE)
  # Independent reference comparison, when the supplied reference file exists.
  reference_file <- file.path(dirname(output_dir), "reference", "series_and_boxplot_audit.csv")
  if (file.exists(reference_file)) {
    expected <- read.csv(reference_file, check.names = FALSE)
    key <- function(x) paste(x$Site, x$Nutrient, sep = "|")
    expected <- expected[match(key(audits), key(expected)), ]
    for (v in c("N_total", "N_censored", "N_unflagged", "Q1", "Median", "Q3", "Lower_whisker", "Upper_whisker")) {
      if (max(abs(audits[[v]] - expected[[v]])) > 1e-10) stop("Reference mismatch: ", v)
    }
  }
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "R_sessionInfo.txt"))
  invisible(audits)
}

main <- function(project_dir = PROJECT_DIR) {
  root <- locate_project(project_dir)
  output_dir <- file.path(root, "outputs")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  records <- read_series(file.path(root, "inputs"), CONFIG)
  audit <- write_outputs(records, output_dir, CONFIG)
  save_figure("Figure_S30_Revised", function() draw_s30(records, CONFIG), 7.2, 8.8, output_dir, CONFIG)
  save_figure("Figure_S31_Revised", function() draw_s31(records, CONFIG), 7.2, 8.2, output_dir, CONFIG)
  caption_file <- file.path(root, "Captions_and_manuscript_text.txt")
  if (file.exists(caption_file)) file.copy(caption_file, output_dir, overwrite = TRUE)
  message("Completed Figures S30-S31: ", normalizePath(output_dir, winslash = "/"))
  message("Records: ", sum(audit$N_total), "; left-censored working-file entries: ", sum(audit$N_censored))
  invisible(records)
}

if (!isTRUE(getOption("s30.s31.skip_run", FALSE))) main()
