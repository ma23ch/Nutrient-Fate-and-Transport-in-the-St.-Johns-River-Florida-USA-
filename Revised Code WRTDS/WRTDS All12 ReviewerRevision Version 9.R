# ============================================================================
# ST. JOHNS RIVER - REPRODUCIBLE EGRET / WRTDS REVIEWER REVISION (V6)
# Manuscript ECOLIND-62759
# Makes Table 1 (model diagnostics), Tables 2-4 (TKN, TN, TP changes),
# Figures 5-7 (redesigned four sites x annual concentration/flux, 600-dpi TIFF + PDF).
# Can ALSO calculate genuine EGRETci 95% bands and selected change CIs.
#
# IMPORTANT: RESULTS ARE EXPLORATORY UNTIL THE ORIGINAL SIGNED DISCHARGE,
# NONDETECT METHODS, SOURCE STATION MATCHES, AND 2024 ENDPOINT SUPPORT ARE
# VALIDATED. WRTDStidal is a separate model and is NOT reproduced here.
#
# Use the installed EGRET/R versions; base R + EGRET, EGRETci optional.
# Source data are NEVER overwritten.
# ============================================================================

suppressPackageStartupMessages(library(EGRET))

# ---------------------------- USER SETTINGS ---------------------------------
ROOT <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS"
FLOW_DIR <- file.path(ROOT, "Discharge")
WQ_DIR   <- file.path(ROOT, "Water Quality")
# NEVER overwrite a potentially open CSV/Excel table from earlier runs.
# Locked Table_4_TP...csv in your old folder caused "Permission denied".
# A new time-stamped result directory avoids that collision.
OUTPUT_PARENT <- file.path(ROOT, "Reviewer_Revision_WRTDS")
dir.create(OUTPUT_PARENT, recursive = TRUE, showWarnings = FALSE)
RUN_TAG <- format(Sys.time(), "%Y%m%d_%H%M%S")
OUT <- file.path(OUTPUT_PARENT, paste0("Rerun_", RUN_TAG))
suffix <- 0L
while (file.exists(OUT)) {
  suffix <- suffix + 1L
  OUT <- file.path(OUTPUT_PARENT,
                   paste0("Rerun_", RUN_TAG, "_", suffix))
}
if (!dir.create(OUT, recursive = TRUE, showWarnings = FALSE)) {
  stop("Unable to create writable output directory: ", OUT)
}
write_probe <- file.path(OUT, ".write_permission_test")
tryCatch(writeLines("ok", write_probe),
  error = function(e) stop("Output folder is not writable: ", conditionMessage(e)))
unlink(write_probe)

# Old 12-model outputs can be REUSED without refitting when rebuilding figures.
# FALSE = clean rerun, suitable for diagnosing baseline reproducibility.
# TRUE  = reuse saved EGRET models, clearly labeled as reused in report.
USE_SAVED_MODELS <- FALSE
SAVED_MODELS_DIR <- file.path(OUTPUT_PARENT, "Models")


# Files supplied by user store discharge in cubic feet per second (cfs).
INPUT_Q_UNIT <- 1L  # 1 = cfs, 2 = m^3/s; EGRET internal Q always m^3/s
PA_START <- 10L      # Water year: October through September, NOT calendar year
PA_LONG <- 12L
# Keep the submitted comparison years for AUDIT, not unquestioned publication.
# Inspect endpoint support below. Set 2023 only after deciding to revise all
# results consistently, then document which water years lack actual samples.
YEAR_POINTS <- c(2004L, 2009L, 2014L, 2019L, 2024L)
FIG_START <- 2000L
FIG_END   <- 2024L
DPI <- 600L

# These inputs have negative signed discharges converted to zero.  EGRET's
# documented adjust=TRUE adds a positive constant for the log transform.
# This is a modeling scenario, NOT physical reconstruction of reverse flow.
ZERO_ADJUST <- TRUE

# Run all 12 WRTDS models and export basic tables and 600-dpi figures.
RUN_MODELS <- TRUE # Script always generates outputs for available model fits

# CI steps are computationally EXPENSIVE. Set TRUE after first audit and after
# input/hydrologic validation. Neither routine invents intervals.
# Genuine annual 95% bands -> Figure 5/6/7 bands via EGRETci::ciCalculations.
RUN_CI_BANDS <- FALSE
# Genuine 95% change intervals for key windows -> EGRETci::runPairsBoot.
RUN_CI_CHANGES <- FALSE
CI_N_BOOT <- 100L       # >=100 for meaningful bootstrap intervals
CI_BLOCK_LENGTH <- 200L
# Reviewer requests uncertainty for EVERY principal change; all 10 spans:
CI_WINDOWS <- combn(YEAR_POINTS, 2, simplify = FALSE)
# To test only key spans first (faster):
# CI_WINDOWS <- list(c(2004L, 2024L), c(2019L, 2024L))
CI_SEED <- 494817L

# This flag documents whether you have personally validated signed-flow
# provenance and station/gauge pairing. It only controls READ-ME wording.
HYDROLOGY_VALIDATED <- FALSE

# All 12 combinations: use the exact filenames supplied in your question.
config <- data.frame(
  site = rep(c("Rockledge", "Sanford", "Cow Creek", "Jacksonville"), each = 3),
  analyte = rep(c("TKN", "TN", "TP"), times = 4),
  flow_file = rep(c("Cocoa_Final.CSV", "Sanford_Final.CSV",
                    "Satsuma_Final.CSV", "Jacksonville_Final.CSV"), each = 3),
  sample_file = c(
    "TKN_Rockledge.CSV", "TN_Rockledge.CSV", "TP_Rockledge.CSV",
    "TKN_Sanford_Final.CSV", "TN_Sanford.CSV", "TP_Sanford.CSV",
    "TKN_CowCreek.CSV", "TN_CowCreek.CSV", "TP_CowCreek.CSV",
    "TKN_Jacksonville.CSV", "TN_Jacksonville.CSV", "TP_Jacksonville.CSV"),
  info_file = c(
    "TKN_Rockledge_Info.csv", "TN_Rockledge_Info.csv", "TP_Rockledge_Info.csv",
    "TKN_Sanford_Info.csv", "TN_Sanford_Info.csv", "TP_Sanford_Info.csv",
    "TKN_CowCreek_Info.csv", "TN_CowCreek_Info.csv", "TP_CowCreek_Info.csv",
    "TKN_Jacksonville_Info.csv", "TN_Jacksonville_Info.csv", "TP_Jacksonville_Info.csv"),
  stringsAsFactors = FALSE
)
stopifnot(nrow(config) == 12L, !anyDuplicated(paste(config$site, config$analyte)))

# ----- HELPERS ---------------------------------------------------------------
slug <- function(s) gsub("[^A-Za-z0-9]+", "_", s)
write_plain <- function(x, path) writeLines(as.character(x), con = path)
issue_log <- character()
record_issue <- function(msg) {
  message("AUDIT NOTICE: ", msg)
  issue_log <<- c(issue_log, paste(format(Sys.time(), "%H:%M:%S"), msg))
}
# A locked/open file never hides the remaining figures and tables.
# New run-specific output directory makes permission errors unlikely, but
# any new writing errors are explicitly recorded, not reported as success.
safe_csv <- function(dat, path) {
  ok <- tryCatch({
    write.csv(dat, path, row.names = FALSE, na = "")
    file.exists(path) && file.info(path)$size > 0
  }, error=function(e) {
    record_issue(paste("WRITE FAILED", path, conditionMessage(e)))
    FALSE
  })
  if (!isTRUE(ok)) record_issue(paste("CSV NOT VERIFIED", path))
  invisible(ok)
}


# A single row of diagnostics, with explicit consistent column names.
extract_diagnostics <- function(eList, site, analyte, sample) {
  st <- EGRET::errorStats(eList)
  required <- c("RsqLogC", "RsqLogF", "rmse", "sepPercent")
  if (!all(required %in% names(st))) {
    stop("EGRET errorStats column names have changed: ", paste(names(st), collapse = ", "))
  }
  n_cen <- sum(sample$Uncen == 0, na.rm = TRUE)
  fb <- EGRET::fluxBiasStat(eList$Sample)
  flux_bias <- if (length(fb) >= 3L) as.numeric(fb[3L]) else NA_real_
  # Water year N = calendar year + 1 for Oct/Nov/Dec.
  wy <- as.integer(format(sample$Date, "%Y")) +
    as.integer(as.integer(format(sample$Date, "%m")) >= 10L)
  data.frame(Site = site, Constituent = analyte,
             n = nrow(sample), n_censored = n_cen,
             censored_pct = round(100 * n_cen / nrow(sample), 2),
             first_sample_date = as.character(min(sample$Date)),
             last_sample_date  = as.character(max(sample$Date)),
             n_samples_WY2024 = sum(wy == 2024),
             RsqLogC = as.numeric(st$RsqLogC[1]),
             RsqLogF = as.numeric(st$RsqLogF[1]),
             RMSE_ln = as.numeric(st$rmse[1]),
             SEP_percent = as.numeric(st$sepPercent[1]),
             Flux_bias_statistic = flux_bias,
             stringsAsFactors = FALSE)
}

# Get annual EGRET estimates in their NATIVE units, convert flux rates
# kg/day -> 10^3 kg/year exactly once. Do NOT multiply by 35.3147 again.
annual_results <- function(eList) {
  a <- EGRET::setupYears(eList$Daily, paLong = PA_LONG, paStart = PA_START)
  must <- c("DecYear", "Conc", "FNConc", "Flux", "FNFlux")
  if (!all(must %in% names(a))) stop("Unexpected EGRET setupYears output")
  # For Oct-Sep water years the year containing the mid-point is the WY.
  a$WaterYear <- floor(a$DecYear)
  a$Conc_mgL <- a$Conc
  a$FNConc_mgL <- a$FNConc
  a$Flux_1000kgyr <- a$Flux * 365.25 / 1000
  a$FNFlux_1000kgyr <- a$FNFlux * 365.25 / 1000
  a[, c("WaterYear", "DecYear", "Conc_mgL", "FNConc_mgL",
         "Flux_1000kgyr", "FNFlux_1000kgyr")]
}

# EGRET tableChangeSingle is the authoritative routine that reproduces the
# ten time windows in Tables 2-4. Never scrape rounded console output.
change_results <- function(eList, site, analyte, dest) {
  # Returns columns: Year1, Year2, change, slope, change %, slope %.
  cc <- NULL
  ff <- NULL
  capture.output(cc <- EGRET::tableChangeSingle(
    eList, fluxUnit = 8, yearPoints = YEAR_POINTS, flux = FALSE),
    file = file.path(dest, "EGRET_concentration_changes_console.txt"))
  capture.output(ff <- EGRET::tableChangeSingle(
    eList, fluxUnit = 8, yearPoints = YEAR_POINTS, flux = TRUE),
    file = file.path(dest, "EGRET_flux_changes_console.txt"))
  if (ncol(cc) != 6L || ncol(ff) != 6L) stop("Unexpected EGRET tableChangeSingle columns")
  names(cc) <- c("From_WY", "To_WY", "Concentration_change_mgL",
                 "Concentration_slope_mgL_per_year", "Concentration_change_pct",
                 "Concentration_slope_pct_per_year")
  names(ff) <- c("From_WY", "To_WY", "Flux_change_1000kg_per_year",
                 "Flux_slope_1000kg_per_year_per_year", "Flux_change_pct",
                 "Flux_slope_pct_per_year")
  cc[] <- lapply(cc, as.numeric)
  ff[] <- lapply(ff, as.numeric)
  key1 <- paste(cc$From_WY, cc$To_WY)
  key2 <- paste(ff$From_WY, ff$To_WY)
  expected <- unlist(lapply(seq_len(length(YEAR_POINTS) - 1L), function(i)
    paste(YEAR_POINTS[i], YEAR_POINTS[(i + 1L):length(YEAR_POINTS)])))
  if (!setequal(key1, expected) || !setequal(key2, expected)) {
    stop("EGRET silently changed yearPoints! Check data record and years: ", site, " ", analyte)
  }
  ff <- ff[match(key1, key2), , drop = FALSE]
  out <- cbind(data.frame(Site = site, Constituent = analyte),
               cc, ff[, -c(1, 2), drop = FALSE])
  # Reviewer-required CIs: NA is deliberately NOT A NUMBER when not computed.
  out$Conc_change_CI95_low_mgL <- NA_real_
  out$Conc_change_CI95_high_mgL <- NA_real_
  out$Flux_change_CI95_low_1000kgyr <- NA_real_
  out$Flux_change_CI95_high_1000kgyr <- NA_real_
  out$CI_method <- "NOT_COMPUTED"
  out
}

# Read raw discharge once per gauge (metadata/audit ONLY). The first column
# is date; the second is cfs. Model imports use EGRET readUserDaily.
flow_audit <- function(flow_path, out_path) {
  raw <- read.csv(flow_path, check.names = FALSE)
  if (ncol(raw) < 2L) stop("Need Date and discharge columns in ", flow_path)
  ds <- as.Date(as.character(raw[[1]]), format = "%m/%d/%Y")
  iso <- as.Date(as.character(raw[[1]]), format = "%Y-%m-%d")
  ds[is.na(ds)] <- iso[is.na(ds)]
  q  <- suppressWarnings(as.numeric(raw[[2]]))
  if (anyNA(ds) || anyNA(q) || anyDuplicated(ds))
    stop("Bad dates/flows (or duplicate dates) in ", flow_path)
  if (any(q < 0)) stop("Signed NEGATIVE Q found in input. Cannot directly log; build documented scenarios first: ", flow_path)
  if (!all(diff(ds) == 1)) warning("Missing/gapped daily records in ", basename(flow_path), "; do not rely on implicit filling")
  s <- data.frame(file = basename(flow_path), n_days = length(q),
                  first_date = as.character(min(ds)), last_date = as.character(max(ds)),
                  n_zero = sum(q == 0), pct_zero = 100 * mean(q == 0),
                  n_negative = sum(q < 0),
                  n_missing = sum(is.na(q)))
  safe_csv(s, out_path)
  s
}

# Map the annual CI data to the EGRET AnnualResults water year.  ciCalculations
# outputs Year in decimal-year scale; do not interpret as a calendar-year series.
annual_with_ci <- function(annual, ci) {
  required <- c("Year", "FNConcLow", "FNConcHigh", "FNFluxLow", "FNFluxHigh")
  if (!all(required %in% names(ci))) stop("Unexpected EGRETci CI column names")
  matched <- match(annual$WaterYear, floor(as.numeric(ci$Year)))
  if (all(is.na(matched))) stop("EGRETci Year did not match any annual WaterYear")
  annual$FNConc_L95 <- as.numeric(ci$FNConcLow[matched])
  annual$FNConc_U95 <- as.numeric(ci$FNConcHigh[matched])
  annual$FNFlux_L95_1000kgyr <- as.numeric(ci$FNFluxLow[matched]) * 365.25/1000
  annual$FNFlux_U95_1000kgyr <- as.numeric(ci$FNFluxHigh[matched]) * 365.25/1000
  annual
}

# ----- PRINT-SIZE PUBLICATION FIGURES 5–7 (LARGER FONTS) ------------------
# FINAL 2-COLUMN PAGE SIZE. Do not draw at 11.2 in and then shrink to 7.35 in:
# doing so reduces 10-pt lettering to ~6.6 pt. Every font here is sized for
# the intended final width (~187 mm). No model estimates are changed.
#
# Outputs: 600-dpi TIFF and vector PDF. Model uncertainty is plotted ONLY
# when corresponding actual EGRETci intervals are present in annual_list.
# This section uses annual_list[["Rockledge_TKN"]], etc. automatically.

FIG_SITE_ORDER <- c("Rockledge", "Sanford", "Cow Creek", "Jacksonville")
FIG_LINE_COL <- "#188C39"   # darker green, readable in print
FIG_DOT_COL  <- "#101010"
FIG_XTICKS <- c(2000L, 2005L, 2010L, 2015L, 2020L, 2024L)

# Journal DOUBLE-COLUMN figure; the 4 x 2 grid is unsuitable for single column.
FIG_WIDTH_IN <- 7.35     # 186.7 mm, nearly full double-column width
FIG_HEIGHT_IN <- 9.65  # slightly taller to prevent larger labels crowding rows
FIG_FONT_PT <- 12.5     # final figure font baseline (points), applies to TIFF/PDF

fig_yticks <- function(top) {
  if (!is.finite(top) || top <= 0) return(c(0, 0.5, 1))
  vals <- pretty(c(0, top), n = 4)
  vals[vals >= 0 & vals <= top * (1 + 1e-9)]
}

fig_tick_labels <- function(at) {
  if (!length(at)) return(character())
  delta <- diff(sort(unique(at)))
  delta <- delta[is.finite(delta) & delta > 0]
  step <- if (length(delta)) min(delta) else 1
  digits <- if (step >= 1) 0L else min(4L, max(1L, as.integer(ceiling(-log10(step)))))
  formatC(at, format = "f", digits = digits, big.mark = ",")
}

fig_consecutive_groups <- function(ind, x) {
  if (!length(ind)) return(list())
  g <- cumsum(c(TRUE, diff(ind) != 1L | diff(x[ind]) > 1.01))
  split(ind, g)
}

fig_draw_line <- function(x, y, ...) {
  ii <- which(is.finite(x) & is.finite(y) & x >= FIG_START & x <= FIG_END)
  for (g in fig_consecutive_groups(ii, x)) {
    if (length(g) >= 2) lines(x[g], y[g], ...)
    else points(x[g], y[g], pch = 16, cex = 0.48, col = FIG_LINE_COL)
  }
  invisible(NULL)
}

fig_draw_CI <- function(x, lo, hi) {
  ii <- which(is.finite(x) & is.finite(lo) & is.finite(hi) & lo <= hi &
                x >= FIG_START & x <= FIG_END)
  for (g in fig_consecutive_groups(ii, x)) {
    if (length(g) >= 2) {
      polygon(c(x[g], rev(x[g])), c(lo[g], rev(hi[g])),
              col = adjustcolor(FIG_LINE_COL, alpha.f = 0.16), border = NA)
    }
  }
  invisible(NULL)
}

fig_one_panel <- function(annual, site, analyte, metric = c("conc", "flux"),
                          panel_letter = "a") {
  metric <- match.arg(metric)
  is_flux <- metric == "flux"
  required <- if (is_flux)
    c("WaterYear", "Flux_1000kgyr", "FNFlux_1000kgyr")
  else c("WaterYear", "Conc_mgL", "FNConc_mgL")
  if (!all(required %in% names(annual)))
    stop("Missing annual figure columns in ", site, " ", analyte, ": ",
         paste(setdiff(required, names(annual)), collapse = ", "))
  annual <- annual[order(annual$WaterYear), , drop = FALSE]
  if (anyDuplicated(annual$WaterYear))
    stop("Duplicate water years in ", site, " ", analyte)
  x <- as.numeric(annual$WaterYear)
  obs <- if (is_flux) as.numeric(annual$Flux_1000kgyr)
         else as.numeric(annual$Conc_mgL)
  norm <- if (is_flux) as.numeric(annual$FNFlux_1000kgyr)
          else as.numeric(annual$FNConc_mgL)
  loname <- if (is_flux) "FNFlux_L95_1000kgyr" else "FNConc_L95"
  hiname <- if (is_flux) "FNFlux_U95_1000kgyr" else "FNConc_U95"
  low <- if (loname %in% names(annual)) as.numeric(annual[[loname]])
         else rep(NA_real_, length(x))
  high <- if (hiname %in% names(annual)) as.numeric(annual[[hiname]])
          else rep(NA_real_, length(x))

  keep <- is.finite(x) & x >= FIG_START & x <= FIG_END
  vals <- c(obs[keep], norm[keep], low[keep], high[keep])
  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    plot.new(); box()
    text(0.5, 0.5, paste("No supported annual data:", site), cex = .9)
    return(invisible(FALSE))
  }
  maxval <- max(0, vals)
  if (maxval == 0) maxval <- 1
  top <- max(pretty(c(0, maxval * 1.10), n = 4))
  if (!is.finite(top) || top <= 0) top <- maxval * 1.2
  y_ticks <- fig_yticks(top)

  plot(NA_real_, xlim = c(FIG_START - 0.50, FIG_END + 0.50),
       ylim = c(0, top), type = "n", axes = FALSE,
       xlab = "", ylab = "", xaxs = "i", yaxs = "i")

  # Faint horizontal guides assist reading small printed panels.
  abline(h = y_ticks[y_ticks > 0], col = "#E6E6E6", lwd = 0.55)

  # More readable axes at ACTUAL double-column print size.
  axis(1, at = FIG_XTICKS, labels = FIG_XTICKS,
       cex.axis = 0.94, tck = -0.015, lwd = .7, lwd.ticks = .7)
  axis(2, at = y_ticks, labels = fig_tick_labels(y_ticks),
       las = 1, cex.axis = .94, tck = -0.015,
       lwd = .7, lwd.ticks = .7)
  box(lwd = .9, col = "#333333")

  fig_draw_CI(x, low, high)
  p <- keep & is.finite(obs)
  points(x[p], obs[p], pch = 16, cex = .72, col = FIG_DOT_COL)
  fig_draw_line(x, norm, col = FIG_LINE_COL, lwd = 2.3)

  # Site text ABOVE panel, not over the data. Panel lettering a-h.
  mtext(paste0("(", panel_letter, ")"), side = 3, line = .28,
        adj = 0.0, cex = .91, font = 2)
  mtext(site, side = 3, line = .28,
        adj = 1, cex = 1.09, font = 2)
  invisible(TRUE)
}

make_figure <- function(analyte, annual_list) {
  number <- switch(analyte, TKN = 5L, TN = 6L, TP = 7L,
                   stop("Unknown analyte: ", analyte))
  dat <- lapply(FIG_SITE_ORDER, function(site)
    annual_list[[paste(site, analyte, sep = "_")]])
  missing <- FIG_SITE_ORDER[vapply(dat, is.null, logical(1))]
  if (length(missing)) {
    warning("Figure ", number, " not generated: missing annual results for ",
            paste(missing, collapse = ", "))
    return(invisible(FALSE))
  }

  dest <- file.path(OUT, "Figures_5_6_7_Manuscript_PrintSize")
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  base <- sprintf("Figure_%d_%s_WRTDS_Manuscript", number, analyte)
  tiffile <- file.path(dest, paste0(base, "_600dpi.tif"))
  pdffile <- file.path(dest, paste0(base, "_vector.pdf"))

  draw <- function() {
    layout(matrix(1:8, nrow = 4L, byrow = TRUE))
    par(mar = c(2.68, 4.45, 1.75, .70),
        oma = c(2.40, .60, 2.60, .35),
        mgp = c(1.85, .54, 0), tcl = -.20,
        family = "sans", cex = 1, xpd = FALSE)
    panel_letters <- letters[1:8]
    for (i in seq_along(FIG_SITE_ORDER)) {
      fig_one_panel(dat[[i]], FIG_SITE_ORDER[i], analyte,
                    metric = "conc", panel_letter = panel_letters[(i-1)*2+1])
      fig_one_panel(dat[[i]], FIG_SITE_ORDER[i], analyte,
                    metric = "flux", panel_letter = panel_letters[(i-1)*2+2])
    }
    # Put units in COLUMN HEADERS rather than squeezing tall axis titles
    # between panels. Leave the figure caption outside the illustration.
    mtext("Concentration (mg/L)", side = 3, outer = TRUE,
          at = .25, line = .75, cex = 1.16, font = 2)
    mtext(expression(plain("Flux")~(10^3~plain("kg/year"))),
          side = 3, outer = TRUE,
          at = .75, line = .75, cex = 1.16, font = 1)
    mtext("Water year", side = 1, outer = TRUE,
          line = .88, cex = 1.05)
    # No internal figure title or verbose legend; use manuscript caption.
    # Black dots = annual model estimates; green = flow-normalized model.
  }

  # Device closure is guaranteed even if drawing fails. If either export
  # fails, the error propagates to the figure status instead of false success.
  export_one <- function(path, kind) {
    if (kind == "tiff") {
      grDevices::tiff(path, width=FIG_WIDTH_IN, height=FIG_HEIGHT_IN,
                      units="in", res=DPI, compression="lzw",
                      pointsize=FIG_FONT_PT, bg="white")
    } else {
      grDevices::pdf(path, width=FIG_WIDTH_IN, height=FIG_HEIGHT_IN,
                     pointsize=FIG_FONT_PT, useDingbats=FALSE,
                     family="Helvetica", bg="white")
    }
    tryCatch(draw(), finally=grDevices::dev.off())
    if (!file.exists(path) || is.na(file.info(path)$size) ||
        file.info(path)$size <= 0) stop("Figure export missing/empty: ", path)
  }
  export_one(tiffile, "tiff")
  export_one(pdffile, "pdf")

  any_ci <- any(vapply(dat, function(z)
    "FNConc_L95" %in% names(z) && any(is.finite(z$FNConc_L95)),
    logical(1)))
  writeLines(c(
    sprintf("Figure %d. Annual and flow-normalized %s concentration and flux estimates at four St. Johns River locations.",
            number, analyte),
    "Black dots represent EGRET annual model estimates; dark green lines connect annual flow-normalized estimates.",
    "Concentration in mg/L; annual flux rate in 10^3 kg/year.",
    if (any_ci) "Some panels may display computed EGRETci 95% confidence bands where supported."
    else "No EGRETci confidence bands are shown; intervals have not been computed.",
    "Water year: October through September.",
    "The modeled flux does not represent net signed bidirectional tidal transport.",
    "Publication figure requires model and hydrologic validation before submission."
  ), file.path(dest, paste0(base, "_caption.txt")))
  cat("Verified print-size Figure ", number, ":\n  ", tiffile,
      "\n  ", pdffile, "\n", sep = "")
  invisible(TRUE)
}


# ===========================================================================
# ST. JOHNS RIVER / EGRET WRTDS SUPPLEMENTARY DIAGNOSTICS -- V9
# Figure formatting only: no model refitting, no modification of predictions.
# Designed for readability at the final journal print width (7.35 inches).
# The original 8-panel composition AND optional split (4-panel) pages are saved.
# ===========================================================================
SUPP_WIDTH_IN <- 7.35
SUPP_HEIGHT_IN <- 10.00
SUPP_DPI <- 600L
SUPP_POINTSIZE <- 11.3
SUPP_CEX_AXIS <- 0.83
SUPP_CEX_LABEL <- 0.84
SUPP_CEX_PANEL <- 0.86
SUPP_POINT_CEX <- 0.38
SUPP_SPLIT_PAGES <- TRUE

# No experiment changes, no simulated censoring values, and no truncation of
# plotted observations. Censored values remain intervals/limits.
# Typography must be checked at 100% on the actual 7.35-inch PDF page.

supp_require <- function(x, columns, where) {
  bad <- setdiff(columns, names(x))
  if (length(bad)) stop(where, " is missing: ", paste(bad, collapse = ", "))
}

supp_panel <- function(draw, heading) {
  draw()
  # The heading carries long scientific units: never duplicate them on axes.
  graphics::mtext(heading, side=3, line=0.17, adj=0, font=2,
                  cex=SUPP_CEX_PANEL)
  invisible(TRUE)
}

# Panel (e): no second y-axis title and no detached (mg/L) label.
# Each box summarizes EGRET's original plotting variables; Sample$ConcAve is
# NOT an exact detection for censored measurements. This is disclosed in caption.
supp_concentration_boxes <- function(eList) {
  s <- eList$Sample
  d <- eList$Daily
  supp_require(s, c("ConcAve", "ConcHat"), "EGRET Sample")
  supp_require(d, "ConcDay", "EGRET Daily")
  vals <- list(
    "Samples*" = as.numeric(s$ConcAve),
    "Sample fit" = as.numeric(s$ConcHat),
    "Daily fit" = as.numeric(d$ConcDay)
  )
  vals <- lapply(vals, function(z) z[is.finite(z)])
  if (any(lengths(vals) < 2L)) stop("Panel (e): insufficient finite concentrations")
  graphics::boxplot(vals, outline=TRUE, varwidth=FALSE,
    col="#F0F0F0", border="#3B3B3B", pch=16, cex=0.22,
    ylab="", xlab="", las=1,
    cex.axis=SUPP_CEX_AXIS*0.92, tcl=-0.2,
    medcol="#303030", staplewex=0.35)
  # Units belong in the panel heading, not as a vertical displaced label.
  invisible(TRUE)
}

# Axis values are labelled compactly without changing their numeric scale.
supp_compact_number <- function(z) {
  vapply(z, function(x) {
    if (!is.finite(x)) return("")
    if (x == 0) return("0")
    ax <- abs(x)
    if (ax >= 1e6) return(paste0(formatC(x/1e6, digits=2, format="fg"),"M"))
    if (ax >= 1e3) return(paste0(formatC(x/1e3, digits=2, format="fg"),"k"))
    formatC(x, digits=3, format="fg")
  }, character(1))
}

# Panel (h): EGRET-equivalent sample flux conversion.
# mg/L * m3/s -> kg/day factor 86.4; kg/day -> 10^3 kg/year factor .36525
# This makes factor 31.5576. No change to modeled values.
supp_flux_compact_panel <- function(eList) {
  s <- eList$Sample
  supp_require(s, c("ConcHat", "ConcLow", "ConcHigh", "Uncen", "Q"),
               "EGRET Sample")
  conversion <- 86.4 * 0.36525
  x <- as.numeric(s$ConcHat) * as.numeric(s$Q) * conversion
  ymin <- as.numeric(s$ConcLow) * as.numeric(s$Q) * conversion
  ymax <- as.numeric(s$ConcHigh) * as.numeric(s$Q) * conversion
  uncen <- as.numeric(s$Uncen)
  ok <- is.finite(x) & is.finite(ymin) & is.finite(ymax) &
        is.finite(uncen) & x>=0 & ymin>=0 & ymax>=0 & ymin<=ymax
  if (sum(ok) < 2L) stop("Panel (h): insufficient valid sample-flux estimates")
  x<-x[ok]; ymin<-ymin[ok]; ymax<-ymax[ok]; uncen<-uncen[ok]
  top <- max(x, ymax)
  if (!is.finite(top) || top<=0) stop("Panel (h): non-positive flux range")
  # Equal physical scales on x and y guarantee a correct 1:1 line.
  lim <- c(0, 1.06*top)
  graphics::plot(NA_real_, xlim=lim, ylim=lim,
       axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
  breaks <- pretty(lim, n=4)
  breaks <- breaks[breaks>=lim[1] & breaks<=lim[2]]
  labs <- supp_compact_number(breaks)
  graphics::axis(1, at=breaks, labels=labs, cex.axis=.78, tck=-.02)
  graphics::axis(2, at=breaks, labels=labs, las=1, cex.axis=.78, tck=-.02)
  graphics::box(lwd=.8)
  graphics::abline(0,1,col="#7D7D7D",lwd=.9)
  detected <- uncen == 1
  if (any(detected))
    graphics::points(x[detected], ymax[detected], pch=16,
                     cex=SUPP_POINT_CEX, col="#222222")
  censored <- uncen == 0
  if (any(censored)) {
    graphics::segments(x[censored], ymin[censored],
                       x[censored], ymax[censored],
                       col="#6B6B6B", lwd=.65)
    graphics::points(x[censored], ymax[censored], pch=1,
                     cex=SUPP_POINT_CEX+.06, col="#4B4B4B")
  }
  # Short labels inside their own panels. 'k' and 'M' are tick multipliers.
  graphics::mtext("Predicted", side=1, line=1.70, cex=.77)
  graphics::mtext("Observed", side=2, line=2.05, cex=.77)
  invisible(TRUE)
}

# An individual panel is drawn from EGRET's fitted eList, so the original
# residual calculations remain package-defined and uncensored/limit handling
# stays the package's default except for the explicitly bounded flux panel.
supp_draw_one <- function(eList, id) {
  common <- list(eList=eList, tinyPlot=TRUE, printTitle=FALSE,
                 customPar=TRUE, cex=SUPP_POINT_CEX,
                 cex.axis=SUPP_CEX_AXIS, cex.lab=SUPP_CEX_LABEL)
  if (id==1L) return(supp_panel(function()
      do.call(EGRET::plotResidPred,c(common,list(stdResid=FALSE,randomCensored=FALSE))),
      "(a) Residual vs. fitted"))
  if (id==2L) return(supp_panel(function()
      do.call(EGRET::plotResidQ,c(common,list(qUnit=2,stdResid=FALSE,
                                           randomCensored=FALSE,rmSciX=FALSE))),
      "(b) Residual vs. discharge"))
  if (id==3L) return(supp_panel(function()
      do.call(EGRET::plotResidTime,c(common,list(stdResid=FALSE,randomCensored=FALSE))),
      "(c) Residual vs. time"))
  if (id==4L) return(supp_panel(function()
      do.call(EGRET::boxResidMonth,c(common,list(stdResid=FALSE,randomCensored=FALSE))),
      "(d) Residuals by month"))
  if (id==5L) return(supp_panel(function() supp_concentration_boxes(eList),
      "(e) Concentration (mg/L)"))
  if (id==6L) return(supp_panel(function()
      do.call(EGRET::plotConcPred,c(common,list(randomCensored=FALSE,logScale=FALSE))),
      "(f) Observed vs. fitted conc."))
  if (id==7L) return(supp_panel(function()
      do.call(EGRET::boxQTwice,c(common,list(qUnit=2,logScale=TRUE))),
      "(g) Discharge distributions"))
  if (id==8L) return(supp_panel(function() supp_flux_compact_panel(eList),
      expression("(h) Flux ("*10^3*" kg/year)")))
  stop("Invalid diagnostic panel number: ", id)
}

supp_draw_diagnostics <- function(eList, site, analyte, ids=1:8) {
  if (!(length(ids) %in% c(4L,8L))) stop("Expected four or eight panels")
  old <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old), add=TRUE)
  # The middle blank column is a real physical spacer. It is larger than
  # the maximal axis-label width required by panels (f) and (h).
  nrows <- length(ids)/2L
  cells <- matrix(0L, nrow=nrows, ncol=3L)
  cells[,1] <- seq(1L, length(ids), by=2L)
  cells[,3] <- seq(2L, length(ids), by=2L)
  graphics::layout(cells, widths=c(1,0.24,1), heights=rep(1,nrows))
  graphics::par(oma=c(.60,.20,.45,.85), family="sans", cex=1)

  for (j in seq_along(ids)) {
    is_right <- j%%2L==0L
    # Separate margins for left / right columns. All long axis names have
    # been omitted from (e), and (h) has controlled short axis names.
    margins <- if(is_right) c(3.25,3.15,1.70,.95) else c(3.25,4.00,1.70,.50)
    graphics::par(mar=margins, mgp=c(1.55,.44,0), las=1,
                  tcl=-.22, xaxs="r", yaxs="r", cex=1,
                  cex.axis=SUPP_CEX_AXIS, cex.lab=SUPP_CEX_LABEL,
                  family="sans", fg="#252525", col.axis="#252525",
                  col.lab="#252525")
    supp_draw_one(eList,ids[j])
  }
  invisible(TRUE)
}

supp_validate_results <- function(eList, site, analyte) {
  s<-eList$Sample; d<-eList$Daily
  supp_require(s,c("ConcHigh","Uncen","ConcHat","ConcAve"),"EGRET Sample")
  supp_require(d,"ConcDay","EGRET Daily")
  max_finite <- function(x) {
    x<-as.numeric(x); x<-x[is.finite(x)];
    if(length(x)) max(x) else NA_real_
  }
  # A QC alarm, not a threshold for deletion or resetting predicted values.
  observed_ceiling <- max_finite(s$ConcHigh)
  sample_fitted_max <- max_finite(s$ConcHat)
  daily_fitted_max <- max_finite(d$ConcDay)
  data.frame(Site=site,Constituent=analyte,
    Sample_count=nrow(s), Censored_count=sum(s$Uncen==0,na.rm=TRUE),
    Reported_maximum_mgL=observed_ceiling,
    Model_sample_maximum_mgL=sample_fitted_max,
    Model_daily_maximum_mgL=daily_fitted_max,
    Sample_fit_exceeds_reported_max=sample_fitted_max>observed_ceiling,
    Daily_fit_exceeds_reported_max=daily_fitted_max>observed_ceiling,
    stringsAsFactors=FALSE)
}

supp_export_one <- function(eList, site, analyte, path, kind, ids=1:8) {
  if(kind=="tiff") {
    grDevices::tiff(path, width=SUPP_WIDTH_IN,
       height=if(length(ids)==8L) SUPP_HEIGHT_IN else 6.05,
       units="in",res=SUPP_DPI,compression="lzw",
       pointsize=SUPP_POINTSIZE,bg="white")
  } else {
    grDevices::pdf(path,width=SUPP_WIDTH_IN,
       height=if(length(ids)==8L) SUPP_HEIGHT_IN else 6.05,
       pointsize=SUPP_POINTSIZE,useDingbats=FALSE,
       family="Helvetica",bg="white")
  }
  tryCatch(supp_draw_diagnostics(eList,site,analyte,ids),
           finally=grDevices::dev.off())
  if(!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size<1000)
    stop("Exported figure missing or too small: ",path)
  invisible(path)
}

save_supp_diagnostics <- function(eList, site, analyte, dest_dir) {
  supp_require(eList,c("Sample","Daily","INFO"),"Fitted eList")
  supp_require(eList$Sample,c("ConcHat","ConcLow","ConcHigh","Uncen","Q"),
               "Fitted eList$Sample")
  supp_require(eList$Daily,"ConcDay","Fitted eList$Daily")
  dir.create(dest_dir,showWarnings=FALSE,recursive=TRUE)
  if(!dir.exists(dest_dir)) stop("No output directory: ",dest_dir)
  prefix <- paste0("Supplement_",gsub("[^A-Za-z0-9]+","_",site),"_",analyte,
                   "_v9_PrintSafe")
  tif<-file.path(dest_dir,paste0(prefix,"_600dpi.tif"))
  pdf<-file.path(dest_dir,paste0(prefix,"_vector.pdf"))
  caption<-file.path(dest_dir,paste0(prefix,"_caption.txt"))
  supp_export_one(eList,site,analyte,tif,"tiff",1:8)
  supp_export_one(eList,site,analyte,pdf,"pdf",1:8)
  parts <- character()
  if (isTRUE(SUPP_SPLIT_PAGES)) for(part in 1:2) {
    ids<-if(part==1) 1:4 else 5:8
    for(kind in c("pdf","tiff")) {
      ext<-if(kind=="pdf") ".pdf" else ".tif"
      path<-file.path(dest_dir,paste0(prefix,"_Part",part,ext))
      supp_export_one(eList,site,analyte,path,kind,ids)
      parts<-c(parts,path)
    }
  }
  fb <- tryCatch(EGRET::fluxBiasStat(eList$Sample),error=function(e)NULL)
  fb_text <- if(length(fb)>=3) format(as.numeric(fb[3]),digits=4) else "not available"
  writeLines(c(
    paste0("WRTDS diagnostic figure for ",analyte," at ",site,"."),
    "Eight panels (a)-(h); optional split two-page versions show a-d then e-h.",
    "(a) Log-concentration residuals versus fitted log concentration.",
    "(b) Log-concentration residuals versus discharge (m3/s, log x-axis).",
    "(c) Log-concentration residuals versus sample year.",
    "(d) Monthly distributions of log-concentration residuals.",
    "(e) Concentration (mg/L) distributions: EGRET ConcAve of sampled values*,",
    "    sample-date modeled concentration (ConcHat), and all-day modeled (ConcDay).",
    "    * For nondetects, EGRET ConcAve is a plotting/estimation value, NOT",
    "      an exact detected concentration. Interpret this boxplot with censoring context.",
    "(f) Observed versus fitted sample concentration (mg/L).",
    "(g) Sampled-day and all-day discharge distributions (m3/s, log axis).",
    "(h) Estimated (horizontal) and observed (vertical) sample nutrient flux",
    "    (10^3 kg/year); k and M on tick marks mean 10^3 and 10^6 tick units.",
    "    Open markers and vertical segments show censored bounds in (h), not detections.",
    "    Grey diagonal is a 1:1 reference; not a fitted regression.",
    paste0("EGRET flux-bias statistic: ",fb_text,"."),
    "Residual panels use the original EGRET censored-residual conventions.",
    "The models and flux estimates remain subject to signed-flow, gauge-pairing,",
    "censoring provenance, and TN >= TKN consistency validation."
  ), caption, useBytes=TRUE)
  qc <- supp_validate_results(eList,site,analyte)
  utils::write.csv(qc,file.path(dest_dir,paste0(prefix,"_model_QC.csv")),row.names=FALSE)
  invisible(list(tiff=tif,pdf=pdf,caption=caption,parts=parts,qc=qc))
}

# ----- FIT ALL TWELVE MODELS --------------------------------------------------
fit_list <- list()
annual_list <- list()
table1_list <- list()
change_list <- list()
ci_list <- list()
status <- vector("list", nrow(config))

for (i in seq_len(nrow(config))) {
  row <- config[i, ]
  site <- row$site; analyte <- row$analyte
  key <- paste(site, analyte, sep = "_")
  jobdir <- file.path(OUT, "Models", paste0(slug(site), "_", analyte))
  dir.create(jobdir, recursive = TRUE, showWarnings = FALSE)
  cat("\n=====", i, "/ 12", key, "=====\n")
  run_one <- tryCatch({
    files <- c(file.path(FLOW_DIR, row$flow_file),
               file.path(WQ_DIR, row$sample_file),
               file.path(WQ_DIR, row$info_file))
    if (!all(file.exists(files)))
      stop("Required input file(s) missing: ",
           paste(files[!file.exists(files)], collapse="; "))
    saved_path <- file.path(SAVED_MODELS_DIR,
                            paste0(slug(site), "_", analyte),
                            "EGRET_fitted_eList.rds")
    reused <- USE_SAVED_MODELS && file.exists(saved_path)
    if (reused) {
      eList <- readRDS(saved_path)
      if (!all(c("Daily", "Sample", "INFO") %in% names(eList)) ||
          !all(c("ConcDay","FluxDay","FNConc","FNFlux") %in% names(eList$Daily)))
        stop("Saved model is incomplete: ",saved_path)
      Sample <- eList$Sample
      Daily <- eList$Daily
      INFO <- eList$INFO
      record_issue(paste(key,"REUSED earlier fitted EGRET model; NOT a fresh rerun"))
    } else {
      Sample <- EGRET::readUserSample(WQ_DIR, row$sample_file,
                                      separator=",")
      Daily <- EGRET::readUserDaily(FLOW_DIR, row$flow_file,
                                    qUnit=INPUT_Q_UNIT,
                                    adjust=ZERO_ADJUST, fill=FALSE)
      if (anyNA(Daily$Q) || any(Daily$Q <= 0) ||
          any(!is.finite(Daily$LogQ)))
        stop("Invalid/missing/nonpositive EGRET discharge after import")
      if (!isTRUE(all.equal(as.numeric(Daily$LogQ),log(as.numeric(Daily$Q)),
                   tolerance=1e-7,check.attributes=FALSE)))
        stop("Daily$LogQ differs from log(Daily$Q)")
      INFO <- EGRET::readUserInfo(WQ_DIR,row$info_file,
                                  interactive=FALSE)
      INFO$shortName <- site
      INFO$station.nm <- site
      INFO$paramShortName <- analyte
      INFO$constitAbbrev <- analyte
      INFO$param.nm <- switch(analyte,
              TKN="Total Kjeldahl nitrogen",TN="Total nitrogen",
              TP="Total phosphorus")
      INFO$staAbbrev <- switch(site,"Rockledge"="RLC",
            "Sanford"="SanF","Cow Creek"="CoCr",
            "Jacksonville"="Jack")
      INFO$param.units <- "mg/l"
      eList <- EGRET::mergeReport(INFO,Daily,Sample)
      eList <- EGRET::setPA(eList,paStart=PA_START,paLong=PA_LONG)
      saveRDS(list(INFO=INFO,Daily=Daily,Sample=Sample),
              file.path(jobdir,"exact_model_inputs.rds"))
      eList <- EGRET::modelEstimation(eList,
               windowY=7,windowQ=2,windowS=.5,
               minNumObs=100,minNumUncen=50,edgeAdjust=TRUE)
    }
    eList <- EGRET::setPA(eList, paStart=PA_START, paLong=PA_LONG)
    Sample <- eList$Sample
    Daily <- eList$Daily
    saveRDS(eList,file.path(jobdir,"EGRET_fitted_eList.rds"))
    safe_csv(data.frame(Site=site,Constituent=analyte,
        origin=if(reused)"REUSED_SAVED_FIT" else "FRESH_RUN",
        input_flow_file=row$flow_file,input_sample_file=row$sample_file,
        sample_MD5=unname(tools::md5sum(file.path(WQ_DIR,row$sample_file))),
        flow_MD5=unname(tools::md5sum(file.path(FLOW_DIR,row$flow_file)))),
        file.path(jobdir,"input_provenance.csv"))
    # Report but do not replace the previously zeroed reverse-flow days.
    if (any(!is.finite(Daily$LogQ))) stop("Non-finite LogQ in model")
    diag <- extract_diagnostics(eList, site, analyte, Sample)
    # Reviewer 2: show annual source support, including 1999-2003.
    sample_wy <- as.integer(format(Sample$Date, "%Y")) +
      as.integer(as.integer(format(Sample$Date, "%m")) >= 10L)
    coverage <- do.call(rbind, lapply(sort(unique(sample_wy)), function(y) {
      ids <- which(sample_wy == y)
      data.frame(Site=site, Constituent=analyte, WaterYear=y,
                 n_samples=length(ids),
                 n_censored=sum(Sample$Uncen[ids] == 0, na.rm=TRUE))
    }))
    safe_csv(coverage, file.path(jobdir, "observed_sample_counts_by_WY.csv"))
    # Show water-quality coverage in 1999-2003, even though trends start 2004.
    pre2004 <- coverage[coverage$WaterYear < 2004, , drop=FALSE]
    safe_csv(pre2004, file.path(jobdir, "reviewer2_samples_before_2004.csv"))
    # Journal-size 8-panel supplementary diagnostics: readable 10-point labels,
    # independent 600 dpi TIFF and vector PDF for every site/analyte.
    tryCatch({
      supp_paths <- save_supp_diagnostics(
        eList, site, analyte,
        file.path(OUT, "Supplementary_Diagnostics", paste0(slug(site), "_", analyte)))
      safe_csv(data.frame(Site=site, Constituent=analyte,
                          TIFF=supp_paths$tiff, PDF=supp_paths$pdf,
                          Caption=supp_paths$caption,
                          TIFF_size_bytes=as.numeric(file.info(supp_paths$tiff)$size),
                          PDF_size_bytes=as.numeric(file.info(supp_paths$pdf)$size)),
               file.path(jobdir, "Supplement_diagnostic_export_status.csv"))
    }, error=function(e) {
      record_issue(paste("SUPPLEMENTARY FIGURE EXPORT FAILED", key,
                         conditionMessage(e)))
    })
    ann <- annual_results(eList)
    safe_csv(diag, file.path(jobdir, "Table1_model_diagnostics.csv"))
    safe_csv(ann, file.path(jobdir, "annual_EGRET_estimates.csv"))
    change <- change_results(eList, site, analyte, jobdir)
    safe_csv(change, file.path(jobdir, "Table_changes_no_CI.csv"))

    # Reviewer requested endpoint support, not just record end date.
    last_sample <- max(Sample$Date)
    end_wy_lastday <- as.Date(paste0(max(YEAR_POINTS), "-09-30"))
    if (last_sample < end_wy_lastday - 365)
      warning(key, ": end-WY model >1 year beyond last actual constituent observation")
    endpoint_counts <- vapply(YEAR_POINTS, function(y)
      sum(sample_wy == y, na.rm=TRUE), integer(1))
    if (any(endpoint_counts < 3L)) {
      bad <- YEAR_POINTS[endpoint_counts < 3L]
      record_issue(paste0(key, ": comparison endpoint water years with <3 samples: ",
                           paste(bad, collapse=", "),
                           "; inspect interpolation/extrapolation and CIs"))
    }
    safe_csv(data.frame(WaterYear=YEAR_POINTS,n_observed_samples=endpoint_counts),
             file.path(jobdir,"reviewer_endpoint_support.csv"))

    # Optional: real EGRETci 95% annual bands. If error: abort this CI step,
    # leave intervals missing, NEVER substitute a visually invented ribbon.
    if (RUN_CI_BANDS) tryCatch({
      ci <- EGRETci::ciCalculations(
        eList, startSeed = CI_SEED + i, verbose = TRUE,
        nBoot = CI_N_BOOT, blockLength = CI_BLOCK_LENGTH, widthCI = 95)
      saveRDS(ci, file.path(jobdir, "EGRETci_annual_95pct_CI.rds"))
      safe_csv(ci, file.path(jobdir, "annual_95pct_CI_native_units.csv"))
      ann <- annual_with_ci(ann, ci)
      ci_list[[key]] <- ci
      safe_csv(ann, file.path(jobdir, "annual_EGRET_with_95pct_CI.csv"))
    }, error=function(e) {
      record_issue(paste("CI annual FAILED; non-CI model preserved:",key,
                         conditionMessage(e)))
    })

    if (RUN_CI_CHANGES) {
      for (win in CI_WINDOWS) tryCatch({
        yr1 <- as.integer(win[1]); yr2 <- as.integer(win[2])
        j <- which(change$From_WY == yr1 & change$To_WY == yr2)
        if (length(j) != 1L) stop("CI requested for a window not in YEAR_POINTS")
        # windowSide=0 is the stationary-flow reference used by modelEstimation.
        pair <- EGRET::runPairs(
          eList, yr1, yr2, windowSide = 0,
          windowY=7, windowQ=2, windowS=0.5,
          minNumObs=100, minNumUncen=50, edgeAdjust=TRUE)
        boot <- EGRETci::runPairsBoot(
          eList, pair, nBoot=CI_N_BOOT,
          startSeed=CI_SEED + 100*i + yr2 - yr1,
          blockLength=CI_BLOCK_LENGTH)
        saveRDS(boot, file.path(jobdir,
                     paste0("EGRETci_change_WY",yr1,"_",yr2,".rds")))
        # EGRETci exposes the raw bootstrap changes as xConc (mg/L) and
        # xFlux (10^6 kg/year; follows EGRET::runPairs units). Calculate
        # empirical 95% quantiles; do not assume undocumented bootOut names.
        xConc <- as.numeric(boot$xConc)
        xFlux <- as.numeric(boot$xFlux)
        if (sum(is.finite(xConc)) < 0.9 * CI_N_BOOT ||
            sum(is.finite(xFlux)) < 0.9 * CI_N_BOOT)
          stop("Too few finite EGRETci replicates: ", key,
               " ",yr1,"-",yr2)
        qqC <- quantile(xConc, c(.025,.975), na.rm=TRUE, names=FALSE)
        qqF <- quantile(xFlux, c(.025,.975), na.rm=TRUE, names=FALSE)*1000
        change$Conc_change_CI95_low_mgL[j] <- qqC[1]
        change$Conc_change_CI95_high_mgL[j] <- qqC[2]
        change$Flux_change_CI95_low_1000kgyr[j] <- qqF[1]
        change$Flux_change_CI95_high_1000kgyr[j] <- qqF[2]
        change$CI_method[j] <- paste0("EGRETci block bootstrap, n=",CI_N_BOOT,
                                      ", days=",CI_BLOCK_LENGTH)
        # Check bootstrap mean and original point value without forced alignment.
        compare <- data.frame(From_WY=yr1, To_WY=yr2,
            Change_EGRET_mgL=change$Concentration_change_mgL[j],
            Median_boot_change_mgL=median(xConc,na.rm=TRUE),
            Change_EGRET_1000kgyr=change$Flux_change_1000kg_per_year[j],
            Median_boot_change_1000kgyr=median(xFlux,na.rm=TRUE)*1000)
        safe_csv(compare, file.path(jobdir,
              sprintf("CI_reproducibility_%d_%d.csv",yr1,yr2)))
      }, error=function(e) {
        record_issue(paste("CI change FAILED; original model preserved:",key,
                           paste(win,collapse="-"),conditionMessage(e)))
      })
      safe_csv(change, file.path(jobdir, "Table_changes_with_selected_CIs.csv"))
    }
    list(eList=eList, diag=diag, ann=ann, change=change)
  }, error = function(e) {
    write_plain(c("FAILED", key, conditionMessage(e)), file.path(jobdir,"FAILED.txt"))
    warning("Model FAILED: ", key, " -> ", conditionMessage(e))
    NULL
  })
  if (is.null(run_one)) {
    status[[i]] <- data.frame(Site=site, Constituent=analyte,
                             Status="FAILED", stringsAsFactors=FALSE)
  } else {
    fit_list[[key]] <- run_one$eList
    annual_list[[key]] <- run_one$ann
    table1_list[[key]] <- run_one$diag
    change_list[[key]] <- run_one$change
    status[[i]] <- data.frame(Site=site, Constituent=analyte,
                             Status=if(USE_SAVED_MODELS)"COMPLETED_OR_REUSED_EXPLORATORY"
                              else "COMPLETED_FRESH_EXPLORATORY", stringsAsFactors=FALSE)
  }
  # Store progress even if a subsequent fit fails.
  safe_csv(do.call(rbind,status[seq_len(i)]),
            file.path(OUT, "00_run_status.csv"))
}

# ----- COMBINE TABLES 1-4 / FIGURES 5-7 ------------------------------
if (!length(table1_list)) stop("No model fitted successfully; inspect FAILED.txt")
export_status <- list()
export_status[["Table1"]] <- safe_csv(do.call(rbind,table1_list),
                       file.path(OUT,"Table_1_Model_Diagnostics.csv"))
for (a in c("TKN","TN","TP")) {
  tblnum <- switch(a,TKN=2L,TN=3L,TP=4L)
  relevant <- change_list[endsWith(names(change_list),paste0("_",a))]
  path <- file.path(OUT,sprintf("Table_%d_%s_Trends_mgL_and_1000kg_per_year.csv",tblnum,a))
  export_status[[paste0("Table",tblnum)]] <- if(length(relevant))
      safe_csv(do.call(rbind,relevant),path) else FALSE
  if (!length(relevant)) record_issue(paste("TABLE MISSING",tblnum,"no completed models"))
  export_status[[paste0("Figure",tblnum+3L)]] <- tryCatch(
    make_figure(a,annual_list),
    error=function(e) {record_issue(paste("FIGURE FAILED",a,conditionMessage(e)));FALSE})
}
safe_csv(data.frame(Artifact=names(export_status),
    Exported=vapply(export_status,isTRUE,logical(1))),
    file.path(OUT,"00_export_status.csv"))

# Extra reviewer-specific physical-consistency check across modelled annual data.
# These are independently modeled values: even if TN < TKN, do not "correct"
# them by truncation; investigate the original observations and model support.
compare <- list()
for (site in c("Rockledge", "Sanford", "Cow Creek", "Jacksonville")) {
  tn <- annual_list[[paste(site,"TN",sep="_")]]
  kn <- annual_list[[paste(site,"TKN",sep="_")]]
  if (is.null(tn) || is.null(kn)) next
  m <- merge(tn[,c("WaterYear","FNConc_mgL")], kn[,c("WaterYear","FNConc_mgL")],
             by="WaterYear",suffixes=c("_TN","_TKN"))
  m$Site <- site
  m$TN_minus_TKN_mgL <- m$FNConc_mgL_TN - m$FNConc_mgL_TKN
  m$Flag_model_TN_below_TKN <- is.finite(m$TN_minus_TKN_mgL) &
                               m$TN_minus_TKN_mgL < 0
  compare[[site]] <- m
}
if (length(compare)) {
  cmp <- do.call(rbind,compare)
  safe_csv(cmp, file.path(OUT, "REVIEWER2_Modelled_TN_minus_TKN_by_WY.csv"))
}

# Reviewer 2: observed same-date TN/TKN comparison, using censoring bounds.
# IMPORTANT: date matches are not proof of identical sampling site, fraction,
# or physical sample; verify original station/activity IDs separately.
obs_pairs <- list()
for (site in FIG_SITE_ORDER) {
  etn <- fit_list[[paste(site,"TN",sep="_")]]
  etkn <- fit_list[[paste(site,"TKN",sep="_")]]
  if (is.null(etn) || is.null(etkn)) next
  take <- function(e, postfix) {
    x <- e$Sample
    must <- c("Date","ConcHigh","Uncen")
    if (!all(must %in% names(x))) stop("EGRET Sample missing columns")
    if (anyDuplicated(x$Date)) record_issue(paste(site,postfix,"has same-date replicates"))
    x <- x[!duplicated(x$Date),must,drop=FALSE]
    names(x) <- c("Date",paste0("Value_",postfix),paste0("Uncen_",postfix))
    x
  }
  z <- merge(take(etn,"TN"),take(etkn,"TKN"),by="Date")
  if (!nrow(z)) next
  z$Site <- site
  z$Both_detected <- z$Uncen_TN == 1 & z$Uncen_TKN == 1
  z$TN_minus_TKN_detected_mgL <- ifelse(z$Both_detected,
                             z$Value_TN-z$Value_TKN,NA_real_)
  z$TN_ge_TKN_supported <- z$Uncen_TN == 1 &
                            z$Value_TN >= z$Value_TKN
  z$Confirmed_detected_violation <- z$Both_detected &
                                     z$Value_TN < z$Value_TKN
  obs_pairs[[site]] <- z
}
if (length(obs_pairs))
  safe_csv(do.call(rbind,obs_pairs),
           file.path(OUT,"REVIEWER2_Observed_date_matched_TN_TKN_Bounds.csv"))

# Log discharge zeros at the *raw input* level for each unique input file.
flow_status <- list()
for (f in unique(config$flow_file)) {
  flow_status[[f]] <- tryCatch(
    flow_audit(file.path(FLOW_DIR,f),file.path(OUT,
                       paste0("AUDIT_",slug(f),"_discharge.csv"))),
    error=function(e){record_issue(paste("FLOW AUDIT ERROR",f,
                      conditionMessage(e)));NULL})
}

write_plain(c(
  "READ BEFORE SUBMISSION",
  "1. Only EGRET WRTDS is computed here. WRTDStidal salinity analysis requires its own", 
  "   independently corrected code using ORIGINAL sample responses and station-specific Smax.",
  "2. No custom low-flow redistribution. Existing Final.CSV zeros may be RECORDED REVERSE FLOWS.",
  "3. Flux values must be checked against source units and paired gauge/site drainage areas.",
  "4. Water years (Oct-Sep); 2024 water year may include 2023 fall sampling but is not always supported.",
  "5. Missing 95% confidence intervals are NA, not zero. Switch on both EGRETci modes",
  "   and verify diagnostic stability before submission.",
  "6. Table 1-4 are NEW results from THESE input files and current software:",
  "   they need not reproduce archived submission until all processing decisions match.",
  "7. EGRET fluxUnit=8: 10^3 kg/year; 5: short tons/year; input flow qUnit=1 cfs.",
  "8. Verify station IDs, analytical methods, and censoring for TN/TKN pairs.",
  "9. Does NOT calculate signed net transport or reproduce WRTDStidal code.",
  if (!HYDROLOGY_VALIDATED) "10. HYDROLOGY_VALIDATED=FALSE: these outputs are EXPLORATORY only."
  else "10. Set HYDROLOGY_VALIDATED=TRUE by user; supporting methods must be deposited."
), file.path(OUT, "00_PUBLICATION_VALIDATION_REQUIRED.txt"))

# Preserve environment information, settings, and run warnings.
write_plain(capture.output(sessionInfo()),file.path(OUT,"00_R_session_info.txt"))
write_plain(c(paste("Run timestamp:",RUN_TAG),
  paste("Reused fits:",USE_SAVED_MODELS),
  paste("Input Q unit: cfs; qUnit=",INPUT_Q_UNIT),
  paste("Period: water year starting month",PA_START),
  paste("Year points:",paste(YEAR_POINTS,collapse=",")),
  paste("Annual flux conversion: kg/day * 365.25 / 1000"),
  paste("EGRETci annual bands:",RUN_CI_BANDS),
  paste("EGRETci change bands:",RUN_CI_CHANGES),
  paste("Hydrology validated:",HYDROLOGY_VALIDATED)),
  file.path(OUT,"00_run_settings.txt"))
write_plain(if(length(issue_log)) issue_log else "No additional audit notices",
            file.path(OUT,"00_run_notices.txt"))
cat("\nRUN ATTEMPT COMPLETED. OUTPUT:", OUT, "\n")
cat("Models completed:", length(table1_list), "/12\n")
cat("Verified artifacts:",sum(vapply(export_status,isTRUE,logical(1))),
    "/",length(export_status),"\n")
cat("Figures: 600-dpi TIFF + vector PDF under Figures_5_6_7_Manuscript_PrintSize\n")
cat("Supplement: 8-panel journal-size TIFF + PDF under Supplementary_Diagnostics\n")
cat("Tables: diagnostics and modelled change, units 10^3 kg/year\n")
if (length(issue_log)) cat("CHECK NOTICES: 00_run_notices.txt\n")
if (!all(vapply(export_status,isTRUE,logical(1))))
  warning("Some tables/figures failed; inspect 00_export_status.csv and 00_run_notices.txt")
if (!(RUN_CI_BANDS && RUN_CI_CHANGES))
  cat("WARNING: Genuine reviewer-required CIs are NOT complete. Enable EGRETci options.\n")
if (!HYDROLOGY_VALIDATED)
  cat("WARNING: Results remain exploratory: reverse flows/gauge comparability unvalidated.\n")
