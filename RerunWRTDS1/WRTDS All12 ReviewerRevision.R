# ============================================================================
# ST. JOHNS RIVER - EGRET / WRTDS REVIEWER REVISION V10
# Manuscript ECOLIND-62759
#
# Updated from the supplied V9 PrintSafeSupp script. Run from the beginning.
# V9 main-figure drawing, eight-panel diagnostics, and split pages are retained.
# The CI/checkpoint engine includes endpoint guards and model provenance checks.
# Table 1: diagnostics. Tables 2-4: changes and bootstrap 95% change intervals.
# Figures 5-7 and supplementary diagnostics: 600-dpi TIFF and vector PDF.
#
# DEFAULTS:
#   - Reuse the latest complete prior set of 12 fitted models when available.
#   - Fit missing models from the original inputs when RUN_MODELS = TRUE.
#   - Calculate change CIs for all ten requested windows, with 1000 replicates.
#   - Leave annual figure bands off; enable RUN_CI_BANDS separately if desired.
#   - Preserve the original comparison years; never silently move an endpoint.
#
# Unsupported or failed CIs remain NA with an explicit CI_status and CI_note.
# Original input files and previous run directories are never overwritten.
# WRTDStidal is a separate analysis and is not implemented here.
# ============================================================================

# ------------------------------- SETTINGS -----------------------------------
ROOT <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS"
FLOW_DIR <- file.path(ROOT, "Discharge")
WQ_DIR <- file.path(ROOT, "Water Quality")
OUTPUT_PARENT <- file.path(ROOT, "Reviewer_Revision_WRTDS")

USE_SAVED_MODELS <- TRUE
# "" = find the latest prior Rerun_* directory containing all 12 fitted models.
# Alternatively, enter the exact prior Models directory, for example:
# SAVED_MODELS_DIR <- file.path(OUTPUT_PARENT, "Rerun_20261024_123456", "Models")
# The timestamp above is an illustration; use your actual directory name.
SAVED_MODELS_DIR <- ""
RUN_MODELS <- TRUE             # Allow fresh fits if a reusable fit is absent.
REUSE_MATCHING_BOOTSTRAPS <- TRUE

INPUT_Q_UNIT <- 1L             # 1 = cfs; 2 = m^3/s. Internal EGRET Q is m^3/s.
ZERO_ADJUST <- TRUE            # Preserve the V9 discharge-import scenario.
PA_START <- 10L                # October-September water years.
PA_LONG <- 12L
YEAR_POINTS <- c(2004L, 2009L, 2014L, 2019L, 2024L)
FIG_START <- 2000L
FIG_END <- 2024L

MODEL_WINDOW_Y <- 7
MODEL_WINDOW_Q <- 2
MODEL_WINDOW_S <- 0.5
MODEL_MIN_OBS <- 100L
MODEL_MIN_UNCEN <- 50L
MODEL_EDGE_ADJUST <- TRUE

RUN_CI_CHANGES <- TRUE         # Tables 2-4: ENABLED.
RUN_CI_BANDS <- FALSE          # Annual figure bands: separate optional step.
CI_N_BOOT <- 1000L             # Use 100L for an initial run if needed.
CI_BLOCK_LENGTH <- 200L
CI_MIN_GOOD_FRACTION <- 0.90
CI_SEED <- 494817L

# QC flags request review; they do not cap predictions or alter the fits.
QC_HIGH_CENSOR_PCT <- 50
QC_EXTREME_ROWS <- 20L
CI_WINDOWS <- combn(YEAR_POINTS, 2L, simplify = FALSE)
# For a short workflow trial, replace the preceding line with:
# CI_WINDOWS <- list(c(2004L, 2019L))
# Unrequested windows will then be explicitly marked NOT_REQUESTED.

MAKE_MAIN_FIGURES <- TRUE
MAKE_SUPP_DIAGNOSTICS <- TRUE
DPI <- 600L
HYDROLOGY_VALIDATED <- FALSE   # Reporting flag only; not a validation procedure.

config <- data.frame(
  site = rep(c("Rockledge", "Sanford", "Cow Creek", "Jacksonville"), each = 3L),
  analyte = rep(c("TKN", "TN", "TP"), times = 4L),
  flow_file = rep(c("Cocoa_Final.CSV", "Sanford_Final.CSV",
                    "Satsuma_Final.CSV", "Jacksonville_Final.CSV"), each = 3L),
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

# --------------------------- GENERAL HELPERS --------------------------------
slug <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
series_key <- function(site, analyte) paste(site, analyte, sep = "_")
model_folder <- function(site, analyte) paste(slug(site), analyte, sep = "_")
window_key <- function(y1, y2) paste(y1, y2, sep = "_")
water_year <- function(dates) {
  dates <- as.Date(dates)
  as.integer(format(dates, "%Y")) +
    as.integer(as.integer(format(dates, "%m")) >= 10L)
}
wy_start <- function(year) as.Date(sprintf("%d-10-01", as.integer(year) - 1L))
wy_end <- function(year) as.Date(sprintf("%d-09-30", as.integer(year)))

run_log <- new.env(parent = emptyenv())
run_log$messages <- character()
run_log$lines <- character()
record_issue <- function(msg) {
  msg <- paste(as.character(msg), collapse = " ")
  if (!(msg %in% run_log$messages)) {
    run_log$messages <- c(run_log$messages, msg)
    run_log$lines <- c(run_log$lines,
                       paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
    message("NOTICE: ", msg)
  }
  invisible(NULL)
}
with_logged_warnings <- function(expr, context) {
  withCallingHandlers(expr, warning = function(w) {
    record_issue(paste(context, "WARNING:", conditionMessage(w)))
    invokeRestart("muffleWarning")
  })
}
write_plain <- function(x, path) {
  writeLines(as.character(x), path, useBytes = TRUE)
}
safe_csv <- function(dat, path) {
  ok <- tryCatch({
    write.csv(dat, path, row.names = FALSE, na = "")
    file.exists(path) && is.finite(file.info(path)$size) && file.info(path)$size > 0
  }, error = function(e) {
    record_issue(paste("CSV WRITE FAILED:", path, conditionMessage(e)))
    FALSE
  })
  invisible(isTRUE(ok))
}
save_rds <- function(object, path) {
  # A temporary file prevents an interrupted save from becoming a valid cache.
  tmp <- tempfile("rds_", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp)
  if (!file.rename(tmp, path)) {
    # Windows may refuse rename over an existing file in this new run folder.
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("RDS write failed: ", path)
  }
  invisible(path)
}
log_console <- function(expr, path) {
  con <- file(path, open = "wt")
  n_sink <- sink.number()
  on.exit({
    while (sink.number() > n_sink) sink()
    close(con)
  }, add = TRUE)
  sink(con, split = TRUE)
  force(expr)
}
md5_if_exists <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) return(NA_character_)
  unname(as.character(tools::md5sum(path)))
}
new_output_dir <- function(parent) {
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  tag <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out <- file.path(parent, paste0("Rerun_", tag))
  k <- 0L
  while (file.exists(out)) {
    k <- k + 1L
    out <- file.path(parent, paste0("Rerun_", tag, "_", k))
  }
  if (!dir.create(out, recursive = TRUE, showWarnings = FALSE))
    stop("Cannot create output directory: ", out)
  probe <- file.path(out, ".write_permission_test")
  writeLines("ok", probe)
  unlink(probe)
  out
}
find_saved_models <- function(parent, explicit_dir, use_saved) {
  if (!isTRUE(use_saved)) return("")
  if (nzchar(trimws(explicit_dir))) {
    if (!dir.exists(explicit_dir)) stop("SAVED_MODELS_DIR does not exist: ", explicit_dir)
    return(normalizePath(explicit_dir, winslash = "/", mustWork = TRUE))
  }
  if (!dir.exists(parent)) return("")
  runs <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
  runs <- runs[grepl("^Rerun_", basename(runs))]
  candidates <- c(file.path(runs, "Models"), file.path(parent, "Models"))
  expected <- file.path(model_folder(config$site, config$analyte),
                        "EGRET_fitted_eList.rds")
  complete <- vapply(candidates, function(p) all(file.exists(file.path(p, expected))),
                     logical(1))
  candidates <- candidates[complete]
  if (!length(candidates)) return("")
  # The timestamped folder name makes selection visible and reproducible.
  candidates <- candidates[order(basename(dirname(candidates)), decreasing = TRUE)]
  normalizePath(candidates[1L], winslash = "/", mustWork = TRUE)
}

validate_settings <- function() {
  stopifnot(nrow(config) == 12L,
            !anyDuplicated(series_key(config$site, config$analyte)),
            identical(PA_START, 10L), identical(PA_LONG, 12L),
            length(YEAR_POINTS) >= 2L, !anyDuplicated(YEAR_POINTS),
            all(diff(YEAR_POINTS) > 0), FIG_END >= FIG_START,
            INPUT_Q_UNIT %in% c(1L, 2L),
            CI_N_BOOT >= 10L, CI_N_BOOT == as.integer(CI_N_BOOT),
            CI_BLOCK_LENGTH > 0, CI_MIN_GOOD_FRACTION > 0,
            CI_MIN_GOOD_FRACTION <= 1, length(CI_WINDOWS) > 0L)
  valid <- vapply(CI_WINDOWS, function(w) {
    length(w) == 2L && all(is.finite(w)) &&
      all(w %in% YEAR_POINTS) && w[1L] < w[2L]
  }, logical(1))
  if (!all(valid)) stop("Every CI window must be an ordered pair from YEAR_POINTS")
  keys <- vapply(CI_WINDOWS, function(w) window_key(w[1L], w[2L]), character(1))
  if (anyDuplicated(keys)) stop("Duplicate CI_WINDOWS")
  if (!requireNamespace("EGRET", quietly = TRUE))
    stop("Install EGRET first: install.packages('EGRET')")
  if ((RUN_CI_CHANGES || RUN_CI_BANDS) && !requireNamespace("EGRETci", quietly = TRUE))
    stop("Install EGRETci first: install.packages('EGRETci')")
}

blank_changes <- function(site, analyte) {
  pairs <- t(combn(YEAR_POINTS, 2L))
  n <- nrow(pairs)
  out <- data.frame(Site = rep(site, n), Constituent = rep(analyte, n),
                     From_WY = pairs[, 1L], To_WY = pairs[, 2L],
                     stringsAsFactors = FALSE)
  point_fields <- c("Concentration_change_mgL", "Concentration_slope_mgL_per_year",
    "Concentration_change_pct", "Concentration_slope_pct_per_year",
    "Flux_change_1000kg_per_year", "Flux_slope_1000kg_per_year_per_year",
    "Flux_change_pct", "Flux_slope_pct_per_year")
  ci_fields <- c("Conc_change_CI95_low_mgL", "Conc_change_CI95_high_mgL",
    "Flux_change_CI95_low_1000kgyr", "Flux_change_CI95_high_1000kgyr")
  for (nm in c(point_fields, ci_fields)) out[[nm]] <- NA_real_
  out$CI_method <- "NOT_COMPUTED"
  out$CI_status <- "PENDING_MODEL"
  out$CI_note <- ""
  out$CI_n_good <- NA_integer_
  out$n_samples_From_WY <- NA_integer_
  out$n_samples_To_WY <- NA_integer_
  out$Model_QC_review_flag <- NA
  out$Model_QC_note <- ""
  out
}

extract_diagnostics <- function(eList, site, analyte) {
  s <- eList$Sample
  st <- EGRET::errorStats(eList)
  required <- c("RsqLogC", "RsqLogF", "rmse", "sepPercent")
  if (!all(required %in% names(st)))
    stop("Unexpected errorStats names: ", paste(names(st), collapse = ", "))
  fb <- EGRET::fluxBiasStat(s)
  n_cen <- sum(s$Uncen == 0L, na.rm = TRUE)
  data.frame(Site = site, Constituent = analyte, n = nrow(s),
    n_censored = n_cen, censored_pct = round(100 * n_cen / nrow(s), 2L),
    first_sample_date = as.character(min(s$Date)),
    last_sample_date = as.character(max(s$Date)),
    n_samples_WY2024 = sum(water_year(s$Date) == 2024L),
    RsqLogC = as.numeric(st$RsqLogC[1L]), RsqLogF = as.numeric(st$RsqLogF[1L]),
    RMSE_ln = as.numeric(st$rmse[1L]), SEP_percent = as.numeric(st$sepPercent[1L]),
    Flux_bias_statistic = if (length(fb) >= 3L) as.numeric(fb[3L]) else NA_real_,
    stringsAsFactors = FALSE)
}
annual_results <- function(eList) {
  a <- EGRET::setupYears(eList$Daily, paLong = PA_LONG, paStart = PA_START)
  required <- c("DecYear", "Conc", "FNConc", "Flux", "FNFlux")
  if (!all(required %in% names(a))) stop("Unexpected setupYears columns")
  out <- data.frame(WaterYear = as.integer(floor(a$DecYear)), DecYear = a$DecYear,
    Conc_mgL = a$Conc, FNConc_mgL = a$FNConc,
    Flux_1000kgyr = a$Flux * 365.25 / 1000,
    FNFlux_1000kgyr = a$FNFlux * 365.25 / 1000)
  if (anyDuplicated(out$WaterYear)) stop("Duplicate water years in annual estimates")
  out
}
change_results <- function(eList, site, analyte, dest) {
  # Preserve the V9 tableChangeSingle point estimates, including its rounding.
  # Exact, unrounded estimates remain in annual_EGRET_estimates.csv and are
  # used for checking that the bootstrap targets the same fitted change.
  cc <- ff <- NULL
  capture.output(cc <- EGRET::tableChangeSingle(eList, fluxUnit = 8,
    yearPoints = YEAR_POINTS, flux = FALSE),
    file = file.path(dest, "EGRET_concentration_changes_console.txt"))
  capture.output(ff <- EGRET::tableChangeSingle(eList, fluxUnit = 8,
    yearPoints = YEAR_POINTS, flux = TRUE),
    file = file.path(dest, "EGRET_flux_changes_console.txt"))
  if (ncol(cc) != 6L || ncol(ff) != 6L) stop("Unexpected tableChangeSingle columns")
  cc <- as.data.frame(cc, stringsAsFactors = FALSE)
  ff <- as.data.frame(ff, stringsAsFactors = FALSE)
  cc[] <- lapply(cc, function(x) as.numeric(as.character(x)))
  ff[] <- lapply(ff, function(x) as.numeric(as.character(x)))
  out <- blank_changes(site, analyte)
  wanted <- window_key(out$From_WY, out$To_WY)
  kc <- window_key(cc[[1L]], cc[[2L]])
  kf <- window_key(ff[[1L]], ff[[2L]])
  if (anyDuplicated(kc) || anyDuplicated(kf) ||
      !setequal(wanted, kc) || !setequal(wanted, kf))
    stop("EGRET changed the requested comparison years for ", site, " ", analyte)
  out[, 5L:8L] <- cc[match(wanted, kc), 3L:6L, drop = FALSE]
  out[, 9L:12L] <- ff[match(wanted, kf), 3L:6L, drop = FALSE]
  sy <- water_year(eList$Sample$Date)
  out$n_samples_From_WY <- vapply(out$From_WY, function(y) sum(sy == y), integer(1))
  out$n_samples_To_WY <- vapply(out$To_WY, function(y) sum(sy == y), integer(1))
  requested <- vapply(CI_WINDOWS, function(w) window_key(w[1L], w[2L]), character(1))
  out$CI_status <- ifelse(RUN_CI_CHANGES & wanted %in% requested,
                           "PENDING", "NOT_REQUESTED")
  out
}

sample_coverage <- function(eList, site, analyte) {
  sy <- water_year(eList$Sample$Date)
  dy <- water_year(eList$Daily$Date)
  yrs <- seq.int(min(c(sy, dy, YEAR_POINTS)), max(c(sy, dy, YEAR_POINTS)))
  do.call(rbind, lapply(yrs, function(y) {
    ii <- which(sy == y)
    data.frame(Site = site, Constituent = analyte, WaterYear = y,
      n_samples = length(ii),
      n_censored = sum(eList$Sample$Uncen[ii] == 0L, na.rm = TRUE),
      n_daily_days = sum(dy == y),
      expected_daily_days = as.integer(wy_end(y) - wy_start(y)) + 1L)
  }))
}
endpoint_reason <- function(eList, year1, year2) {
  # Mirror EGRET's sample-range guard before calling its bootstrap routines.
  # Fewer than three samples is flagged separately, not treated as a universal
  # statistical cutoff. No endpoint is changed automatically.
  sd <- as.Date(eList$Sample$Date)
  dd <- as.Date(eList$Daily$Date)
  if (wy_start(year2) > max(sd))
    return(paste0("WY", year2, " starts after the last sample (", max(sd), ")."))
  if (wy_end(year1) < min(sd))
    return(paste0("WY", year1, " ends before the first sample (", min(sd), ")."))
  for (y in c(year1, year2)) {
    needed <- seq(wy_start(y), wy_end(y), by = "day")
    if (!all(needed %in% dd))
      return(paste0("Daily discharge does not cover all days of WY", y, "."))
  }
  ""
}

bootstrap_limits <- function(boot, n_requested) {
  xc <- as.numeric(boot$xConc)
  xf <- as.numeric(boot$xFlux)
  if (!length(xc) || length(xc) != length(xf))
    stop("Bootstrap concentration and flux vectors are absent or have different lengths")
  good <- is.finite(xc) & is.finite(xf)
  n_good <- sum(good)
  if (n_good < ceiling(CI_MIN_GOOD_FRACTION * n_requested))
    stop("Too few valid paired bootstrap replicates: ", n_good, "/", n_requested)
  # EGRETci uses quantile(type=6). Flux here is in 10^6 kg/year, whereas
  # Tables 2-4 use 10^3 kg/year. Apply the factor 1000 exactly once.
  qc <- stats::quantile(xc[good], c(0.025, 0.975), type = 6,
                         names = FALSE, na.rm = TRUE)
  qf <- stats::quantile(xf[good], c(0.025, 0.975), type = 6,
                         names = FALSE, na.rm = TRUE) * 1000
  if (any(!is.finite(c(qc, qf))) || qc[1L] > qc[2L] || qf[1L] > qf[2L])
    stop("Invalid bootstrap interval limits")
  list(conc = qc, flux = qf, n_good = n_good,
       median_conc = stats::median(xc[good]),
       median_flux = stats::median(xf[good]) * 1000)
}
annual_with_ci <- function(annual, ci, sample_dates) {
  required <- c("Year", "FNConcLow", "FNConcHigh", "FNFluxLow", "FNFluxHigh")
  if (!all(required %in% names(ci))) stop("Unexpected annual CI columns")
  ci_year <- as.integer(floor(as.numeric(ci$Year)))
  if (anyDuplicated(ci_year)) stop("Duplicate annual CI years")
  m <- match(annual$WaterYear, ci_year)
  if (all(is.na(m))) stop("Annual confidence bands do not match water years")
  annual$FNConc_L95 <- as.numeric(ci$FNConcLow[m])
  annual$FNConc_U95 <- as.numeric(ci$FNConcHigh[m])
  annual$FNFlux_L95_1000kgyr <- as.numeric(ci$FNFluxLow[m]) * 365.25 / 1000
  annual$FNFlux_U95_1000kgyr <- as.numeric(ci$FNFluxHigh[m]) * 365.25 / 1000
  supported <- annual$WaterYear >= min(water_year(sample_dates)) &
                 annual$WaterYear <= max(water_year(sample_dates))
  ci_names <- c("FNConc_L95", "FNConc_U95", "FNFlux_L95_1000kgyr", "FNFlux_U95_1000kgyr")
  annual[!supported, ci_names] <- NA_real_
  annual
}

# ---------------- FIGURES 5-7: PRESERVED FROM UPLOADED V9 ----------------
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

make_figure <- function(analyte, annual_list, out) {
  OUT <- out
  number <- switch(analyte, TKN = 5L, TN = 6L, TP = 7L,
                   stop("Unknown analyte: ", analyte))
  dat <- lapply(FIG_SITE_ORDER, function(site)
    annual_list[[paste(site, analyte, sep = "_")]])
  missing <- FIG_SITE_ORDER[vapply(dat, is.null, logical(1))]
  if (length(missing)) {
    stop("Figure ", number, " not generated: missing annual results for ",
         paste(missing, collapse = ", "))
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
  invisible(list(tiff=tiffile, pdf=pdffile))
}


# ------------ PRINT-SAFE SUPPLEMENT: PRESERVED V9 DRAWING FUNCTIONS --------
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
                   "_v10_PrintSafe")
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

# ------------------ MODEL QC: INDEPENDENT OF FIGURE EXPORT -------------------
empty_model_qc <- function(site, analyte) {
  data.frame(Site = site, Constituent = analyte,
    Sample_count = NA_integer_, Censored_count = NA_integer_,
    Reported_maximum_mgL = NA_real_, Model_sample_maximum_mgL = NA_real_,
    Model_daily_maximum_mgL = NA_real_, Sample_fit_exceeds_reported_max = NA,
    Daily_fit_exceeds_reported_max = NA, Censored_pct = NA_real_,
    Detected_maximum_mgL = NA_real_, Sample_fit_to_reported_max_ratio = NA_real_,
    Daily_fit_to_reported_max_ratio = NA_real_,
    Nonfinite_sample_predictions = NA_integer_, Nonfinite_daily_predictions = NA_integer_,
    Peak_sample_prediction_date = NA_character_, Peak_daily_prediction_date = NA_character_,
    SEP_percent = NA_real_, Flux_bias_statistic = NA_real_,
    Model_QC_review_flag = NA, QC_status = "PENDING_MODEL", QC_note = "",
    stringsAsFactors = FALSE)
}
collect_model_qc <- function(eList, site, analyte, diag, jobdir) {
  # Retain the original nine V9 QC fields and add review context separately.
  base_qc <- supp_validate_results(eList, site, analyte)
  qc <- empty_model_qc(site, analyte)
  qc[, names(base_qc)] <- base_qc
  s <- eList$Sample
  d <- eList$Daily
  sx <- as.numeric(s$ConcHat)
  dx <- as.numeric(d$ConcDay)
  max_finite <- function(x) {
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    if (length(x)) max(x) else NA_real_
  }
  peak_date <- function(frame, values) {
    ii <- which(is.finite(values))
    if (!length(ii) || !("Date" %in% names(frame))) return(NA_character_)
    as.character(as.Date(frame$Date[ii[which.max(values[ii])]]))
  }
  qc$Censored_pct <- 100 * qc$Censored_count / qc$Sample_count
  qc$Detected_maximum_mgL <- max_finite(s$ConcHigh[!is.na(s$Uncen) & s$Uncen == 1L])
  if (is.finite(qc$Reported_maximum_mgL) && qc$Reported_maximum_mgL > 0) {
    qc$Sample_fit_to_reported_max_ratio <- qc$Model_sample_maximum_mgL / qc$Reported_maximum_mgL
    qc$Daily_fit_to_reported_max_ratio <- qc$Model_daily_maximum_mgL / qc$Reported_maximum_mgL
  }
  qc$Nonfinite_sample_predictions <- sum(!is.finite(sx))
  qc$Nonfinite_daily_predictions <- sum(!is.finite(dx))
  qc$Peak_sample_prediction_date <- peak_date(s, sx)
  qc$Peak_daily_prediction_date <- peak_date(d, dx)
  for (nm in c("SEP_percent", "Flux_bias_statistic"))
    if (nm %in% names(diag)) qc[[nm]] <- as.numeric(diag[[nm]][1L])
  notes <- character()
  if (isTRUE(qc$Sample_fit_exceeds_reported_max))
    notes <- c(notes, "Sample-date fitted maximum exceeds the reported maximum.")
  if (isTRUE(qc$Daily_fit_exceeds_reported_max))
    notes <- c(notes, "Daily fitted maximum exceeds the reported maximum.")
  if (is.finite(qc$Censored_pct) && qc$Censored_pct >= QC_HIGH_CENSOR_PCT)
    notes <- c(notes, paste0("Censoring is at or above the configured ",
                             QC_HIGH_CENSOR_PCT, "% review flag threshold."))
  if (qc$Nonfinite_sample_predictions || qc$Nonfinite_daily_predictions)
    notes <- c(notes, "Nonfinite fitted predictions require inspection.")
  qc$Model_QC_review_flag <- length(notes) > 0L
  qc$QC_status <- if (qc$Model_QC_review_flag) "REVIEW_REQUIRED" else "NO_SCREENING_FLAGS"
  qc$QC_note <- if (length(notes)) paste(notes, collapse = " ") else
    "No flags from these limited maximum/censoring screens; overall model validity is not established."
  safe_csv(qc, file.path(jobdir, "model_QC_summary.csv"))

  export_extremes <- function(frame, values, label) {
    finite <- which(is.finite(values))
    ord <- finite[order(values[finite], decreasing = TRUE)]
    top <- head(ord, QC_EXTREME_ROWS)
    above <- finite[is.finite(qc$Reported_maximum_mgL) &
                      values[finite] > qc$Reported_maximum_mgL]
    safe_csv(frame[top, , drop = FALSE],
              file.path(jobdir, paste0(label, "_largest_predictions.csv")))
    safe_csv(frame[above, , drop = FALSE],
              file.path(jobdir, paste0(label, "_predictions_above_reported_max.csv")))
  }
  export_extremes(s, sx, "Sample")
  export_extremes(d, dx, "Daily")
  write_plain(c(
    "These files retain the fitted values and original model-data columns.",
    "Reported_maximum_mgL is the maximum Sample ConcHigh, including nondetect limits.",
    "Detected_maximum_mgL uses uncensored records only.",
    "Maximum exceedance and high censoring are review screens, not automatic invalidity tests.",
    "Inspect dates, Q, detection limits, and model diagnostics for large predictions.",
    "No prediction is capped, deleted, or replaced by these QC exports.",
    "A computed CI quantifies uncertainty conditional on this fit; it does not resolve model bias."
  ), file.path(jobdir, "model_QC_notes.txt"))
  qc
}

# ----------------------- MODEL INPUTS AND PROVENANCE -------------------------
validate_fitted_model <- function(eList, site, analyte) {
  if (!all(c("Daily", "Sample", "INFO") %in% names(eList)))
    stop("Saved model lacks Daily, Sample, or INFO")
  if (!all(c("Date", "Q", "LogQ", "ConcDay", "FluxDay", "FNConc", "FNFlux") %in%
           names(eList$Daily))) stop("Model Daily predictions are incomplete")
  if (!all(c("Date", "Uncen", "ConcHigh") %in% names(eList$Sample)) ||
      !nrow(eList$Sample)) stop("Model Sample data are incomplete")
  d <- eList$Daily
  if (anyNA(d$Date) || anyDuplicated(d$Date) || any(diff(as.Date(d$Date)) != 1))
    stop("Model Daily dates are missing, duplicated, or nonconsecutive")
  if (any(!is.finite(d$Q)) || any(d$Q <= 0) || any(!is.finite(d$LogQ)))
    stop("Invalid discharge or LogQ in fitted model")
  if (!isTRUE(all.equal(as.numeric(d$LogQ), log(as.numeric(d$Q)),
                        tolerance = 1e-7, check.attributes = FALSE)))
    stop("Daily LogQ differs from log(Q)")
  if (anyNA(eList$Sample$Date)) stop("Missing sample dates")
  if (isTRUE(attr(eList, "runSeries")))
    stop("This script expects the V9 stationary-flow fit, not a runSeries model")
  nm <- eList$INFO$shortName
  an <- eList$INFO$paramShortName
  if (length(nm) == 1L && !is.na(nm) && nm != site)
    stop("Saved model site differs from configuration: ", nm, " versus ", site)
  if (length(an) == 1L && !is.na(an) && an != analyte)
    stop("Saved model constituent differs from configuration: ", an, " versus ", analyte)
  invisible(TRUE)
}
model_parameters <- function(eList) {
  defaults <- list(windowY = MODEL_WINDOW_Y, windowQ = MODEL_WINDOW_Q,
    windowS = MODEL_WINDOW_S, minNumObs = MODEL_MIN_OBS,
    minNumUncen = MODEL_MIN_UNCEN, edgeAdjust = MODEL_EDGE_ADJUST)
  for (nm in names(defaults)) {
    v <- eList$INFO[[nm]]
    if (length(v) == 1L && !is.na(v)) defaults[[nm]] <- v
  }
  defaults
}
load_or_fit_model <- function(row, saved_dir, jobdir) {
  site <- row$site
  analyte <- row$analyte
  key <- series_key(site, analyte)
  source_job <- if (nzchar(saved_dir)) file.path(saved_dir, model_folder(site, analyte)) else ""
  saved_path <- if (nzchar(source_job)) file.path(source_job, "EGRET_fitted_eList.rds") else ""
  reused <- USE_SAVED_MODELS && nzchar(saved_path) && file.exists(saved_path)
  paths <- c(sample = file.path(WQ_DIR, row$sample_file),
             flow = file.path(FLOW_DIR, row$flow_file),
             info = file.path(WQ_DIR, row$info_file))
  current_hash <- vapply(paths, md5_if_exists, character(1))
  old_prov <- NULL
  if (reused) {
    p <- file.path(source_job, "input_provenance.csv")
    if (file.exists(p)) old_prov <- read.csv(p, stringsAsFactors = FALSE)
    for (nm in names(paths)) {
      col <- paste0(nm, "_MD5")
      if (!is.null(old_prov) && nrow(old_prov) && col %in% names(old_prov)) {
        old_hash <- as.character(old_prov[[col]][1L])
        if (!is.na(old_hash) && nzchar(old_hash) && !is.na(current_hash[[nm]]) &&
            old_hash != current_hash[[nm]])
          stop("Current ", nm, " input differs from saved-model provenance for ", key,
               ". Set USE_SAVED_MODELS <- FALSE to refit consistently from current inputs.")
      }
    }
    eList <- readRDS(saved_path)
    validate_fitted_model(eList, site, analyte)
    message("Reusing fitted model: ", saved_path)
    if (is.null(old_prov) || !nrow(old_prov))
      record_issue(paste(key, "saved-model source input checksums are unavailable"))
    if (!is.null(old_prov))
      safe_csv(old_prov, file.path(jobdir, "reused_model_input_provenance.csv"))
  } else {
    if (!RUN_MODELS) stop("No reusable fit and RUN_MODELS is FALSE: ", key)
    if (!all(file.exists(paths)))
      stop("Missing original input files: ", paste(paths[!file.exists(paths)], collapse = "; "))
    message("Fitting original inputs for ", key)
    Sample <- EGRET::readUserSample(WQ_DIR, row$sample_file, separator = ",")
    Daily <- EGRET::readUserDaily(FLOW_DIR, row$flow_file, qUnit = INPUT_Q_UNIT,
                                 adjust = ZERO_ADJUST, fill = FALSE)
    if (any(!is.finite(Daily$Q)) || any(Daily$Q <= 0) || any(!is.finite(Daily$LogQ)))
      stop("Invalid discharge after import for ", key)
    INFO <- EGRET::readUserInfo(WQ_DIR, row$info_file, interactive = FALSE)
    INFO$shortName <- site
    INFO$station.nm <- site
    INFO$paramShortName <- analyte
    INFO$constitAbbrev <- analyte
    INFO$param.nm <- switch(analyte, TKN = "Total Kjeldahl nitrogen",
                            TN = "Total nitrogen", TP = "Total phosphorus")
    INFO$staAbbrev <- switch(site, Rockledge = "RLC", Sanford = "SanF",
                             "Cow Creek" = "CoCr", Jacksonville = "Jack")
    INFO$param.units <- "mg/l"
    eList <- EGRET::mergeReport(INFO, Daily, Sample)
    eList <- EGRET::setPA(eList, paStart = PA_START, paLong = PA_LONG)
    save_rds(list(INFO = INFO, Daily = Daily, Sample = Sample),
              file.path(jobdir, "exact_model_inputs.rds"))
    eList <- EGRET::modelEstimation(eList, windowY = MODEL_WINDOW_Y,
      windowQ = MODEL_WINDOW_Q, windowS = MODEL_WINDOW_S,
      minNumObs = MODEL_MIN_OBS, minNumUncen = MODEL_MIN_UNCEN,
      edgeAdjust = MODEL_EDGE_ADJUST)
    validate_fitted_model(eList, site, analyte)
  }
  eList <- EGRET::setPA(eList, paStart = PA_START, paLong = PA_LONG)
  target <- file.path(jobdir, "EGRET_fitted_eList.rds")
  save_rds(eList, target)
  provenance <- data.frame(Site = site, Constituent = analyte,
    origin = if (reused) "REUSED_SAVED_FIT" else "FRESH_RUN",
    source_saved_model = if (reused) saved_path else "",
    input_flow_file = row$flow_file, input_sample_file = row$sample_file,
    input_info_file = row$info_file, stringsAsFactors = FALSE)
  for (nm in names(paths)) {
    col <- paste0(nm, "_MD5")
    original_hash <- if (!reused) current_hash[[nm]] else NA_character_
    if (reused && !is.null(old_prov) && nrow(old_prov) && col %in% names(old_prov))
      original_hash <- as.character(old_prov[[col]][1L])
    provenance[[col]] <- original_hash
    provenance[[paste0("current_", col)]] <- current_hash[[nm]]
  }
  safe_csv(provenance, file.path(jobdir, "input_provenance.csv"))
  save_rds(model_parameters(eList), file.path(jobdir, "model_fitting_parameters.rds"))
  list(eList = eList, reused = reused, source_job = source_job,
       origin = provenance$origin, model_md5 = md5_if_exists(target))
}

flow_audit <- function(flow_path, out_path) {
  raw <- read.csv(flow_path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(raw) < 2L) stop("Need Date and discharge columns: ", flow_path)
  ds <- as.Date(as.character(raw[[1L]]), format = "%m/%d/%Y")
  iso <- as.Date(as.character(raw[[1L]]), format = "%Y-%m-%d")
  ds[is.na(ds)] <- iso[is.na(ds)]
  q <- suppressWarnings(as.numeric(raw[[2L]]))
  s <- data.frame(file = basename(flow_path), n_days = length(q),
    first_date = if (all(is.na(ds))) NA_character_ else as.character(min(ds, na.rm = TRUE)),
    last_date = if (all(is.na(ds))) NA_character_ else as.character(max(ds, na.rm = TRUE)),
    n_zero = sum(q == 0, na.rm = TRUE), pct_zero = 100 * mean(q == 0, na.rm = TRUE),
    n_negative = sum(q < 0, na.rm = TRUE), n_missing = sum(is.na(q)),
    n_bad_dates = sum(is.na(ds)), n_duplicate_dates = sum(duplicated(ds)),
    stringsAsFactors = FALSE)
  safe_csv(s, out_path)
  if (anyNA(ds) || anyNA(q) || anyDuplicated(ds)) stop("Bad dates/flows in ", flow_path)
  if (any(q < 0)) stop("Negative signed Q exists in ", flow_path,
                        "; this script does not reconstruct reverse-flow scenarios")
  if (any(diff(ds) != 1)) stop("Daily discharge dates are not consecutive: ", flow_path)
  s
}
reviewer_consistency_checks <- function(fit_list, annual_list, out) {
  model_pairs <- obs_pairs <- list()
  for (site in FIG_SITE_ORDER) {
    tn <- annual_list[[series_key(site, "TN")]]
    kn <- annual_list[[series_key(site, "TKN")]]
    if (!is.null(tn) && !is.null(kn)) {
      m <- merge(tn[, c("WaterYear", "FNConc_mgL")], kn[, c("WaterYear", "FNConc_mgL")],
        by = "WaterYear", suffixes = c("_TN", "_TKN"))
      m$Site <- site
      m$TN_minus_TKN_mgL <- m$FNConc_mgL_TN - m$FNConc_mgL_TKN
      m$Flag_model_TN_below_TKN <- is.finite(m$TN_minus_TKN_mgL) & m$TN_minus_TKN_mgL < 0
      model_pairs[[site]] <- m
    }
    etn <- fit_list[[series_key(site, "TN")]]
    ekn <- fit_list[[series_key(site, "TKN")]]
    if (is.null(etn) || is.null(ekn)) next
    take <- function(e, postfix) {
      s <- e$Sample
      required <- c("Date", "ConcHigh", "Uncen")
      if (!all(required %in% names(s))) stop("Missing paired-sample columns")
      if (anyDuplicated(s$Date))
        record_issue(paste(site, postfix, "has same-date replicates; first record retained in date-only audit"))
      s <- s[!duplicated(s$Date), required, drop = FALSE]
      names(s) <- c("Date", paste0("Value_", postfix), paste0("Uncen_", postfix))
      s
    }
    z <- merge(take(etn, "TN"), take(ekn, "TKN"), by = "Date")
    if (!nrow(z)) next
    z$Site <- site
    z$Both_detected <- z$Uncen_TN == 1L & z$Uncen_TKN == 1L
    z$TN_minus_TKN_detected_mgL <- ifelse(z$Both_detected, z$Value_TN - z$Value_TKN, NA_real_)
    z$TN_ge_TKN_supported <- z$Uncen_TN == 1L & z$Value_TN >= z$Value_TKN
    z$Confirmed_detected_violation <- z$Both_detected & z$Value_TN < z$Value_TKN
    obs_pairs[[site]] <- z
  }
  if (length(model_pairs)) safe_csv(do.call(rbind, model_pairs),
    file.path(out, "REVIEWER2_Modelled_TN_minus_TKN_by_WY.csv"))
  if (length(obs_pairs)) safe_csv(do.call(rbind, obs_pairs),
    file.path(out, "REVIEWER2_Observed_date_matched_TN_TKN_Bounds.csv"))
  write_plain(c(
    "TN/TKN records are matched by date only; verify station, fraction, method, and activity IDs.",
    "Detected TN<TKN flags require source-data investigation, not automatic replacement.",
    "TN_ge_TKN_supported means the available detection/bound supports the ordering;",
    "FALSE does not alone establish a violation when either measurement is censored.",
    "Independent modeled annual curves are not forced to satisfy TN>=TKN."
  ), file.path(out, "REVIEWER2_TN_TKN_audit_notes.txt"))
  invisible(NULL)
}

# ----------------------- BOOTSTRAP CHANGE WORKFLOW ---------------------------
calculate_one_change_ci <- function(eList, annual, year1, year2, model_index,
                                     window_index, model_md5, jobdir, source_job) {
  j1 <- match(year1, annual$WaterYear)
  j2 <- match(year2, annual$WaterYear)
  if (anyNA(c(j1, j2))) stop("Comparison water year missing from annual estimates")
  exact_c <- annual$FNConc_mgL[j2] - annual$FNConc_mgL[j1]
  exact_f <- annual$FNFlux_1000kgyr[j2] - annual$FNFlux_1000kgyr[j1]
  pars <- model_parameters(eList)
  # Original fitted surface and the same water-year/FN settings as V9.
  pair <- do.call(EGRET::runPairs, c(list(eList = eList, year1 = year1, year2 = year2,
    windowSide = 0, oldSurface = TRUE, paStart = PA_START, paLong = PA_LONG,
    verbose = FALSE), pars))
  if (!all(c("x11", "x22") %in% names(pair)) || nrow(pair) != 2L)
    stop("Unexpected runPairs output")
  pair_c <- as.numeric(pair$x22[1L] - pair$x11[1L])
  pair_f <- as.numeric(pair$x22[2L] - pair$x11[2L]) * 1000
  tol_c <- max(1e-8, 1e-6 * max(abs(annual$FNConc_mgL[c(j1, j2)])))
  tol_f <- max(1e-5, 1e-6 * max(abs(annual$FNFlux_1000kgyr[c(j1, j2)])))
  audit <- data.frame(From_WY = year1, To_WY = year2,
    Exact_annual_change_mgL = exact_c, Pair_change_mgL = pair_c,
    Exact_annual_change_1000kgyr = exact_f, Pair_change_1000kgyr = pair_f,
    Conc_difference = pair_c - exact_c, Flux_difference_1000kgyr = pair_f - exact_f)
  safe_csv(audit, file.path(jobdir, sprintf("CI_point_estimate_check_%d_%d.csv", year1, year2)))
  if (any(!is.finite(c(exact_c, exact_f, pair_c, pair_f))) ||
      abs(pair_c - exact_c) > tol_c || abs(pair_f - exact_f) > tol_f)
    stop("Bootstrap point estimate does not match the original annual fitted change; inspect CI_point_estimate_check file")

  seed <- as.integer(CI_SEED + 1000L * model_index + window_index)
  stem <- sprintf("EGRETci_change_WY%d_%d", year1, year2)
  # This CI-engine schema is retained for compatible V7/V10 bootstrap reuse.
  meta <- list(schema = "WRTDS_V7_change_CI_1", model_md5 = model_md5,
    year1 = as.integer(year1), year2 = as.integer(year2), paStart = PA_START,
    paLong = PA_LONG, nBoot = as.integer(CI_N_BOOT),
    blockLength = as.integer(CI_BLOCK_LENGTH), seed = seed,
    egret_version = as.character(utils::packageVersion("EGRET")),
    egretci_version = as.character(utils::packageVersion("EGRETci")),
    fitting_parameters = pars)
  boot <- NULL
  reused_boot <- FALSE
  if (REUSE_MATCHING_BOOTSTRAPS && nzchar(source_job)) {
    old_boot <- file.path(source_job, paste0(stem, ".rds"))
    old_meta <- file.path(source_job, paste0(stem, "_metadata.rds"))
    if (file.exists(old_boot) && file.exists(old_meta)) {
      cached_meta <- tryCatch(readRDS(old_meta), error = function(e) NULL)
      if (identical(cached_meta, meta)) {
        boot <- readRDS(old_boot)
        reused_boot <- TRUE
        message("Reusing matching bootstrap: ", old_boot)
      }
    }
  }
  save_rds(pair, file.path(jobdir, sprintf("EGRET_pair_WY%d_%d.rds", year1, year2)))
  if (is.null(boot)) {
    # The console file preserves the actual EGRETci output for this window.
    boot <- log_console(EGRETci::runPairsBoot(eList, pairResults = pair,
      nBoot = CI_N_BOOT, startSeed = seed, blockLength = CI_BLOCK_LENGTH,
      run.parallel = FALSE), path = file.path(jobdir, paste0(stem, "_console.txt")))
  } else {
    write_plain("Matching previously computed bootstrap reused; see metadata.",
                file.path(jobdir, paste0(stem, "_console.txt")))
  }
  # Save raw replicates before checking them; failures remain inspectable.
  save_rds(boot, file.path(jobdir, paste0(stem, ".rds")))
  save_rds(meta, file.path(jobdir, paste0(stem, "_metadata.rds")))
  limits <- bootstrap_limits(boot, CI_N_BOOT)
  audit$Median_boot_change_mgL <- limits$median_conc
  audit$Median_boot_change_1000kgyr <- limits$median_flux
  audit$n_good <- limits$n_good
  audit$n_requested <- CI_N_BOOT
  audit$reused_bootstrap <- reused_boot
  safe_csv(audit, file.path(jobdir, sprintf("CI_reproducibility_%d_%d.csv", year1, year2)))
  list(limits = limits, reused = reused_boot)
}

export_tables_and_ci_status <- function(table1_list, change_list, model_status,
                                         annual_status, out) {
  exported <- list()
  exported$Table1 <- if (length(table1_list))
    safe_csv(do.call(rbind, table1_list), file.path(out, "Table_1_Model_Diagnostics.csv")) else FALSE
  for (a in c("TKN", "TN", "TP")) {
    num <- switch(a, TKN = 2L, TN = 3L, TP = 4L)
    keys <- series_key(config$site[config$analyte == a], a)
    dat <- do.call(rbind, change_list[keys])
    exported[[paste0("Table", num)]] <- safe_csv(dat,
      file.path(out, sprintf("Table_%d_%s_Trends_mgL_and_1000kg_per_year.csv", num, a)))
  }
  all_changes <- do.call(rbind, change_list)
  ci_columns <- c("Site", "Constituent", "From_WY", "To_WY", "CI_status", "CI_note",
    "CI_n_good", "n_samples_From_WY", "n_samples_To_WY", "CI_method",
    "Model_QC_review_flag", "Model_QC_note")
  safe_csv(all_changes[, ci_columns, drop = FALSE], file.path(out, "00_CI_change_status.csv"))
  safe_csv(model_status, file.path(out, "00_run_status.csv"))
  safe_csv(annual_status, file.path(out, "00_CI_annual_band_status.csv"))
  summary <- do.call(rbind, lapply(unique(all_changes$Constituent), function(a) {
    z <- all_changes[all_changes$Constituent == a, , drop = FALSE]
    data.frame(Constituent = a, Total_comparisons = nrow(z),
      Completed_CI95 = sum(z$CI_status == "COMPLETED"),
      Completed_trial_CI = sum(z$CI_status == "COMPLETED_TRIAL"),
      Failed = sum(z$CI_status == "FAILED"),
      Skipped = sum(startsWith(z$CI_status, "SKIPPED")),
      Not_requested = sum(z$CI_status == "NOT_REQUESTED"),
      Pending = sum(startsWith(z$CI_status, "PENDING")))
  }))
  safe_csv(summary, file.path(out, "00_CI_completion_summary.csv"))
  write_plain(if (length(run_log$lines)) run_log$lines else "No additional notices",
               file.path(out, "00_run_notices.txt"))
  exported
}

# ----------------------------- MAIN ANALYSIS --------------------------------
run_analysis <- function() {
  validate_settings()
  run_log$messages <- character()
  run_log$lines <- character()
  saved_dir <- find_saved_models(OUTPUT_PARENT, SAVED_MODELS_DIR, USE_SAVED_MODELS)
  out <- new_output_dir(OUTPUT_PARENT)
  cat("\nOUTPUT DIRECTORY:\n", out, "\n", sep = "")
  if (nzchar(saved_dir)) cat("SAVED MODEL SOURCE:\n", saved_dir, "\n", sep = "")
  else if (USE_SAVED_MODELS)
    message("No complete prior set of 12 models was found; fresh fits require RUN_MODELS = TRUE.")
  if (CI_N_BOOT < 100L && (RUN_CI_CHANGES || RUN_CI_BANDS))
    record_issue("CI_N_BOOT < 100: trial run only; intervals will be labeled as trial results")

  keys <- series_key(config$site, config$analyte)
  fit_list <- annual_list <- table1_list <- model_meta <- list()
  qc_list <- setNames(lapply(seq_len(nrow(config)), function(i)
    empty_model_qc(config$site[i], config$analyte[i])), keys)
  change_list <- setNames(lapply(seq_len(nrow(config)), function(i)
    blank_changes(config$site[i], config$analyte[i])), keys)
  model_status <- data.frame(Site = config$site, Constituent = config$analyte,
    Status = "PENDING", Origin = "", Source_model = "", Error = "",
    stringsAsFactors = FALSE)
  annual_status <- data.frame(Site = config$site, Constituent = config$analyte,
    Status = if (RUN_CI_BANDS) "PENDING" else "NOT_REQUESTED",
    Years_with_bands = 0L, Note = "", stringsAsFactors = FALSE)
  supp_status <- data.frame(Site = config$site, Constituent = config$analyte,
    Status = if (MAKE_SUPP_DIAGNOSTICS) "PENDING" else "NOT_REQUESTED",
    TIFF = "", PDF = "", Caption = "", Part_files = "", Error = "", stringsAsFactors = FALSE)
  figure_status <- data.frame(Artifact = paste0("Figure", 5:7),
    Status = if (MAKE_MAIN_FIGURES) "PENDING" else "NOT_REQUESTED",
    TIFF = "", PDF = "", Error = "", stringsAsFactors = FALSE)
  exports <- list()

  write_metadata <- function() {
    all_changes <- do.call(rbind, change_list)
    n_final <- sum(all_changes$CI_status == "COMPLETED")
    n_trial <- sum(all_changes$CI_status == "COMPLETED_TRIAL")
    n_total <- nrow(all_changes)
    write_plain(capture.output(sessionInfo()), file.path(out, "00_R_session_info.txt"))
    pkg_version <- function(p) if (requireNamespace(p, quietly = TRUE))
      as.character(utils::packageVersion(p)) else "not installed"
    write_plain(c(
      "Script: StJohns_WRTDS_All12_ReviewerRevision_v10_CI_PrintSafeSupp.R",
      paste("Run directory:", out), paste("Saved model source:", saved_dir),
      paste("EGRET:", pkg_version("EGRET")), paste("EGRETci:", pkg_version("EGRETci")),
      paste("USE_SAVED_MODELS:", USE_SAVED_MODELS), paste("Allow fresh fits:", RUN_MODELS),
      paste("Requested fresh-fit input Q code:", INPUT_Q_UNIT, "(1=cfs, 2=m^3/s)"),
      paste("Requested fresh-fit ZERO_ADJUST:", ZERO_ADJUST),
      "Reused models retain their existing imported discharge and fitting parameters.",
      paste("Water-year start month:", PA_START, "length:", PA_LONG),
      paste("Comparison years:", paste(YEAR_POINTS, collapse = ",")),
      paste("RUN_CI_CHANGES:", RUN_CI_CHANGES), paste("RUN_CI_BANDS:", RUN_CI_BANDS),
      paste("Bootstrap replicates requested:", CI_N_BOOT),
      paste("Bootstrap block length, days:", CI_BLOCK_LENGTH),
      paste("Minimum valid replicate fraction:", CI_MIN_GOOD_FRACTION),
      paste("Requested windows:", paste(vapply(CI_WINDOWS, function(w)
        window_key(w[1L], w[2L]), character(1)), collapse = ", ")),
      "Quantiles: 0.025 and 0.975, R quantile type=6.",
      "Annual daily flux: kg/day * 365.25 / 1000 -> 10^3 kg/year.",
      "runPairsBoot flux changes: 10^6 kg/year * 1000 -> 10^3 kg/year.",
      paste("Final change CIs completed:", n_final, "of", n_total),
      paste("Trial change CIs completed:", n_trial),
      paste("HYDROLOGY_VALIDATED reporting flag:", HYDROLOGY_VALIDATED)
    ), file.path(out, "00_run_settings.txt"))
    write_plain(c(
      "READ BEFORE SUBMISSION",
      "This script computes EGRET WRTDS, not the separate WRTDStidal salinity analysis.",
      "Source files and earlier run directories are preserved.",
      "Existing Final.CSV zeros may represent previously zeroed reverse flows;",
      "adjust=TRUE does not reconstruct the original signed hydrology.",
      "Flux is a model output under that scenario, not net signed tidal transport.",
      "Validate source Q units, station/gauge pairing, censoring, and TN/TKN comparability.",
      "WY2024 begins 2023-10-01. Missing or sparse samples require endpoint review.",
      "Comparison years are not automatically moved when an endpoint is unsupported.",
      "Point estimates are preserved even where a change CI cannot be calculated.",
      "Missing CI limits are NA, never zero. Read CI_status and CI_note for every row.",
      "Fewer than three endpoint samples is an audit flag, not an automatic validity test.",
      "00_export_status.csv reports file exports; it does not certify complete uncertainty.",
      "00_CI_change_status.csv and 00_CI_completion_summary.csv report change-CI coverage.",
      "Annual confidence bands and change confidence intervals are separate analyses.",
      "V9 print-safe drawing, censored flux bounds, and optional four-panel split pages are retained.",
      "00_Model_QC_summary.csv records the QC state of all twelve requested models.",
      "Per-model Sample/Daily largest-prediction files expose dates and discharge for inspection.",
      "Model_QC_review_flag is separate from computational CI_status; completed intervals",
      "on flagged fits still require model review before scientific interpretation.",
      "For Figures 5-7 annual bands, set RUN_CI_BANDS=TRUE; it is FALSE by default.",
      "Change-CI limits are pointwise intervals, not simultaneous intervals across all comparisons.",
      "If source inputs or intended fitting/import settings change, set USE_SAVED_MODELS=FALSE.",
      "V10 reuses compatible bootstrap archives only when model hash, settings, versions, and years match.",
      "Legacy bootstrap files without matching CI-engine metadata are recalculated.",
      "TN/TKN date matching does not establish identical station, fraction, or physical sample.",
      sprintf("Computed final change CIs: %d of %d comparison rows.", n_final, n_total),
      if (HYDROLOGY_VALIDATED)
        "HYDROLOGY_VALIDATED=TRUE is a user-supplied reporting flag; retain supporting evidence."
      else "HYDROLOGY_VALIDATED=FALSE: outputs remain exploratory pending hydrologic validation."
    ), file.path(out, "00_PUBLICATION_VALIDATION_REQUIRED.txt"))
    write_plain(if (length(run_log$lines)) run_log$lines else "No additional notices",
                 file.path(out, "00_run_notices.txt"))
    safe_csv(supp_status, file.path(out, "00_supplement_export_status.csv"))
    safe_csv(figure_status, file.path(out, "00_figure_export_status.csv"))
    safe_csv(do.call(rbind, qc_list), file.path(out, "00_Model_QC_summary.csv"))
    invisible(NULL)
  }
  # Preserve status even on a normal R interrupt. A forced process kill can
  # only preserve the most recent checkpoint already written to disk.
  on.exit({
    try(export_tables_and_ci_status(table1_list, change_list, model_status,
                                    annual_status, out), silent = TRUE)
    try(write_metadata(), silent = TRUE)
  }, add = TRUE)

  # PHASE 1: establish all twelve fits and point estimates before bootstrapping.
  for (i in seq_len(nrow(config))) {
    row <- config[i, , drop = FALSE]
    site <- row$site
    analyte <- row$analyte
    key <- keys[i]
    jobdir <- file.path(out, "Models", model_folder(site, analyte))
    dir.create(jobdir, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("\nMODEL %d/%d: %s\n", i, nrow(config), key))
    model_status$Status[i] <- "RUNNING"
    result <- tryCatch(with_logged_warnings({
      got <- load_or_fit_model(row, saved_dir, jobdir)
      eList <- got$eList
      diag <- extract_diagnostics(eList, site, analyte)
      annual <- annual_results(eList)
      change <- change_results(eList, site, analyte, jobdir)
      qc <- tryCatch(collect_model_qc(eList, site, analyte, diag, jobdir),
        error = function(e) {
          record_issue(paste("QC AUDIT FAILED:", key, conditionMessage(e)))
          q <- empty_model_qc(site, analyte)
          q$QC_status <- "FAILED"
          q$QC_note <- conditionMessage(e)
          q
        })
      change$Model_QC_review_flag <- qc$Model_QC_review_flag
      change$Model_QC_note <- qc$QC_note
      if (isTRUE(qc$Model_QC_review_flag))
        record_issue(paste("MODEL QC REVIEW:", key, qc$QC_note))
      coverage <- sample_coverage(eList, site, analyte)
      safe_csv(coverage, file.path(jobdir, "observed_sample_counts_by_WY.csv"))
      safe_csv(coverage[coverage$WaterYear < 2004L, , drop = FALSE],
                file.path(jobdir, "reviewer2_samples_before_2004.csv"))
      endpoints <- coverage[match(YEAR_POINTS, coverage$WaterYear), , drop = FALSE]
      endpoints$low_sample_count_flag <- endpoints$n_samples < 3L
      safe_csv(endpoints, file.path(jobdir, "reviewer_endpoint_support.csv"))
      if (any(endpoints$low_sample_count_flag))
        record_issue(paste(key, "endpoint water years with fewer than 3 samples:",
          paste(endpoints$WaterYear[endpoints$low_sample_count_flag], collapse = ", ")))
      if (diag$censored_pct >= 50 || diag$SEP_percent >= 100)
        record_issue(paste(key, "requires model review; censored percent =", diag$censored_pct,
                            "and SEP percent =", diag$SEP_percent))
      safe_csv(diag, file.path(jobdir, "Table1_model_diagnostics.csv"))
      safe_csv(annual, file.path(jobdir, "annual_EGRET_estimates.csv"))
      safe_csv(change, file.path(jobdir, "Table_changes_no_CI.csv"))
      list(got = got, diag = diag, annual = annual, change = change, qc = qc)
    }, key), error = function(e) {
      record_issue(paste("MODEL FAILED:", key, conditionMessage(e)))
      write_plain(c("FAILED", key, conditionMessage(e)), file.path(jobdir, "FAILED.txt"))
      model_status$Error[i] <<- conditionMessage(e)
      NULL
    })
    if (is.null(result)) {
      model_status$Status[i] <- "FAILED"
      qc_list[[key]]$QC_status <- "MODEL_FAILED"
      qc_list[[key]]$QC_note <- model_status$Error[i]
      ch <- change_list[[key]]
      requested <- vapply(CI_WINDOWS, function(w) window_key(w[1L], w[2L]), character(1))
      use <- RUN_CI_CHANGES & window_key(ch$From_WY, ch$To_WY) %in% requested
      ch$CI_status <- ifelse(use, "SKIPPED_MODEL_FAILED", "NOT_REQUESTED")
      ch$CI_note <- model_status$Error[i]
      change_list[[key]] <- ch
      if (RUN_CI_BANDS) annual_status$Status[i] <- "SKIPPED_MODEL_FAILED"
      if (MAKE_SUPP_DIAGNOSTICS) supp_status$Status[i] <- "SKIPPED_MODEL_FAILED"
    } else {
      fit_list[[key]] <- result$got$eList
      model_meta[[key]] <- result$got[c("origin", "source_job", "model_md5")]
      annual_list[[key]] <- result$annual
      table1_list[[key]] <- result$diag
      change_list[[key]] <- result$change
      qc_list[[key]] <- result$qc
      model_status$Status[i] <- "COMPLETED"
      model_status$Origin[i] <- result$got$origin
      model_status$Source_model[i] <- if (result$got$reused)
        file.path(result$got$source_job, "EGRET_fitted_eList.rds") else ""
      if (MAKE_SUPP_DIAGNOSTICS) {
        supp_result <- tryCatch(with_logged_warnings(save_supp_diagnostics(
          fit_list[[key]], site, analyte,
          file.path(out, "Supplementary_Diagnostics", model_folder(site, analyte))), key),
          error = function(e) {
            record_issue(paste("SUPPLEMENTARY FIGURE FAILED:", key, conditionMessage(e)))
            supp_status$Error[i] <<- conditionMessage(e)
            NULL
          })
        if (is.null(supp_result)) supp_status$Status[i] <- "FAILED"
        else {
          supp_status$Status[i] <- "EXPORTED"
          supp_status$TIFF[i] <- supp_result$tiff
          supp_status$PDF[i] <- supp_result$pdf
          supp_status$Caption[i] <- supp_result$caption
          supp_status$Part_files[i] <- paste(supp_result$parts, collapse = "; ")
          figure_files <- c(supp_result$tiff, supp_result$pdf, supp_result$parts)
          safe_csv(data.frame(Site = site, Constituent = analyte,
            File = figure_files, Size_bytes = as.numeric(file.info(figure_files)$size),
            Exists = file.exists(figure_files), stringsAsFactors = FALSE),
            file.path(jobdir, "Supplement_diagnostic_export_status.csv"))
        }
      }
    }
    exports <- export_tables_and_ci_status(table1_list, change_list, model_status,
                                           annual_status, out)
    write_metadata()
  }

  # Keep audit results available before the longer bootstrap phase starts.
  tryCatch(reviewer_consistency_checks(fit_list, annual_list, out), error = function(e)
    record_issue(paste("TN/TKN AUDIT FAILED:", conditionMessage(e))))
  for (f in unique(config$flow_file)) {
    p <- file.path(FLOW_DIR, f)
    if (!file.exists(p)) {
      record_issue(paste("RAW FLOW AUDIT NOT AVAILABLE:", p))
      next
    }
    tryCatch(flow_audit(p, file.path(out, paste0("AUDIT_", slug(f), "_discharge.csv"))),
      error = function(e) record_issue(paste("FLOW AUDIT:", conditionMessage(e))))
  }
  if (!length(fit_list)) stop("No models completed; see 00_run_status.csv and per-model FAILED.txt")

  # PHASE 2: compute change CIs, retaining a checkpoint after every window.
  if (RUN_CI_CHANGES) {
    for (i in seq_len(nrow(config))) {
      key <- keys[i]
      eList <- fit_list[[key]]
      if (is.null(eList)) next
      jobdir <- file.path(out, "Models", model_folder(config$site[i], config$analyte[i]))
      ch <- change_list[[key]]
      for (k in seq_len(nrow(ch))) {
        if (ch$CI_status[k] != "PENDING") next
        y1 <- as.integer(ch$From_WY[k])
        y2 <- as.integer(ch$To_WY[k])
        cat(sprintf("\nCHANGE CI %s: WY%d-WY%d; %d requested replicates\n",
                    key, y1, y2, CI_N_BOOT))
        reason <- endpoint_reason(eList, y1, y2)
        if (nzchar(reason)) {
          ch$CI_status[k] <- "SKIPPED_UNSUPPORTED_ENDPOINT"
          ch$CI_note[k] <- reason
          record_issue(paste("CI SKIPPED:", key, y1, y2, reason))
        } else {
          answer <- tryCatch(with_logged_warnings(calculate_one_change_ci(
            eList, annual_list[[key]], y1, y2, i, k, model_meta[[key]]$model_md5,
            jobdir, model_meta[[key]]$source_job), paste(key, y1, y2)),
            error = function(e) {
              record_issue(paste("CI CHANGE FAILED:", key, y1, y2, conditionMessage(e)))
              list(error = conditionMessage(e))
            })
          if (!is.null(answer$error)) {
            ch$CI_status[k] <- "FAILED"
            ch$CI_note[k] <- answer$error
          } else {
            z <- answer$limits
            ch$Conc_change_CI95_low_mgL[k] <- z$conc[1L]
            ch$Conc_change_CI95_high_mgL[k] <- z$conc[2L]
            ch$Flux_change_CI95_low_1000kgyr[k] <- z$flux[1L]
            ch$Flux_change_CI95_high_1000kgyr[k] <- z$flux[2L]
            ch$CI_n_good[k] <- z$n_good
            ch$CI_method[k] <- paste0("EGRETci block bootstrap; n_requested=", CI_N_BOOT,
              "; n_good=", z$n_good, "; block_days=", CI_BLOCK_LENGTH,
              "; percentile95; quantile_type=6")
            ch$CI_status[k] <- if (CI_N_BOOT < 100L) "COMPLETED_TRIAL" else "COMPLETED"
            notes <- character()
            if (isTRUE(ch$Model_QC_review_flag[k]))
              notes <- c(notes, "Fit has QC review flags; inspect 00_Model_QC_summary.csv before interpreting this interval.")
            if (qc_list[[key]]$QC_status == "FAILED")
              notes <- c(notes, "Model QC audit failed; see Model_QC_note.")
            if (ch$n_samples_From_WY[k] < 3L || ch$n_samples_To_WY[k] < 3L)
              notes <- c(notes, "Sparse endpoint sampling; review reviewer_endpoint_support.csv.")
            if (answer$reused) notes <- c(notes, "Matching saved bootstrap reused.")
            if (CI_N_BOOT < 100L) notes <- c(notes, "Trial interval; increase replicate count for reporting.")
            ch$CI_note[k] <- paste(notes, collapse = " ")
            cat("Computed interval from", z$n_good, "valid replicates.\n")
          }
        }
        change_list[[key]] <- ch
        safe_csv(ch, file.path(jobdir, "Table_changes_with_selected_CIs.csv"))
        exports <- export_tables_and_ci_status(table1_list, change_list, model_status,
                                               annual_status, out)
      }
    }
  }

  # PHASE 3: annual bands are optional and are not needed for table-change CIs.
  if (RUN_CI_BANDS) {
    for (i in seq_len(nrow(config))) {
      key <- keys[i]
      eList <- fit_list[[key]]
      if (is.null(eList)) next
      jobdir <- file.path(out, "Models", model_folder(config$site[i], config$analyte[i]))
      cat("\nANNUAL CONFIDENCE BANDS:", key, "\n")
      band_result <- tryCatch(with_logged_warnings({
        ci <- log_console(EGRETci::ciCalculations(eList,
          startSeed = CI_SEED + i, verbose = TRUE, nBoot = CI_N_BOOT,
          blockLength = CI_BLOCK_LENGTH, widthCI = 95),
          file.path(jobdir, "EGRETci_annual_95pct_CI_console.txt"))
        save_rds(ci, file.path(jobdir, "EGRETci_annual_95pct_CI.rds"))
        safe_csv(ci, file.path(jobdir, "annual_95pct_CI_native_units.csv"))
        ann <- annual_with_ci(annual_list[[key]], ci, eList$Sample$Date)
        good <- is.finite(ann$FNConc_L95) & is.finite(ann$FNConc_U95) &
          is.finite(ann$FNFlux_L95_1000kgyr) & is.finite(ann$FNFlux_U95_1000kgyr)
        if (!any(good)) stop("No supported finite annual bands")
        safe_csv(ann, file.path(jobdir, "annual_EGRET_with_95pct_CI.csv"))
        list(annual = ann, n_years = sum(good))
      }, key), error = function(e) {
        record_issue(paste("CI ANNUAL FAILED:", key, conditionMessage(e)))
        annual_status$Note[i] <<- conditionMessage(e)
        NULL
      })
      if (is.null(band_result)) annual_status$Status[i] <- "FAILED"
      else {
        annual_list[[key]] <- band_result$annual
        annual_status$Status[i] <- if (CI_N_BOOT < 100L) "COMPLETED_TRIAL" else "COMPLETED"
        annual_status$Years_with_bands[i] <- band_result$n_years
        annual_status$Note[i] <- "Bands outside the sampled water-year range are omitted."
      }
      exports <- export_tables_and_ci_status(table1_list, change_list, model_status,
                                             annual_status, out)
    }
  }

  # PHASE 4: figures use actual annual bands only when computed above.
  if (MAKE_MAIN_FIGURES) {
    for (j in seq_along(c("TKN", "TN", "TP"))) {
      a <- c("TKN", "TN", "TP")[j]
      fig <- tryCatch(with_logged_warnings(make_figure(a, annual_list, out), a),
        error = function(e) {
          record_issue(paste("MAIN FIGURE FAILED:", a, conditionMessage(e)))
          figure_status$Error[j] <<- conditionMessage(e)
          NULL
        })
      if (is.null(fig)) figure_status$Status[j] <- "FAILED"
      else {
        figure_status$Status[j] <- "EXPORTED"
        figure_status$TIFF[j] <- fig$tiff
        figure_status$PDF[j] <- fig$pdf
      }
    }
  }
  exports <- export_tables_and_ci_status(table1_list, change_list, model_status,
                                         annual_status, out)
  for (j in seq_len(nrow(figure_status))) {
    if (figure_status$Status[j] != "NOT_REQUESTED")
      exports[[figure_status$Artifact[j]]] <- figure_status$Status[j] == "EXPORTED"
  }
  safe_csv(data.frame(Artifact = names(exports),
    Exported = vapply(exports, isTRUE, logical(1))), file.path(out, "00_export_status.csv"))
  write_metadata()

  all_changes <- do.call(rbind, change_list)
  n_final <- sum(all_changes$CI_status == "COMPLETED")
  n_trial <- sum(all_changes$CI_status == "COMPLETED_TRIAL")
  cat("\nRUN FINISHED. OUTPUT:\n", out, "\n", sep = "")
  cat("Models completed:", length(fit_list), "/", nrow(config), "\n")
  cat("Change CIs completed:", n_final, "/", nrow(all_changes), "comparison rows\n")
  if (n_trial) cat("Additional trial CIs:", n_trial, "\n")
  cat("Failed change CIs:", sum(all_changes$CI_status == "FAILED"), "\n")
  cat("Skipped change CIs:", sum(startsWith(all_changes$CI_status, "SKIPPED")), "\n")
  cat("Unrequested change CIs:", sum(all_changes$CI_status == "NOT_REQUESTED"), "\n")
  all_qc <- do.call(rbind, qc_list)
  cat("Models with QC review flags:", sum(all_qc$Model_QC_review_flag, na.rm = TRUE), "\n")
  cat("Unavailable/failed QC audits:", sum(all_qc$QC_status %in% c("FAILED", "MODEL_FAILED")), "\n")
  if (RUN_CI_BANDS) cat("Annual-band series completed:",
    sum(annual_status$Status == "COMPLETED"), "/", nrow(config), "\n")
  else cat("Annual figure confidence bands were not requested. Table CIs are reported separately.\n")
  if (RUN_CI_CHANGES && n_final != nrow(all_changes))
    cat("Review 00_CI_change_status.csv and 00_CI_completion_summary.csv for remaining intervals.\n")
  if (length(run_log$lines)) cat("Review 00_run_notices.txt.\n")
  if (!HYDROLOGY_VALIDATED) cat("Hydrology-validation reporting flag remains FALSE.\n")
  invisible(list(output_directory = out, models_completed = length(fit_list),
                  change_CIs_completed = n_final, total_comparisons = nrow(all_changes)))
}

# Normal use: source this complete script or use RStudio's Source button.
# The option is only for checking helper functions without starting analysis.
if (!isTRUE(getOption("wrtds.v10.define_only", FALSE))) run_analysis()

# Official API references:
# https://doi-usgs.github.io/EGRET/reference/runPairs.html
# https://doi-usgs.github.io/EGRETci/reference/runPairsBoot.html
# https://doi-usgs.github.io/EGRETci/reference/ciCalculations.html
# https://github.com/DOI-USGS/EGRETci/blob/main/R/runPairsBoot.R

