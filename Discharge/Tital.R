# --- Install/load packages ----------------------------------------------------
pkgs <- c("WRTDStidal","readr","dplyr","lubridate","ggplot2")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

# --- File + column settings ---------------------------------------------------
# Point to your St. Johns River Basin data (one row per sample)
# Required columns: Date, Value (nutrient), Flo (salinity OR flow), LOD (detection limit)
# Optional: Station to run multiple sites in a loop
data_path <- "F:/St Johns DEM/Data/SJRB_TN.csv"  # <- change me

# Example expected headers in the CSV:
# Station,Date,TN_mgL,Sal_psu,LOD_mgL
date_col <- "Date"      # date of sample
resp_col <- "TN_mgL"    # nutrient (e.g., TN or TP) in mg/L
flo_col  <- "Sal_psu"   # salinity in psu (or use a flow column instead)
lod_col  <- "LOD_mgL"   # detection limit (use a tiny value if not censored)
station_col <- "Station" # optional; set to NULL if not present

# --- Read & prep --------------------------------------------------------------
raw <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    Date = as.Date(.data[[date_col]]),
    resp = .data[[resp_col]],
    flo  = .data[[flo_col]],
    lod  = if (lod_col %in% names(.)) .data[[lod_col]] else 1e-9
  ) %>%
  arrange(Date) %>%
  filter(is.finite(resp), is.finite(flo), !is.na(Date))

if (!is.null(station_col) && station_col %in% names(raw)) {
  stations <- sort(unique(raw[[station_col]]))
} else {
  stations <- "ALL"
  raw$Station <- stations
}

# Helper: fit Tidal WRTDS for one station -------------------------------------
fit_wrtds_one <- function(df_station, var_label = "Total Nitrogen (mg/L)",
                          flo_label = "Salinity (psu)",
                          taus = c(0.1, 0.5, 0.9),
                          use_grid_search = FALSE) {
  
  # Format to four columns: date, response, sal/flow, detection limit
  dat4 <- df_station %>%
    select(Date, resp, flo, lod)
  
  # Option A (simple): one-call wrapper does tidal + wrtds + predictions + normalization
  # reslog = FALSE tells WRTDStidal to log-transform the response for you
  mod <- modfit(
    dat4,
    resp_type = "quantile",
    ind = c(1,2,3,4),
    reslog = FALSE,
    tau = taus,
    flo_div = 10,       # interpolation grid across salinity/flow
    trace = TRUE
  )
  
  # Option B (optional): choose optimal half-window widths via cross-validated grid search
  # (can take a while on large datasets, so off by default)
  if (use_grid_search) {
    tid <- tidal(dat4, ind = c(1,2,3,4), reslab = var_label, flolab = flo_label, reslog = FALSE)
    message("Running window-width grid search (this can be slow)...")
    g <- winsrch_grid(tid)                       # evaluate many window combos
    best <- g[which.min(g$err), c("mos","yrs","flo")]
    mod  <- modfit(dat4, resp_type = "quantile", ind = c(1,2,3,4),
                   reslog = FALSE, tau = taus,
                   wins = as.list(best))
  }
  
  # Basic QC/performance
  perf <- wrtdsperf(mod, logspace = TRUE)
  
  # Plots (ggplot objects)
  p_fit   <- fitplot(mod, predicted = TRUE, annuals = TRUE, logspace = FALSE)
  p_norm  <- prdnrmplot(mod, logspace = FALSE)        # compares predicted vs flow/sal-normalized
  p_seas  <- seasplot(mod)                            # seasonality through the years
  
  # Annual aggregates (water year by default)
  ann <- annual_agg(mod, mo_strt = 10, min_mo = 9, logspace = FALSE)
  
  list(model = mod, perf = perf, plots = list(fit = p_fit, prdnrm = p_norm, seas = p_seas), annual = ann)
}

# --- Run for each station -----------------------------------------------------
results <- vector("list", length(stations))
names(results) <- stations

for (st in stations) {
  message("Fitting station: ", st)
  df_st <- raw %>% filter(Station == st)
  results[[st]] <- fit_wrtds_one(df_st,
                                 var_label = "Total Nitrogen (mg/L)",
                                 flo_label = "Salinity (psu)",
                                 taus = c(0.1,0.5,0.9),
                                 use_grid_search = FALSE)  # set TRUE to optimize windows
}

# --- Example: view plots for first station -----------------------------------
first_st <- stations[1]
results[[first_st]]$plots$fit
results[[first_st]]$plots$prdnrm
results[[first_st]]$plots$seas

# --- Export predicted & normalized time series -------------------------------
# The mod object stores predicted (fit*) and normalized (norm*) columns on the observed dates.
extract_timeseries <- function(mod_obj) {
  # The tidal object returned by modfit holds the working data frame internally;
  # we can access it directly (it's a data.frame subclass).
  df <- as.data.frame(mod_obj)
  # Columns added by respred/resnorm have prefixes 'fit' and 'norm'
  df %>%
    select(Date = date, resp, flo, starts_with("fit"), starts_with("norm"))
}

out_dir <- "F:/St Johns DEM/Outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
for (st in stations) {
  ts_out <- extract_timeseries(results[[st]]$model)
  write_csv(ts_out, file.path(out_dir, paste0("SJRB_", st, "_TN_WRTDStidal_timeseries.csv")))
}

# --- Trend estimates (flow/sal-normalized) -----------------------------------
# Example: median (tau = 0.5) normalized annual trend by water-year
for (st in stations) {
  ann <- results[[st]]$annual
  print(ggplot(ann, aes(year, norm0.5)) + geom_line() + geom_point() +
          labs(title = paste("SJRB", st, "- TN normalized (tau=0.5) by water year"),
               x = "Water year", y = "TN (mg/L, normalized)"))
}

# If you want formal trend tests, you can use wrtdstrnd() or seasonal-Kendall:
# tr <- wrtdstrnd(results[[first_st]]$model)        # WRTDS trend framework
# tr_sk <- wrtdstrnd_sk(results[[first_st]]$model)  # seasonal Kendall variant
