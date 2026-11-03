library(WRTDStidal)
library(tidyverse)
library(patchwork)

# Define exact local directories for inputs and figures
sal_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Salinity"
wq_dir  <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Water Quality"
fig_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Figures"

# set default linewidth for all geom_line layers
update_geom_defaults("line", list(linewidth = 1.2))

# label for plots
ylb <- 'log(TKN) (mg/L)'

# function to interpolate data to monthly timestep
interp_fun <- function(saldat, nutdat) {
  
  saldat$date <- as.Date(saldat$date)
  nutdat$date <- as.Date(nutdat$date)
  
  saldat <- saldat[order(saldat$date), ]
  nutdat <- nutdat[order(nutdat$date), ]
  
  start_date <- min(nutdat$date)
  end_date <- max(nutdat$date)
  
  monthly_dates <- seq(from = as.Date(paste0(format(start_date, "%Y-%m"), "-01")),
                       to = as.Date(paste0(format(end_date, "%Y-%m"), "-01")),
                       by = "month")
  
  sal_monthly <- approx(x = saldat$date,
                        y = saldat$sal,
                        xout = monthly_dates,
                        method = "linear",
                        rule = 2)
  
  res_monthly <- approx(x = nutdat$date,
                        y = nutdat$res,
                        xout = monthly_dates,
                        method = "linear",
                        rule = 2)
  
  out <- data.frame(
    date = monthly_dates,
    res_interpolated = res_monthly$y,
    sal_interpolated = sal_monthly$y
  )
  
  return(out)
}

# function to process data and fit model for a site (with site-specific s_max)
process_site <- function(site_name, sal_file, tkn_file, s_max = NULL) {
  
  # import salinity
  saldat <- read.csv(sal_file) |>
    select(date = Date, sal = Value) |>
    mutate(
      date = as.Date(date, format = "%m/%d/%Y")
    ) |>
    filter(!is.na(sal))
  
  # Calculate site-specific s_max from raw salinity data if not provided
  if (is.null(s_max)) {
    s_max <- max(saldat$sal, na.rm = TRUE)
  }
  
  # import nutrients
  nutdat <- read.csv(tkn_file) |>
    select(date = Date, res = Value, detect = Remark) |>
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      detect = ifelse(detect == "<", TRUE, FALSE)
    ) |>
    filter(!is.na(res))
  
  # interpolate
  intdat <- interp_fun(saldat, nutdat)
  
  # convert to tidalmean (strictly keeping 4 columns)
  totidal <- intdat |>
    mutate(
      res = log(res_interpolated),
      flo = 1 - (sal_interpolated / s_max),
      lim = log(0.01)
    ) |>
    select(date, res, flo, lim) |>
    tidalmean(reslog = TRUE, reslab = ylb, flolab = 'Salinity (ppt)')
  
  # fit model
  res <- modfit(totidal, resp_type = 'mean')
  
  return(res)
}

# process all four sites using dynamic file paths & site-specific s_max
cat("Processing Rockledge...\n")
rock_res <- process_site("Rockledge",
                         file.path(sal_dir, "Rockledge_salinity.csv"),
                         file.path(wq_dir, "TKN_Rockledge.csv"))

cat("Processing Sanford...\n")
san_res <- process_site("Sanford",
                        file.path(sal_dir, "Sanford_salinity.csv"),
                        file.path(wq_dir, "TKN_Sanford.csv"))

cat("Processing Cow Creek...\n")
cow_res <- process_site("CowCreek",
                        file.path(sal_dir, "CowCreek_Salinity.csv"),
                        file.path(wq_dir, "TKN_CowCreek.csv"))

cat("Processing Jacksonville...\n")
jac_res <- process_site("Jacksonville",
                        file.path(sal_dir, "Jacksonville_Salinity.csv"),
                        file.path(wq_dir, "TKN_Jacksonville.csv"))

# Helper function to generate standardized site plots
make_site_plot <- function(mod_res, title_label) {
  seasyrplot(mod_res, predicted = FALSE, lwd = 2) +
    ylab("log(TKN)") +
    annotate("text", x = -Inf, y = Inf, label = title_label, hjust = -0.1, vjust = 1.5,
             size = 8, fontface = "bold") +
    theme(text = element_text(size = 14),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.position = "none")
}

p1 <- make_site_plot(rock_res, "Rockledge")
p2 <- make_site_plot(san_res, "Sanford")
p3 <- make_site_plot(cow_res, "Cow Creek")
p4 <- make_site_plot(jac_res, "Jacksonville")

# combine into 2x2 layout
combined_plot <- (p1 | p2) / (p3 | p4) +
  plot_layout() +
  theme(plot.margin = margin(t = 10, r = 10, b = 20, l = 10))

# construct full file path for output
out_fig <- file.path(fig_dir, "four_sites_seasyrplot_TKN.jpeg")

# save combined plot
jpeg(out_fig, width = 14, height = 12, units = "in", res = 600)
print(combined_plot)
dev.off()

cat("Combined plot saved as:", out_fig, "\n")
