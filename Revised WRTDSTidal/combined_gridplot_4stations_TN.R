library(WRTDStidal)
library(tidyverse)
library(patchwork)

# 1. Define your exact local directories
sal_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Salinity"
wq_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Water Quality"
fig_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Figures"

ylb <- 'TN (mg/L)'

# 2. Corrected function strictly adhering to reviewer requirements
fit_station <- function(sal_file, nut_file) {
  saldat <- read.csv(sal_file) |>
    select(date = Date, sal = Value) |>
    mutate(date = as.Date(date, format = "%m/%d/%Y")) |>
    filter(!is.na(sal)) |>
    arrange(date)
  
  nutdat <- read.csv(nut_file) |>
    select(date = Date, res = Value, detect = Remark) |>
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      not_cens = !(detect %in% c("<", "U", "ND"))
    ) |>
    filter(!is.na(res)) |>
    arrange(date)
  
  # Calculate site-specific Smax for the freshwater fraction calculation
  s_max <- max(saldat$sal, na.rm = TRUE)
  cat("  -> Using Smax =", s_max, "\n")
  
  # Interpolate salinity strictly to dates of actual nutrient samples
  sal_interp <- approx(
    x = saldat$date, y = saldat$sal,
    xout = nutdat$date, method = "linear", rule = 2
  )
  
  totidal <- nutdat |>
    mutate(
      sal_interpolated = sal_interp$y,
      flo = 1 - (sal_interpolated / s_max), # Uses site-specific Smax
      res_val = log(res),
      # Retains observation-specific censoring status
      lim = ifelse(not_cens, -100, log(res))       
    ) |>
    select(date, res = res_val, flo, lim) |>
    tidalmean(reslog = TRUE, reslab = ylb, flolab = 'Freshwater Fraction')
  
  res_mod <- modfit(totidal, resp_type = 'mean')
  return(res_mod)
}

# shared theme tweaks
my_theme <- theme(
  text = element_text(size = 21),
  legend.position = "top",
  legend.justification = "right",
  legend.direction = "horizontal",
  # 1. Shrink legend text and widen the colorbar to stop overlap
  legend.text = element_text(size = 12), 
  legend.title = element_text(size = 16),
  legend.key.width = unit(1.5, "cm"), 
  # 2. Fix title spacing and remove the vjust = -10 collision hack
  plot.title = element_text(hjust = 0, size = 23, margin = margin(b = 15)),
  plot.title.position = "plot",
  # 3. Add a top margin so the legend has room to breathe
  plot.margin = margin(t = 20, r = 10, b = 10, l = 10) 
)

# 3. Fit all 4 stations using dynamic file paths
cat("Fitting Rockledge...\n")
res_rockledge <- fit_station(file.path(sal_dir, "Rockledge_salinity.csv"), file.path(wq_dir, "TN_Rockledge.csv"))

cat("Fitting Sanford...\n")
res_sanford <- fit_station(file.path(sal_dir, "Sanford_salinity.csv"), file.path(wq_dir, "TN_Sanford.csv"))

cat("Fitting CowCreek...\n")
res_cowcreek <- fit_station(file.path(sal_dir, "CowCreek_Salinity.csv"), file.path(wq_dir, "TN_CowCreek.csv"))

cat("Fitting Jacksonville...\n")
res_jacksonville <- fit_station(file.path(sal_dir, "Jacksonville_Salinity.csv"), file.path(wq_dir, "TN_Jacksonville.csv"))

# 4. Generate gridplots
p1 <- gridplot(res_rockledge, month = 'all', lwd = 2) + ggtitle('Rockledge') + my_theme
p2 <- gridplot(res_sanford, month = 'all', lwd = 2) + ggtitle('Sanford') + my_theme
p3 <- gridplot(res_cowcreek, month = 'all', lwd = 2) + ggtitle('Cow Creek') + my_theme
p4 <- gridplot(res_jacksonville, month = 'all', lwd = 2) + ggtitle('Jacksonville') + my_theme

# Combine 2x2 layout
combined <- (p1 + p2) / (p3 + p4)

# 5. Save to correct Figures directory
out_fig <- file.path(fig_dir, "combined_gridplot_4stations_TN.jpeg")

jpeg(out_fig, width = 16, height = 12, units = "in", res = 600)
print(combined)
dev.off()

cat("Done! Saved as", out_fig, "\n")
