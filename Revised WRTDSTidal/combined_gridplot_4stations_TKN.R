library(WRTDStidal)
library(tidyverse)
library(patchwork)

# 1. Define exact local directories
sal_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Salinity"
wq_dir  <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Water Quality"
fig_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Figures"

ylb <- 'log(TKN) (mg/L)'

# Set default line width for line layers
update_geom_defaults("line", list(linewidth = 1.2))

# 2. Function to process data and fit WRTDS model
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

# 3. Fit all 4 stations using dynamic file paths
cat("Fitting Rockledge...\n")
res_rockledge <- fit_station(file.path(sal_dir, "Rockledge_salinity.csv"), file.path(wq_dir, "TKN_Rockledge.csv"))

cat("Fitting Sanford...\n")
res_sanford <- fit_station(file.path(sal_dir, "Sanford_salinity.csv"), file.path(wq_dir, "TKN_Sanford.csv"))

cat("Fitting CowCreek...\n")
res_cowcreek <- fit_station(file.path(sal_dir, "CowCreek_Salinity.csv"), file.path(wq_dir, "TKN_CowCreek.csv"))

cat("Fitting Jacksonville...\n")
res_jacksonville <- fit_station(file.path(sal_dir, "Jacksonville_Salinity.csv"), file.path(wq_dir, "TKN_Jacksonville.csv"))

# 4. Standardized colorbar guide with proper title position and width
std_colorbar <- guide_colorbar(
  title = "res",
  title.position = "left",        # Places 'res' to the left of the color bar
  title.vjust = 0.8,              # Vertically aligns 'res' with the bar
  barwidth = unit(4.5, "cm"),     # Standardized bar width across all plots
  barheight = unit(0.4, "cm"),    # Standardized bar height
  nbin = 100
)

# 5. Shared theme
my_theme <- theme(
  text = element_text(size = 21),
  legend.position = "top",
  legend.justification = "right",
  legend.direction = "horizontal",
  legend.text = element_text(size = 12),    # Smaller text size prevents overlapping tick labels
  legend.title = element_text(size = 16),
  plot.title = element_text(hjust = 0, size = 23, margin = margin(b = 15)),
  plot.title.position = "plot",
  plot.margin = margin(t = 20, r = 10, b = 10, l = 10) 
)

# Function to generate individual gridplots with explicit control over scale breaks
make_plot <- function(mod_res, title_text, n_breaks = 4) {
  # Generate native gridplot
  p <- gridplot(mod_res, month = 'all', lwd = 2)
  
  # Default color palette used inside WRTDStidal::gridplot
  default_cols <- rev(RColorBrewer::brewer.pal(11, 'Spectral'))
  
  # Apply default palette with explicit break frequency control
  p + 
    ggtitle(title_text) +
    scale_fill_gradientn(
      colors = default_cols,
      n.breaks = n_breaks,        # Controls tick mark density
      guide = std_colorbar
    ) +
    my_theme
}

# 6. Generate gridplots for each station
p1 <- make_plot(res_rockledge, "Rockledge", n_breaks = 4)
p2 <- make_plot(res_sanford, "Sanford", n_breaks = 4)
p3 <- make_plot(res_cowcreek, "Cow Creek", n_breaks = 3) # Lower break frequency for wide Cow Creek range
p4 <- make_plot(res_jacksonville, "Jacksonville", n_breaks = 4)

# Combine into 2x2 layout
combined <- (p1 + p2) / (p3 + p4)

# 7. Save to correct Figures directory
out_fig <- file.path(fig_dir, "combined_gridplot_4stations_TKN.jpeg")

jpeg(out_fig, width = 16, height = 12, units = "in", res = 600)
print(combined)
dev.off()

cat("Done! Saved as", out_fig, "\n")
