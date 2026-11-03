library(WRTDStidal)
library(tidyverse)
library(patchwork)

# 1. Define your exact local directories 
sal_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Salinity"
wq_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Water Quality"
fig_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Figures"

ylb <- 'TP (mg/L)'

# 2. Corrected fitting function strictly adhering to reviewer requirements
fit_station <- function(sal_file, nut_file){
  saldat <- read.csv(sal_file) %>%
    select(date = Date, sal = Value) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
    filter(!is.na(sal)) %>%
    arrange(date)
  
  nutdat <- read.csv(nut_file) %>%
    select(date = Date, res = Value, detect = Remark) %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      not_cens = !(detect %in% c("<", "U", "ND"))
    ) %>%
    filter(!is.na(res)) %>%
    arrange(date)
  
  # Calculate site-specific Smax for the freshwater fraction calculation
  s_max <- max(saldat$sal, na.rm = TRUE)
  cat("  -> Using Smax =", s_max, "\n")
  
  # Interpolate salinity strictly to dates of actual nutrient samples
  sal_interp <- approx(
    x = saldat$date, y = saldat$sal,
    xout = nutdat$date, method = "linear", rule = 2
  )
  
  # Finish building the totidal object
  totidal <- nutdat %>%
    mutate(
      sal_interpolated = sal_interp$y,
      flo = 1 - (sal_interpolated / s_max), # Uses site-specific Smax
      res_val = log(res),
      # Retains observation-specific censoring status
      lim = ifelse(not_cens, -100, log(res))        
    ) %>%
    select(date, res = res_val, flo, lim) %>% 
    tidalmean(reslog = TRUE, reslab = ylb, flolab = 'Freshwater Fraction')
  
  res_mod <- modfit(totidal, resp_type = 'mean')
  return(res_mod)
}

# 3. Define shared plot theme with larger legend text
my_theme <- theme(
  text = element_text(size = 21),
  legend.position = "top",
  legend.justification = "right",
  legend.direction = "horizontal",
  legend.text = element_text(size = 18),  # Increased from 12
  legend.title = element_text(size = 20), # Increased from 16
  legend.key.width = unit(2.5, "cm"),     # Widened colorbar to fit larger text
  plot.title = element_text(hjust = 0, size = 23, margin = margin(b = 15)),
  plot.title.position = "plot",
  plot.margin = margin(t = 20, r = 10, b = 10, l = 10) 
)

# 4. Fit all 4 stations
cat("Fitting Rockledge...\n")
res_rockledge <- fit_station(file.path(sal_dir, "Rockledge_salinity.csv"), file.path(wq_dir, "TP_Rockledge.csv"))

cat("Fitting Sanford...\n")
res_sanford <- fit_station(file.path(sal_dir, "Sanford_salinity.csv"), file.path(wq_dir, "TP_Sanford.csv"))

cat("Fitting CowCreek...\n")
res_cowcreek <- fit_station(file.path(sal_dir, "CowCreek_Salinity.csv"), file.path(wq_dir, "TP_CowCreek.csv"))

cat("Fitting Jacksonville...\n")
res_jacksonville <- fit_station(file.path(sal_dir, "Jacksonville_Salinity.csv"), file.path(wq_dir, "TP_Jacksonville.csv"))

# 5. Generate individual plots limiting legends to 1 decimal place
p1 <- gridplot(res_rockledge, month = 'all', lwd = 2) + 
  ggtitle('Rockledge') + 
  my_theme +
  scale_fill_gradientn(colours = gradcols(), labels = function(x) sprintf("%.1f", x))

p2 <- gridplot(res_sanford, month = 'all', lwd = 2) + 
  ggtitle('Sanford') + 
  my_theme +
  scale_fill_gradientn(colours = gradcols(), labels = function(x) sprintf("%.1f", x))

p3 <- gridplot(res_cowcreek, month = 'all', lwd = 2) + 
  ggtitle('Cow Creek') + 
  my_theme +
  scale_fill_gradientn(colours = gradcols(), labels = function(x) sprintf("%.1f", x))

p4 <- gridplot(res_jacksonville, month = 'all', lwd = 2) + 
  ggtitle('Jacksonville') + 
  my_theme +
  scale_fill_gradientn(colours = gradcols(), labels = function(x) sprintf("%.1f", x))

# 6. Combine 2x2 layout
combined <- (p1 + p2) / (p3 + p4)

# 7. Save to correct Figures directory
out_fig <- file.path(fig_dir, "combined_gridplot_4stations_TP.jpeg")

jpeg(out_fig, width = 16, height = 12, units = "in", res = 600)
print(combined)
dev.off()

cat("Done! Saved as", out_fig, "\n")
