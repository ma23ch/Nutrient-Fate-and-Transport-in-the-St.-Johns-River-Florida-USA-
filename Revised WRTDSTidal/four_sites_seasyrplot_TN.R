library(WRTDStidal)
library(tidyverse)
library(patchwork)

# set default linewidth for all geom_line layers
update_geom_defaults("line", list(linewidth = 1.2))

# label for plots
ylb <- 'log(TN) (mg/L)'

# Define your exact local directories
sal_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Salinity"
wq_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Input_Backup/Water Quality"
fig_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/four_sites_seasyrplot 03092026/combined_gridplot_4stations_code+figures/Figures"

# Corrected function to process data and fit model for a site
process_site <- function(site_name, sal_file, tn_file) {
  
  # import salinity
  saldat <- read.csv(sal_file) |>
    select(date = Date, sal = Value) |>
    mutate(
      date = as.Date(date, format = "%m/%d/%Y")
    ) |>
    filter(!is.na(sal)) |>
    arrange(date)
  
  # import nutrients
  nutdat <- read.csv(tn_file) |>
    select(date = Date, res = Value, detect = Remark) |>
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      not_cens = !(detect %in% c("<", "U", "ND"))
    ) |>
    filter(!is.na(res)) |>
    arrange(date)
  
  # Calculate site-specific Smax for the freshwater fraction calculation
  s_max <- max(saldat$sal, na.rm = TRUE)
  cat("  -> Using Smax =", s_max, "for", site_name, "\n")
  
  # Interpolate salinity strictly to dates of actual nutrient samples
  sal_interp <- approx(
    x = saldat$date, y = saldat$sal,
    xout = nutdat$date, method = "linear", rule = 2
  )
  
  # convert to tidalmean
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
  
  # fit model
  res <- modfit(totidal, resp_type = 'mean')
  
  return(res)
}

# process all four sites using dynamic file paths
cat("Processing Rockledge...\n")
rock_res <- process_site("Rockledge",
                         file.path(sal_dir, "Rockledge_salinity.csv"),
                         file.path(wq_dir, "TN_Rockledge.csv"))

cat("Processing Sanford...\n")
san_res <- process_site("Sanford",
                        file.path(sal_dir, "Sanford_salinity.csv"),
                        file.path(wq_dir, "TN_Sanford.csv"))

cat("Processing Cow Creek...\n")
cow_res <- process_site("CowCreek",
                        file.path(sal_dir, "CowCreek_Salinity.csv"),
                        file.path(wq_dir, "TN_CowCreek.csv"))

cat("Processing Jacksonville...\n")
jac_res <- process_site("Jacksonville",
                        file.path(sal_dir, "Jacksonville_Salinity.csv"),
                        file.path(wq_dir, "TN_Jacksonville.csv"))

# create seasyrplot for each site with site title
# y-axis label: "log(TN)", site name in top-left corner inside plot
p1 <- seasyrplot(rock_res, predicted = F, lwd = 2) +
  ylab("log(TN)") +
  annotate("text", x = -Inf, y = Inf, label = "Rockledge", hjust = -0.1, vjust = 1.5,
           size = 8, fontface = "bold") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 12))

p2 <- seasyrplot(san_res, predicted = F, lwd = 2) +
  ylab("log(TN)") +
  annotate("text", x = -Inf, y = Inf, label = "Sanford", hjust = -0.1, vjust = 1.5,
           size = 8, fontface = "bold") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 12))

p3 <- seasyrplot(cow_res, predicted = F, lwd = 2) +
  ylab("log(TN)") +
  annotate("text", x = -Inf, y = Inf, label = "Cow Creek", hjust = -0.1, vjust = 1.5,
           size = 8, fontface = "bold") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 12))

p4 <- seasyrplot(jac_res, predicted = F, lwd = 2) +
  ylab("log(TN)") +
  annotate("text", x = -Inf, y = Inf, label = "Jacksonville", hjust = -0.1, vjust = 1.5,
           size = 8, fontface = "bold") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 12))

# combine into 2x2 layout, no legend
combined_plot <- (p1 + theme(legend.position = "none") |
                    p2 + theme(legend.position = "none")) /
  (p3 + theme(legend.position = "none") |
     p4 + theme(legend.position = "none")) +
  plot_layout() +
  theme(plot.margin = margin(t = 10, r = 10, b = 20, l = 10))

# save combined plot
out_fig <- file.path(fig_dir, "four_sites_seasyrplot_TN.jpeg")
jpeg(out_fig, width = 14, height = 12, units = "in", res = 600)
print(combined_plot)
dev.off()

cat("Combined plot saved as:", out_fig, "\n")
