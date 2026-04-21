library(WRTDStidal)
library(tidyverse)
library(patchwork)
library(plotly)

# label for plots
ylb <- 'TKN (mg/L)'

# functions --------------------------------------------------------------

# function to interpolate data to monthly timestep
interp_fun <- function(saldat, nutdat) {
  
  # Ensure date columns are in Date format
  saldat$date <- as.Date(saldat$date)
  nutdat$date <- as.Date(nutdat$date)
  
  # Sort both datasets by date to ensure proper interpolation
  saldat <- saldat[order(saldat$date), ]
  nutdat <- nutdat[order(nutdat$date), ]
  
  # Create monthly date sequence spanning the range of both datasets
  start_date <- min(nutdat$date)
  end_date <- max(nutdat$date)
  
  # Generate first day of each month in the range
  monthly_dates <- seq(from = as.Date(paste0(format(start_date, "%Y-%m"), "-01")),
                       to = as.Date(paste0(format(end_date, "%Y-%m"), "-01")),
                       by = "month")
  
  # Interpolate salinity to monthly dates
  sal_monthly <- approx(x = saldat$date, 
                        y = saldat$sal, 
                        xout = monthly_dates, 
                        method = "linear",
                        rule = 2)
  
  # Interpolate TKN to monthly dates
  res_monthly <- approx(x = nutdat$date, 
                        y = nutdat$res, 
                        xout = monthly_dates, 
                        method = "linear",
                        rule = 2)
  
  # Create output dataframe with monthly observations
  out <- data.frame(
    date = monthly_dates,
    res_interpolated = res_monthly$y,
    sal_interpolated = sal_monthly$y
  )
  
  return(out)
  
}

# plotting function
plo_fun <- function(datin, yvar, ylab){
  
  toplo <- datin |> 
    rename(
      yvr = !!yvar
    )
  
  p <- ggplot(toplo, aes(x = date, y = yvr)) +
    geom_point() +
    geom_line() + 
    labs(y = ylab, x = NULL) +
    theme_minimal()
  
  return(p)
  
}

# data import and prep ---------------------------------------------------

# import salinity
saldat <- read.csv("C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Salinity/Jacksonville_Salinity.csv") |> 
  select(date = Date, sal = Value) |> 
  mutate(
    date = as.Date(date, format = "%m/%d/%Y")
  ) |> 
  filter(!is.na(sal))

# import nutrients
nutdat <- read.csv("C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Water Quality/TKN_Jacksonville.csv") |> 
  select(date = Date, res = Value, detect = Remark) |> 
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    detect = ifelse(detect == "<", T, F)
  ) |> 
  filter(!is.na(res))

# use interpolation function
intdat <- interp_fun(saldat, nutdat)

# plot raw and interpolated data -----------------------------------------

p1 <- plo_fun(saldat, yvar = 'sal', ylab = 'Salinity (ppt)')
p2 <- plo_fun(intdat, yvar = 'sal_interpolated', ylab = 'Salinity (ppt) interpolated')
p3 <- plo_fun(nutdat, yvar = 'res', ylab = ylb)
p4 <- plo_fun(intdat, yvar = 'res_interpolated', ylab = paste(ylb, 'interpolated'))

p1 + p2 + p3 + p4 + plot_layout(ncol = 1)

# fit wrtdstidal models --------------------------------------------------

# convert interpolated data to tidalmean object for modfit
totidal <- intdat|> 
  mutate(
    res = log(res_interpolated),
    flo = 1 - (sal_interpolated / 20), # max(sal_interpolated, na.rm = T)), 
    lim = log(0.01) # this is clearly not the right limit
  ) |> 
  select(date, res, flo, lim) |> 
  tidalmean(reslog = T, reslab = ylb, flolab = 'Salinity (ppt)')

# fit model
res <- modfit(totidal, resp_type = 'mean')

# model plots
fitplot(res, annuals = T, logspace= F)
fitplot(res, annual = F, logspace = F)
seasyrplot(res, predicted = F)
prdnrmplot(res, logspace = F, annual = F)
prdnrmplot(res, logspace = F)
dynaplot(res, logspace = F)
gridplot(res)
gridplot(res, month = 'all')

# 3d fit surface
dat <- attr(res, 'fits') %>% 
  .[[1]] %>% 
  select(-date, -year, -month, -day) %>% 
  as.matrix

scene <- list(
  aspectmode = 'manual', 
  aspectratio = list(x = 0.5, y = 1, z = 0.3), 
  xaxis = list(title = 'Salinity (ppt)'), 
  yaxis = list(title = 'Time'), 
  zaxis = list(title = paste0('log', ylb))
)

p <- plot_ly(z = ~dat) %>% 
  add_surface(colors = rev(RColorBrewer::brewer.pal(11, 'Spectral'))) %>% 
  layout(scene = scene)
p
