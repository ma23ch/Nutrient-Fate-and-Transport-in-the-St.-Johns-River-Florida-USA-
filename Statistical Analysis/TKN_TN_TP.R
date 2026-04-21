library(tidyverse)
library(factoextra)
library(patchwork)

# ---------------------------
# Shared data
# ---------------------------
landcover <- tribble(
  ~Site,           ~Water, ~Developed, ~Forest, ~Wetlands,
  "Rockledge",      -3.32,      25.94,   30.27,     -0.21,
  "Sanford",         0.49,      19.20,  -44.37,     -4.20,
  "Cow Creek",       0.96,      19.82,  -14.30,     -0.96,
  "Jacksonville",   -0.46,       0.32,  -47.79,     -5.41
)

tkn <- tribble(
  ~Site,           ~TKN_conc, ~TKN_flux,
  "Rockledge",          40,        18,
  "Sanford",           -14,       -21,
  "Cow Creek",          76,        67,
  "Jacksonville",      -35,       -45
)

tn <- tribble(
  ~Site,           ~TN_conc, ~TN_flux,
  "Rockledge",         23,       0.8,
  "Sanford",            8.9,     3.8,
  "Cow Creek",        -26,     -27,
  "Jacksonville",     -23,     -25
)

tp <- tribble(
  ~Site,           ~TP_conc, ~TP_flux,
  "Rockledge",        -73,      -76,
  "Sanford",          -32,      -41,
  "Cow Creek",        -70,      -75,
  "Jacksonville",     -28,      -30
)

# ---------------------------
# Helper: build PCA + biplot
# ---------------------------
make_biplot <- function(nutrient_df, title) {
  df <- landcover %>% left_join(nutrient_df, by = "Site")
  X  <- df %>% select(-Site) %>% as.data.frame()
  rownames(X) <- df$Site
  
  pca <- prcomp(X, center = TRUE, scale. = TRUE)
  
  var_exp <- summary(pca)$importance[2, 1:2] * 100
  xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
  ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])
  
  fviz_pca_biplot(
    pca,
    repel       = TRUE,
    geom.ind    = "point",
    col.ind     = df$Site,
    fill.ind    = df$Site,
    pointshape  = 21,
    pointsize   = 3,
    alpha.ind   = 0.95,
    col.var     = "gray20",
    arrowsize   = 0.6,
    labelsize   = 7.5
  ) +
    xlab(xlab) + ylab(ylab) +
    guides(color = "none", fill = "none") +
    theme_minimal(base_size = 15) +
    theme(
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.position   = "none",
      panel.grid.major  = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      axis.line         = element_line(color = "black", linewidth = 0.4),
      axis.ticks        = element_line(color = "black"),
      axis.text         = element_text(color = "black"),
      axis.title        = element_text(color = "black", face = "bold"),
      plot.title        = element_text(hjust = 0.5, face = "bold", size = 15)
    ) +
    ggtitle(title)
}

p1 <- make_biplot(tkn, "A) TKN")
p2 <- make_biplot(tn,  "B) TN")
p3 <- make_biplot(tp,  "C) TP")

# ---------------------------
# Extract colors used by factoextra
# so legend matches exactly
# ---------------------------
site_levels <- c("Rockledge", "Sanford", "Cow Creek", "Jacksonville")
site_colors <- scales::hue_pal()(4)
names(site_colors) <- site_levels

# ---------------------------
# Legend panel in slot D
# ---------------------------
legend_df <- data.frame(
  Site = factor(site_levels, levels = site_levels),
  x    = 1,
  y    = rev(seq_along(site_levels))
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, fill = Site)) +
  geom_point(shape = 21, size = 5, color = "white", stroke = 0.8) +
  geom_text(aes(x = 1.15, label = Site),
            hjust = 0, size = 8, color = "black") +
  annotate("text", x = 0.85, y = max(legend_df$y) + 0.7,
           label = "Site", fontface = "bold", size = 8.5,
           hjust = 0, color = "black") +
  scale_fill_manual(values = site_colors) +
  scale_x_continuous(limits = c(0.8, 2.2)) +
  scale_y_continuous(limits = c(0.5, max(legend_df$y) + 1)) +
  guides(fill = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

# ---------------------------
# Combine: 2x2 grid, D = legend
# ---------------------------
combined_plot <- (p1 + p2) / (p3 + legend_plot) +
  plot_layout(guides = "keep") &
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

combined_plot <- combined_plot +
  plot_annotation(
    title   = "PCA Biplots: Nutrient & Land-Cover Changes (1999–2024)",
    caption = "N = 4 sites. PCA on Water, Developed, Forest, Wetlands + nutrient % change (conc & flux).\nBarren Land and Agriculture excluded due to missing values.",
    theme = theme(
      plot.caption    = element_text(color = "gray40", size = 12),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave("Combined_PCA_Biplots_TKN_TN_TP.png",
       combined_plot,
       width = 11.69, height = 8.27, dpi = 600, bg = "white")

cat("Saved: Combined_PCA_Biplots_TKN_TN_TP.png\n")
