# ============================================
# TKN ↔ Land-Cover Changes: Full Analysis (1999–2024)
# ============================================
install.packages(c("tidyverse","tibble","corrplot","factoextra","openxlsx"))

library(tidyverse)
library(tibble)
library(corrplot)
library(factoextra)
library(openxlsx)

# =========================
# 0) Plot output preference
# =========================
# FALSE = show figures in RStudio Plots pane
# TRUE  = save figures to PNG files
SAVE_PNG <- TRUE

# ---------------------------
# 1) Input data (from you)
# ---------------------------
landcover <- tribble(
  ~Site,          ~Water, ~Developed, ~`Barren Land`, ~Forest, ~Agriculture, ~Wetlands,
  "Rockledge",     -3.32,      25.94,         -81.61,   30.27,          0.10,     -0.21,
  "Sanford",        0.49,      19.20,         -23.57,  -44.37,        -65.41,     -4.20,
  "Cow Creek",      0.96,      19.82,          34.42,  -14.30,          4.41,     -0.96,
  "Jacksonville",  -0.46,       0.32,             NA,  -47.79,            NA,     -5.41,
  "Full Watershed",  1.22,      28.07,          47.35,   -4.91,       -13.29,     -1.77
)

tkn <- tribble(
  ~Site,         ~TKN_conc_change_pct_1999_2024, ~TKN_flux_change_pct_1999_2024,
  "Rockledge",                 40,                               18,
  "Sanford",                  -14,                              -21,
  "Cow Creek",                 76,                               67,
  "Jacksonville",             -35,                              -45
)

combined   <- landcover %>% left_join(tkn, by = "Site")
sites_only <- combined %>% filter(Site != "Full Watershed")

# ---------------------------
# 1a) Write primary CSVs (TKN* filenames)
# ---------------------------
readr::write_csv(combined,   "TKN_RawCombinedData.csv")
readr::write_csv(sites_only, "TKN_SitesOnly.csv")

# ------------------------------------------------------
# 2) Correlations (Pearson r) WITH p-values (pairwise)
# ------------------------------------------------------
land_cols <- c("Water","Developed","Barren Land","Forest","Agriculture","Wetlands")
targets   <- c("TKN_conc_change_pct_1999_2024","TKN_flux_change_pct_1999_2024")

corr_rows <- list()
for (lc in land_cols) {
  for (tgt in targets) {
    tmp <- sites_only %>% select(all_of(c(lc, tgt))) %>% drop_na()
    n <- nrow(tmp)
    if (n >= 3) {
      ct <- suppressWarnings(cor.test(tmp[[lc]], tmp[[tgt]], method = "pearson"))
      r  <- unname(ct$estimate); p <- ct$p.value
    } else { r <- NA_real_; p <- NA_real_ }
    corr_rows[[length(corr_rows)+1]] <- tibble(LandCover=lc, Target=tgt, r=r, p_value=p, N=n)
  }
}
corr_df <- bind_rows(corr_rows) %>% arrange(Target, desc(abs(r)))
readr::write_csv(corr_df, "TKN_Correlations_with_p.csv")

# Single-column matrices for heatmaps
heat_conc <- corr_df %>% filter(Target == targets[1]) %>%
  select(LandCover, r) %>% column_to_rownames("LandCover") %>% as.matrix()
heat_flux <- corr_df %>% filter(Target == targets[2]) %>%
  select(LandCover, r) %>% column_to_rownames("LandCover") %>% as.matrix()

# -------------------------
# 2a) Improved correlation heatmaps (white background, no legend overlap)
# -------------------------
plot_heatmap_singlecol <- function(mat, title, file = NULL, colors = NULL) {
  if (isTRUE(SAVE_PNG) && !is.null(file)) {
    png(file, width = 2200, height = 1200, res = 200, bg = "white")
    on.exit(dev.off(), add = TRUE)
  }
  op <- par(bg = "white"); on.exit(par(op), add = TRUE)
  if (is.null(colors)) colors <- colorRampPalette(c("#1b7837","white","#762a83"))(200)
  
  corrplot(
    mat %*% t(rep(1,1)),
    is.corr = FALSE,
    method = "color",
    tl.col = "black",
    tl.srt = 0,
    cl.pos = "n",                # remove colorbar to avoid overlap
    col = colors,
    mar = c(1, 10, 6, 2),
    title = title,
    addgrid.col = "gray90"
  )
  text(1, seq_len(nrow(mat)),
       labels = sprintf("%.2f", mat[,1]),
       cex = 1.15, font = 2)
}

plot_heatmap_singlecol(
  heat_conc,
  "TKN: Correlation (r) — Land Cover vs TKN Concentration % Change (1999–2024)",
  if (SAVE_PNG) "TKN_heatmap_r_conc.png" else NULL,
  colors = colorRampPalette(c("navy","white","darkred"))(200)
)
plot_heatmap_singlecol(
  heat_flux,
  "TKN: Correlation (r) — Land Cover vs TKN Flux % Change (1999–2024)",
  if (SAVE_PNG) "TKN_heatmap_r_flux.png" else NULL,
  colors = colorRampPalette(c("goldenrod","white","brown4"))(200)
)

# -----------------------------------
# 3) PCA (on complete columns only)
# -----------------------------------
pca_vars <- c("Water","Developed","Forest","Wetlands",
              "TKN_conc_change_pct_1999_2024","TKN_flux_change_pct_1999_2024")

pca_data <- sites_only %>%
  select(Site, all_of(pca_vars)) %>%
  drop_na()

row.names(pca_data) <- pca_data$Site
X <- pca_data %>% select(-Site)

pca <- prcomp(X, center = TRUE, scale. = TRUE)

eig_tbl <- factoextra::get_eigenvalue(pca) %>%
  as_tibble(rownames = "Component") %>%
  rename(Eigenvalue = eigenvalue,
         Variance_Explained = `variance.percent`,
         Cumulative_Variance = `cumulative.variance.percent`)
readr::write_csv(eig_tbl, "TKN_PCA_Eigenvalues.csv")

loadings_tbl <- factoextra::get_pca_var(pca)$coord %>%
  as.data.frame() %>% rownames_to_column("Variable") %>% as_tibble()
keep_cols <- intersect(c("PC1","PC2","PC3"), names(loadings_tbl))
loadings_export <- loadings_tbl %>% select(Variable, all_of(keep_cols))
readr::write_csv(loadings_export, "TKN_PCA_Loadings_PC1_PC3.csv")

scores_tbl <- factoextra::get_pca_ind(pca)$coord %>%
  as.data.frame() %>% rownames_to_column("Site") %>% as_tibble()
readr::write_csv(scores_tbl, "TKN_PCA_Scores.csv")

# --------------------------
# 4) Publication plots (white background)
# --------------------------
white_theme <- theme(
  panel.background = element_rect(fill = "white", color = NA),
  plot.background  = element_rect(fill = "white", color = NA),
  legend.background= element_rect(fill = "white", color = NA),
  legend.key       = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
  panel.grid.minor = element_line(color = "gray92", linewidth = 0.2),
  axis.line        = element_line(color = "black"),
  axis.ticks       = element_line(color = "black"),
  axis.text        = element_text(color = "black"),
  axis.title       = element_text(color = "black", face = "bold"),
  plot.title       = element_text(hjust = 0.5, face = "bold")
)

# Scree
p_scree <- fviz_eig(pca, addlabels = TRUE) +
  theme_minimal(base_size = 13) + white_theme + ggtitle("TKN: Scree Plot")
if (SAVE_PNG) {
  ggplot2::ggsave("TKN_plot_scree.png", p_scree, width = 7, height = 5, dpi = 300, bg = "white")
} else { print(p_scree) }

# Biplot
p_biplot <- fviz_pca_biplot(
  pca, repel = TRUE,
  geom.ind = "point",
  col.ind = pca_data$Site, pointshape = 21, alpha.ind = 0.95,
  col.var = "black", arrowsize = 0.7, labelsize = 4
) + theme_minimal(base_size = 13) + white_theme +
  ggtitle("TKN: PCA Biplot — TKN & Land-Cover Changes (1999–2024)") +
  xlab("PC1") + ylab("PC2")
if (SAVE_PNG) {
  ggplot2::ggsave("TKN_plot_biplot.png", p_biplot, width = 8, height = 6.5, dpi = 300, bg = "white")
} else { print(p_biplot) }

# ----------------------------------------
# 5) Write everything to ONE Excel workbook (TKN* filename & sheets)
# ----------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "TKN_RawCombinedData");      writeData(wb, "TKN_RawCombinedData", combined)
addWorksheet(wb, "TKN_SitesOnly");            writeData(wb, "TKN_SitesOnly", sites_only)
addWorksheet(wb, "TKN_Correlations_with_p");  writeData(wb, "TKN_Correlations_with_p", corr_df)
addWorksheet(wb, "TKN_PCA_Eigenvalues");      writeData(wb, "TKN_PCA_Eigenvalues", eig_tbl)
addWorksheet(wb, "TKN_PCA_Loadings_PC1_PC3"); writeData(wb, "TKN_PCA_Loadings_PC1_PC3", loadings_export)
addWorksheet(wb, "TKN_PCA_Scores");           writeData(wb, "TKN_PCA_Scores", scores_tbl)
saveWorkbook(wb, "TKN_Combined_Analysis_LandCover.xlsx", overwrite = TRUE)

cat("\nOutputs written:\n",
    "- TKN_Combined_Analysis_LandCover.xlsx (all tables)\n",
    "- TKN_RawCombinedData.csv, TKN_SitesOnly.csv\n",
    "- TKN_Correlations_with_p.csv\n",
    "- TKN_PCA_Eigenvalues.csv, TKN_PCA_Loadings_PC1_PC3.csv, TKN_PCA_Scores.csv\n",
    "- TKN_heatmap_r_conc.png, TKN_heatmap_r_flux.png\n",
    "- TKN_plot_scree.png, TKN_plot_biplot.png\n")

message("Note: PCA is exploratory only (N=4 sites). ",
        "Correlations report r, p, and N with pairwise deletion.")

