# ============================================
# TP ↔ Land-Cover Changes: Full Analysis (1999–2024)
# ============================================
# install.packages(c("tidyverse","tibble","corrplot","factoextra","openxlsx"))

library(tidyverse)
library(tibble)
library(corrplot)
library(factoextra)
library(openxlsx)

# =========================
# 0) Plot output preference
# =========================
SAVE_PNG <- TRUE  # TRUE = save PNGs, FALSE = show in RStudio

# ---------------------------
# 1) Input data
# ---------------------------
# ---------------------------
landcover <- tribble(
  ~Site,          ~Water, ~Developed, ~`Barren Land`, ~Forest, ~Agriculture, ~Wetlands,
  "Rockledge",     -3.32,      25.94,         -81.61,   30.27,          0.10,     -0.21,
  "Sanford",        0.49,      19.20,         -23.57,  -44.37,        -65.41,     -4.20,
  "Cow Creek",      0.96,      19.82,          34.42,  -14.30,          4.41,     -0.96,
  "Jacksonville",  -0.46,       0.32,             NA,  -47.79,            NA,     -5.41,
  "Full Watershed",  1.22,      28.07,          47.35,   -4.91,       -13.29,     -1.77
)

tp <- tribble(
  ~Site,         ~TP_conc_change_pct_1999_2024, ~TP_flux_change_pct_1999_2024,
  "Rockledge",                -73,                              -76,
  "Sanford",                  -32,                              -41,
  "Cow Creek",                -70,                              -75,
  "Jacksonville",             -28,                              -30
)

combined   <- landcover %>% left_join(tp, by = "Site")
sites_only <- combined %>% filter(Site != "Full Watershed")

readr::write_csv(combined,   "TP_RawCombinedData_1999_2024.csv")
readr::write_csv(sites_only, "TP_SitesOnly_1999_2024.csv")

# ------------------------------------------------------
# 2) Correlations (Pearson r) WITH p-values (pairwise)
# ------------------------------------------------------
land_cols <- c("Water","Developed","Barren Land","Forest","Agriculture","Wetlands")
targets   <- c("TP_conc_change_pct_1999_2024","TP_flux_change_pct_1999_2024")

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
readr::write_csv(corr_df, "TP_Correlations_with_p_1999_2024.csv")

heat_conc <- corr_df %>% filter(Target == targets[1]) %>%
  select(LandCover, r) %>% column_to_rownames("LandCover") %>% as.matrix()
heat_flux <- corr_df %>% filter(Target == targets[2]) %>%
  select(LandCover, r) %>% column_to_rownames("LandCover") %>% as.matrix()

# -------------------------
# 2a) Improved correlation heatmaps (white background, no overlap)
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
    cl.pos = "n",
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
  "Correlation (r): Land Cover vs TP Concentration % Change (1999–2024)",
  if (SAVE_PNG) "TP_heatmap_r_conc_1999_2024.png" else NULL,
  colors = colorRampPalette(c("navy","white","darkred"))(200)
)
plot_heatmap_singlecol(
  heat_flux,
  "Correlation (r): Land Cover vs TP Flux % Change (1999–2024)",
  if (SAVE_PNG) "TP_heatmap_r_flux_1999_2024.png" else NULL,
  colors = colorRampPalette(c("goldenrod","white","brown4"))(200)
)

# -----------------------------------
# 3) PCA (on complete columns only)
# -----------------------------------
pca_vars <- c("Water","Developed","Forest","Wetlands",
              "TP_conc_change_pct_1999_2024","TP_flux_change_pct_1999_2024")

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
readr::write_csv(eig_tbl, "TP_PCA_Eigenvalues_1999_2024.csv")

loadings_tbl <- factoextra::get_pca_var(pca)$coord %>%
  as.data.frame() %>% rownames_to_column("Variable") %>% as_tibble()
keep_cols <- intersect(c("PC1","PC2","PC3"), names(loadings_tbl))
loadings_export <- loadings_tbl %>% select(Variable, all_of(keep_cols))
readr::write_csv(loadings_export, "TP_PCA_Loadings_PC1_PC3_1999_2024.csv")

scores_tbl <- factoextra::get_pca_ind(pca)$coord %>%
  as.data.frame() %>% rownames_to_column("Site") %>% as_tibble()
readr::write_csv(scores_tbl, "TP_PCA_Scores_1999_2024.csv")

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
  theme_minimal(base_size = 13) + white_theme +
  ggtitle("Scree Plot (TP PCA, 1999–2024)")
if (SAVE_PNG) {
  ggplot2::ggsave("TP_plot_scree_1999_2024.png", p_scree, width = 7, height = 5, dpi = 300, bg = "white")
} else { print(p_scree) }

# Biplot
p_biplot <- fviz_pca_biplot(
  pca, repel = TRUE,
  geom.ind = "point",
  col.ind = pca_data$Site, fill.ind = pca_data$Site,
  pointshape = 21, alpha.ind = 0.95,
  col.var = "black", arrowsize = 0.7, labelsize = 4
) + theme_minimal(base_size = 13) + white_theme +
  ggtitle("PCA Biplot: TP & Land-Cover Changes (1999–2024)") +
  xlab("PC1") + ylab("PC2")
if (SAVE_PNG) {
  ggplot2::ggsave("TP_plot_biplot_1999_2024.png", p_biplot, width = 8, height = 6.5, dpi = 300, bg = "white")
} else { print(p_biplot) }

# ----------------------------------------
# 5) Write everything to ONE Excel workbook
# ----------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "TP_RawCombinedData");      writeData(wb, "TP_RawCombinedData", combined)
addWorksheet(wb, "TP_SitesOnly");            writeData(wb, "TP_SitesOnly", sites_only)
addWorksheet(wb, "TP_Correlations");         writeData(wb, "TP_Correlations", corr_df)
addWorksheet(wb, "TP_PCA_Eigenvalues");      writeData(wb, "TP_PCA_Eigenvalues", eig_tbl)
addWorksheet(wb, "TP_PCA_Loadings_PC1_PC3"); writeData(wb, "TP_PCA_Loadings_PC1_PC3", loadings_export)
addWorksheet(wb, "TP_PCA_Scores");           writeData(wb, "TP_PCA_Scores", scores_tbl)
saveWorkbook(wb, "TP_Combined_Analysis_LandCover_1999_2024.xlsx", overwrite = TRUE)

cat("\nOutputs written:\n",
    "- TP_Combined_Analysis_LandCover_1999_2024.xlsx (all tables)\n",
    "- TP_RawCombinedData_1999_2024.csv, TP_SitesOnly_1999_2024.csv\n",
    "- TP_Correlations_with_p_1999_2024.csv\n",
    "- TP_PCA_Eigenvalues_1999_2024.csv, TP_PCA_Loadings_PC1_PC3_1999_2024.csv, TP_PCA_Scores_1999_2024.csv\n",
    "- TP_heatmap_r_conc_1999_2024.png, TP_heatmap_r_flux_1999_2024.png\n",
    "- TP_plot_scree_1999_2024.png, TP_plot_biplot_1999_2024.png\n")

message("Note: PCA is exploratory only (N=4 sites). ",
        "Correlations report r, p, and N with pairwise deletion.")
