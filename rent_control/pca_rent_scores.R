library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

df <- read_excel("../rent_control_scored_output.xlsx")
score_cols <- c("max_rent_increase", "sf_exempt", "cpi_tied", 
                "new_construction_exempt", "hardship_appeals", "units_covered")
plot(df$rent_control_unweighted,df$rent_control_pca)

df_pca <- df %>%
  drop_na(all_of(score_cols))

# Run PCA
pca_result <- PCA(df_pca[score_cols], scale.unit = TRUE, graph = FALSE)

# Visuals
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(pca_result,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# fviz_pca_biplot(pca_result, 
#                 label = "var", 
#                 habillage = rent_control$Municipality, 
#                 addEllipses = FALSE, 
#                 repel = TRUE)

fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 100))


pca_scores <- as.data.frame(pca_result$ind$coord)

df_pca$rent_control_pca_index <- pca_scores$Dim.1

df_final <- df %>%
  left_join(df_pca %>% select(Municipality, rent_control_pca_index), by = "Municipality")

df_final$`Rent Increase Limit`

score_vars <- c("max_rent_increase", "sf_exempt", "cpi_tied", "new_construction_exempt", "hardship_appeals", "units_covered")
df_scores <- df_final %>% select(all_of(score_vars)) %>% na.omit()

# Run PCA
pca_result <- PCA(df_scores, scale.unit = TRUE, graph = FALSE)

# Extract PC1 loadings (aka coordinates or eigenvectors)
pc1_loadings <- pca_result$var$coord[, 1]

# Normalize loadings to sum to 1 for interpretability
pc1_weights <- pc1_loadings / sum(abs(pc1_loadings))

# Compute the PCA-weighted index
df_final$pca_index <- rowSums(sweep(df_final[, score_vars], 2, pc1_weights, `*`), na.rm = TRUE)

plot(df_final$rent_control_weighted,df_final$pca_index)
plot(df_final$rent_control_unweighted,df_final$pca_index)



# write.csv(df_final, "rent_control_with_pca_index.csv", row.names = FALSE)