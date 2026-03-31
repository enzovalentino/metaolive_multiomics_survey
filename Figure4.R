# loading metadata and species
# on the GitHub repository, data for getting panel 4D is in file 'fig4d.xlsx'
fig4dtab <- readxl::read_xlsx("fig4d.xlsx")
fig4dtab <- as.data.frame(fig4dtab)
rownames(fig4dtab) <- fig4dtab$Sample
fig4dtab <- as.data.frame(fig4dtab[,-1])
fig4dtab <- as.data.frame(scale(fig4dtab))


# loading libraries
if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}

if(!require(pheatmap)){
    install.packages("pheatmap")
    library(pheatmap)
}

if(!require(ggpubr)){
    install.packages("ggpubr")
    library(pheatmap)
}

pheatmap(t(fig4dtab), 
         cluster_rows = TRUE, 
         cluster_cols = TRUE, 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "ward.D", 
         show_colnames = T, 
         angle_col = 315,
         drop_levels = T, 
         border_color = NA, 
         fontsize = 10)

#  on the GitHub repository, data for getting panel 4C is in file 'fig4c.xlsx'
fig4ctab <- readxl::read_xlsx("fig5c.xlsx")
fig4ctab <- as.data.frame(fig4ctab)
rownames(fig4ctab) <- fig4ctab$sample
fig4ctab <- as.data.frame(fig4ctab[,-1])
fig4ctab <- as.data.frame(scale(fig4ctab))

pheatmap(t(fig4ctab), 
         cluster_rows = TRUE, 
         cluster_cols = TRUE, 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "ward.D", 
         show_colnames = T, 
         angle_col = 315,
         drop_levels = T, 
         border_color = NA, 
         fontsize = 10)


#  on the GitHub repository, data for getting panel 4B is in file 'fig4b.xlsx'
fig4btab <- readxl::read_xlsx("fig4b.xlsx")
fig4btab <- as.data.frame(fig4btab)

fig4bpca <- prcomp(fig4btab[,c(4:14)], center = T, scale. = T)

fig4bpca_pt <- as.data.frame(fig4bpca$x)
fig4bpca_pt <- as.data.frame(cbind(fig4bpca_pt, fig4btab[,c(1:3,15)]))
fig4bpca_pt$olive_trade_prep <- gsub(" \\(.*", "", fig4bpca_pt$olive_trade_prep)

library(ggplot2)
library(ggpubr)
ggplot(fig4bpca_pt, aes(x = PC1, y = PC2))+
  geom_point(aes(color = Fermentation_class, shape = olive_variety), size = 4)+
  labs(x = paste0("PC1 (", round(summary(fig4bpca)$importance[2,1]*100, digits = 2), " %)"),
       y = paste0("PC2 (", summary(fig4bpca)$importance[2,2]*100, " %)"), 
       color = "", 
       linetype = "",
       shape = "")+
  scale_color_manual(values = c("days ≤ 170" = "gray", "170 < days ≤ 240" = "#F6C54D", "240 < days ≤ 365" = "#4FAE62", "365 < days ≤ 850" = "#C02D45", "before fermentation" = "#828282"))+
  stat_ellipse(aes(linetype = olive_trade_prep), color = "black", linewidth = .3)+
  theme_classic2()+
  theme(legend.position = "top", legend.box = "vertical",
        axis.title = element_text(size = 14), legend.text = element_text(size = 11))




