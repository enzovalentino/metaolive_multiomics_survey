# loading VOCs concentration, correlations between VOCs and species, n. of matches of genes associated with flavour development in metagenomes and their taxonomic identification, relative abundances of species and metadata
# in the GitHub repository, the files are: 
# 1. METAolive_VOCs.xlsx
# 2. correlations_species_vocs.txt
# 3. flavour_presence_absence.txt
# 4. kraken_flavour_metagenomes.out
# 5. species.xlsx
# 6. metadata.txt

# loading libraries
if(!require(patchwork)){
    install.packages("patchwork")
    library(patchwork)
}

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}

if(!require(pheatmap)){
    install.packages("pheatmap")
    library(pheatmap)
}

if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
}

if(!require(corrr)){
    install.packages("corrr")
    library(corrr)
}

if(!require(ggplotify)){
    install.packages("ggplotify")
    library(ggplotify)
}

if(!require(corrplot)){
    install.packages("corrplot")
    library(corrplot)
}


if(!require(tidygraph)){
    install.packages("tidygraph")
    library(tidygraph)
}

if(!require(ggraph)){
    install.packages("ggraph")
    library(ggraph)
}

# Panel A ====
# corrplot of the VOCs concentration and species relative abundance
vocs <- readxl::read_xlsx("METAolive_VOCs.xlsx", sheet = 1)
vocs_cat <- readxl::read_xlsx("METAolive_VOCs.xlsx", sheet = 2)
vocs <- select(vocs, all_of(c(vocs_cat$Compound, "Sample_name")))

vocs <- as.data.frame(vocs)
rownames(vocs) <- vocs$Sample_name
vocs <- vocs %>% rename(`Sample Title` = Sample_name)
length(which(rownames(vocs) %in% species$`Sample Title`))

vocs <- left_join(vocs, species)

compounds <- vocs_cat$Compound
compounds2 <- filter(vocs_cat, Class %in% c("esters", "acids"))$Compound # solo composti attribuibili a fermentazione, non varietali...
compounds3 <- filter(vocs_cat, !(Class %in% c("esters", "acids")))$Compound # solo varietali

correlations3 <- read.table("correlations_species_vocs.txt", header = T, sep = "\t", dec = ".", quote = "")
correlations3_filt <- filter(correlations3, r > .3)
cor.graph3 <- as_tbl_graph(correlations3_filt, directed = FALSE) # dove mi sono fermato

# adding annotation
annot3 <- data_frame()
for (i in 1:length(cor.graph3)) {
  tmp <- names(cor.graph3[[i]])
  annot3 <- rbind(annot3, tmp)
  rm(tmp)
}
rm(i)
colnames(annot3)[1] <- "name"
annot3$category <- ifelse(annot3[,1] %in% names_species, "Taxon", "VOC")

cor.graph3 <- cor.graph3 %>%
  activate(nodes) %>% # extracts nodes names and other information from cor.graph
  left_join(annot3, by = "name") %>%
  rename(label = name)

cor.graph3 <- cor.graph3 %>%
  activate(edges) %>%
  rename(weight = r)

# plotting
set.seed(123) # for reproducibility
cor.graph3 %>% activate(nodes) %>% mutate(cluster = as.factor(group_infomap())) %>% # adds cluster information to the nodes data frame
  ggraph(layout = "graphopt") + 
  geom_edge_link(aes(width = abs(weight), color = weight), check_overlap = T) +
  geom_node_point(aes(shape = category), size = 3) +
  geom_node_text(aes(label = label), repel = TRUE, check_overlap = T, max.overlaps = 5)+
  ggraph::scale_edge_width(range = c(.4, 1.5))+ # to scale the depth of the lines
  scale_edge_colour_gradient2(low = alpha("#991006", .65), high = alpha("#579906", .65), mid = alpha("#ffffff", .65), midpoint = 0)+
  theme(panel.background = element_blank())

# plot was edited with Gephy

# Panel B ====
# bubble plot showing the # of positive correlations between species and VOC categories
correlations3_filt_2 <- correlations3_filt
correlations3_filt_2 <- correlations3_filt_2 %>% rename(Compound = y)
correlations3_filt_2 <- left_join(correlations3_filt_2, vocs_cat)

# panel B correlation matrix
correlations3_filt_2 <- correlations3_filt_2 %>% group_by(x, Class) %>% count()
correlations3_filt_2 %>% ggplot(aes(x = Class, y = x, size = n, fill = Class))+
  geom_point(shape = 21, color = "darkgray")+
  labs(x = "", y = "", size = "# correlations")+
  ggpubr::theme_pubr(legend = "right")+
  theme(axis.text.y = element_text(face = "italic"), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  scale_y_discrete(labels = function (x) gsub("_", " ", x))+
  scale_fill_aaas()+
  scale_size(range = c(4,10))+
  guides(fill = 'none')

# Panel C ====
# this panel is divided into 2 parts: the heatmap with the # of matches and the bar plot with taxonomic assignment of genes
flavour_genelev_tab <- read.table("flavour_presence_absence.txt", header = T, sep = "\t", dec = ".", quote = "")
sampleids <- colnames(flavour_genelev_tab)[1:271] # 271 is the # of samples
tmpp <- as.data.frame(t(flavour_genelev_tab %>% select(all_of(sampleids)))) %>% 
  mutate(across(everything(), ~ifelse(. >= 1, 1, 0))) %>%
  #rowwise() %>%
  select(where(~ (sum(.x >0) >= .1*length(.x))))

tmpp <- colnames(tmpp)

myheatflav <- pheatmap((flavour_genelev_tab[which(rownames(flavour_genelev_tab) %in% tmpp),] %>% select(all_of(sampleids))), 
         cluster_rows = TRUE, 
         cluster_cols = TRUE, 
         clustering_distance_rows = "minkowski", 
         clustering_distance_cols = "minkowski", 
         clustering_method = "ward.D", 
         annotation_row = flavour_genelev_tab[,c(284, 284)],  # broad class
         annotation_col = meta2[,which(colnames(meta2) %in% c("Preparation", "Fermentation_class"))],
         show_colnames = F, 
         show_rownames = T, 
         annotation_colors = mypal,
         drop_levels = T, 
         #labels_row = flavour_genelev_tab[which(rownames(flavour_genelev_tab) %in% tmpp),"kegg_orthology"],
         cutree_cols = 4, 
         border_color = NA, 
         fontsize = 11, 
         color = colorRampPalette(rev(brewer.pal(n = 8, name = "GnBu")))(200))


# barplot for genes taxonomic labelling
# I need to extract the order of the genes from the heatmap
row_order <- myheatflav[["tree_row"]][["order"]]
tmp <- flavour_genelev_tab[which(rownames(flavour_genelev_tab) %in% tmpp),]
tmp[row_order,] # l'ordine corrisponde a quello della heatmap!
rm(tmp)

# faccio un vettore con i nomi delle righe ordinati (mi serve dopo x barplot)
names_order <- myheatflav[["tree_row"]][["labels"]][row_order]

flavor_genes_tax <- read.table("kraken_flavour_metagenomes.out", header = F, sep = "\t", dec = ".", quote = "")
flavor_genes_tax <- flavor_genes_tax[,c(2,3)]

# most abundant taxa
int_taxa <- flavor_genes_tax %>% group_by(V3) %>% count() %>% arrange(desc(n)) %>% head(n = 9)
int_taxa <- int_taxa$V3

colnames(flavor_genes_tax) <- c("qseqid", "taxonomy")
flavour_filtered_tax <- left_join(flavour_filtered, flavor_genes_tax)
flavour_filtered_tax$taxonomy <- ifelse(flavour_filtered_tax$taxonomy %in% int_taxa, flavour_filtered_tax$taxonomy, "Others")

flavour_filtered_tax <- flavour_filtered_tax %>% filter(sseqid %in% flavour_genelev_tab$sseqid)

flavour_filtered_tax <- flavour_filtered_tax %>% group_by(sseqid, taxonomy) %>% count() %>% ungroup() %>% group_by(sseqid) %>% mutate(perc = n/sum(n)*100)
gene_count <- flavour_filtered %>% group_by(sseqid) %>% count()
flavour_filtered_tax <- left_join(flavour_filtered_tax, gene_count)

b <- ggplot((flavour_filtered_tax %>% filter(sseqid %in% tmpp)), aes(x = sseqid, y = perc, fill = taxonomy))+
  geom_col(position = "stack", color = "black", linewidth = .3)+
  coord_flip()+
  labs(x = "", y = "% of matches")+
  scale_y_continuous(labels = function (x) {paste0(x, " %")})+
  scale_x_discrete(limits = as.factor(rev(names_order)))+
  scale_fill_viridis_d(option = "turbo")#+scale_fill_manual(values = c(ggokabeito::palette_okabe_ito()[1:8], ggsci::pal_jco()(7)))

ggplotify::as.ggplot(myheatflav) + b + plot_layout(nrow = 1, widths = c(7,1), heights = c(1,1))
