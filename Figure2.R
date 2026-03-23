# loading metadata and species
# in the GitHub repository, the files are called "metadata.txt" and "species.xlsx", respectively
meta <- read.table("metadata.txt", header = T, sep = "\t", dec = ".", quote = "")
species <- readxl::read_xlsx("species.xlsx", sheet = 1)
species <- as.data.frame(species)
rownames(species) <- species$ID
species <- as.data.frame(t(species[,-1]))
names_species <- colnames(species)
species$Sample <- rownames(species)

species <- left_join(species, meta)
species <- as.data.frame(species)
rownames(species) <- species$Sample
species$Preparation2 <- ifelse(species$Preparation %in% c("Natural olives", "Alkali treated olives", "Specialties"), species$Preparation, "Others")


# loading libraries
if(!require(patchwork)){
    install.packages("patchwork")
    library(patchwork)
}

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}

if(!require(ggpubr)){
    install.packages("ggpubr")
    library(ggpubr)
}

if(!require(readxl)){
    install.packages("readxl")
    library(readxl)
}

if(!require(hrbrthemes)){
    install.packages("hrbrthemes")
    library(hrbrthemes)
}

if(!require(viridis)){
    install.packages("viridis")
    library(viridis)
}

if(!require(ggalign)){
    install.packages("ggalign")
    library(ggalign)
}


if(!require(vegan)){
    install.packages("vegan")
    library(vegan)
}

# Panel A ====
## Computing alpha diversity ====
species_diversity <- data.frame(cbind(Shannon = vegan::diversity(select(species, all_of(names_species))), Simpson = vegan::diversity(select(species, all_of(names_species)), index = "simpson")))
species_diversity$Sample <- rownames(species_diversity)
species_diversity <- left_join(species_diversity, meta)

species_diversity <- species_diversity %>% pivot_longer(cols = c(Shannon, Simpson))

## Selecting LAB species ====
# 1. Lactobacilli
lactobacilli_names <- c(unique(grep("bacillus", gsub("_.*", "", colnames(species)), value = T)), "Pediococcus")
toexcl <- c("Oceanobacillus", 
            "Gracilibacillus", 
            "Sediminibacillus", 
            "Peribacillus", 
            "Virgibacillus", 
            "Alkalihalobacillus", 
            "Streptohalobacillus", 
            "Halolactibacillus", # fa parte delle bacillaceae
            "Paraliobacillus",
            "Amphibacillus",
            "Ornithinibacillus",
            "Terribacillus",
            "Novibacillus",
            "Lysinibacillus",
            "Paenibacillus")

lactobacilli_names <- lactobacilli_names[-(which(lactobacilli_names %in% toexcl))]
# Streptococcaceae
streptococci_names <- unique(grep(paste0(c("Streptococcus", "Lactococcus"), collapse = "|"), gsub("_.*", "", colnames(species)), value = T))

# Carnobacteriaceae
carnobacteriaceae_names <- unique(grep(paste0(c("Alkalibacterium", "Allofustis", "Alloiococcus", "Atopobacter", "Atopococcus", "Atopostipes", "Carnobacterium", "Carnococcus", "Desemzia", "Dolosigranulum", "Granulicatella", "Isobaculum", "Jeotgalibaca", "Lacticigenium", "Lactosphaera", "Marinilactobacillus", "Pisciglobus", "Trichococcus"), collapse = "|"), gsub("_.*", "", colnames(species)), value = T))

# Enterococcaceae
enterococcaceae_names <- unique(grep(paste0(c("Bavariicoccus", "Catellicoccus", "Enterococcus", "Melisococcus", "Melissococcus", "Pilibacter", "Tetragenococcus", "Vagococcus"), collapse = "|"), gsub("_.*", "", colnames(species)), value = T))

# Leuconostocaceae
leuconostocaceae_names <- unique(grep(paste0(c("Leuconostoc"), collapse = "|"), gsub("_.*", "", colnames(species)), value = T))

# various
lab_names <- unique(grep(paste0(c("Weissella", "Oenococcus"), collapse = "|"), gsub("_.*", "", colnames(species)), value = T))

species_lab <- select(species, all_of(c(grep(paste0(c(lactobacilli_names, streptococci_names, carnobacteriaceae_names, enterococcaceae_names, leuconostocaceae_names, lab_names), collapse = "|"), colnames(species), value = T), colnames(meta))))

# alpha diversity based only on LAB
species_diversity_lab <- data.frame(cbind(Shannon = vegan::diversity(select(species_lab, -all_of(colnames(meta)))), Simpson = vegan::diversity(select(species_lab, -all_of(colnames(meta))), index = "simpson")))
species_diversity_lab$Sample <- rownames(species_diversity_lab)
species_diversity_lab <- left_join(species_diversity_lab, meta)

species_diversity_lab <- species_diversity_lab %>% pivot_longer(cols = c(Shannon, Simpson))

# now excluding LAB
species_non_lab <- select(species, -all_of(c(grep(paste0(c(lactobacilli_names, streptococci_names, carnobacteriaceae_names, enterococcaceae_names, leuconostocaceae_names, lab_names), collapse = "|"), colnames(species), value = T), colnames(meta))))

# alpha diversity based only on non-LAB
species_diversity_non_lab <- data.frame(cbind(Shannon = vegan::diversity(species_non_lab), Simpson = vegan::diversity(species_non_lab, index = "simpson")))
species_diversity_non_lab$Sample <- rownames(species_diversity_non_lab)
species_diversity_non_lab <- left_join(species_diversity_non_lab, meta)

species_diversity_non_lab <- species_diversity_non_lab %>% pivot_longer(cols = c(Shannon, Simpson))

species_diversity_lab$community <- rep("LAB", nrow(species_diversity_lab))
species_diversity_non_lab$community <- rep("non-LAB", nrow(species_diversity_non_lab))

species_diversity_lab_vs_nonlab <- full_join(species_diversity_lab, species_diversity_non_lab)

species_diversity_lab_vs_nonlab$Preparation2 <- ifelse(species_diversity_lab_vs_nonlab$Preparation %in% c("Alkali treated olives", "Natural olives", "Specialties"), species_diversity_lab_vs_nonlab$Preparation, "Others")

# final plot
ggplot(filter(species_diversity_lab_vs_nonlab, Preparation == "Natural olives", Fermentation_class != "before fermentation"), aes(x = factor(Fermentation_class, levels = c("days ≤ 170", "170 < days ≤ 240", "240 < days ≤ 365", "365 < days ≤ 850")), y = value, color = Fermentation_class))+
  geom_violin(alpha = 0, size = 1.1)+
  geom_jitter(size = .75)+
  stat_summary(fun = "median", fun.min = "median", fun.max= "median", linewidth= 0.2, geom = "crossbar", color = "black")+
  scale_color_manual(values = c("days ≤ 170" = "azure4", "170 < days ≤ 240" = "#F6C54D", "240 < days ≤ 365" = "#4FAE62", "365 < days ≤ 850" = "#C02D45"))+
  scale_y_sqrt()+
  ggpubr::geom_pwc(hide.ns = T, p.adjust.method = "none", label = "p.signif")+
  facet_grid(community~name, scales = "free")+
  ggpubr::theme_pubr()+
  labs(x = "", y = "")+
  guides(color = 'none', shape = guide_legend(override.aes = list(size = 3)))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12), 
        strip.text = element_text(face = "bold", size = 12), 
        legend.text = element_text(size = 11), 
        plot.margin = unit(c(10,10,10,20), "bigpts"))


# Panel B ====
# loading the RPKM of genes linked with stress resistance in LAB species
# in the GitHub repository, the file is called "rpkm_stressresistance_annot2.txt"
rpkm_stressresistance_annot2 <- read.table("rpkm_stressresistance_annot2.txt", header = T, sep = "\t", dec = ".", quote = "")
ggplot(filter(rpkm_stressresistance_annot2, stress %in% c("pH")), aes(x = factor(Fermentation_class, levels = c("before fermentation", "days ≤ 170" ,"170 < days ≤ 240", "240 < days ≤ 365", "365 < days ≤ 850")), y = rpkm_sum, color = Fermentation_class))+
  geom_violin(alpha = 0)+
  geom_jitter(aes(shape = Preparation2))+
  stat_summary(fun = "median", fun.min = "median", fun.max= "median", linewidth= 0.2, geom = "crossbar", color = "black")+
  #scale_color_manual(values = c("days ≤ 170" = "gray", "170 < days ≤ 240" = "#F6C54D", "240 < days ≤ 365" = "#4FAE62", "365 < days ≤ 850" = "#C02D45", "before fermentation" = "black"))+
  labs(x = "", y = "RPKM", shape = "")+
  facet_wrap(~stress, scales = "free_y")+ # se non specifichi free_y, temperature > di tutti gli altri
  theme_pubr(legend = "right")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 10.5), 
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill ="gray95"),
        plot.margin = unit(c(10,10,10,20), "bigpts"))+
  ggpubr::geom_pwc(hide.ns = T, label = "p.signif")+
  facet_wrap(~stress, scales = "free_y")+
  scale_color_manual(values = c("days ≤ 170" = "gray", "170 < days ≤ 240" = "#F6C54D", "240 < days ≤ 365" = "#4FAE62", "365 < days ≤ 850" = "#C02D45", "before fermentation" = "black"))+
  guides(color = 'none', shape = guide_legend(override.aes = list(size = 3)))


# Panel C ====
# loading the table about the SGB size and taxonomic assignment
# in the GitHub repository, the file is called "METAOlive_GTBDTK_bacteria_archaea.xlsx"
meta_mags <- readxl::read_xlsx("GTDBTK/METAOlive_GTBDTK_bacteria_archaea.xlsx", sheet = 1)
meta_mags$Sample <- gsub("\\.bin.*", "", meta_mags$user_genome)
any(!(meta_mags$Sample %in% meta$Sample)) # FALSE
meta_mags <- left_join(meta_mags, meta)
meta_mags$user_genome <- gsub("fasta", "fa", meta_mags$user_genome)

# selecting the top 10 SGBs
top10_sgbs <- meta_mags %>% group_by(SGB, Preparation2, Fermentation_class) %>% count() %>% ungroup() %>% group_by(SGB) %>% mutate(overall = sum(n)) %>% arrange(desc(overall)) %>% filter(overall > 30)
tmp <- meta_mags[,c(2,22)] %>% group_by(SGB) %>% filter(row_number()==1)
tmp$classification <- gsub(".*g__", "", tmp$classification)
top10_sgbs <- left_join(top10_sgbs, tmp)
top10_sgbs$SGB <- paste0(top10_sgbs$SGB, " (n = ", top10_sgbs$overall, ")")

# plot
a <- top10_sgbs %>% filter(overall > 30) %>% ggplot(aes(x = reorder(SGB, -overall, decreasing = T), y = n, fill = Preparation2))+
  geom_col()+
  scale_fill_manual(values = c("Alkali treated olives" = "#374e55", "Natural olives" = "#df8f44", "Specialties" = "#00a1d5", "Others" = "#bf32bb"))+
  labs(y = "# of MAGs", x = "", fill = "")+
  theme_pubclean()+
  coord_flip()+
  guides(fill = guide_legend(override.aes = list(size = 9)))

b <- top10_sgbs %>% filter(overall > 30 & Preparation2 == "Natural olives" & Fermentation_class == "170 < days ≤ 240") %>% ggplot(aes(x = reorder(SGB, -overall, decreasing = T), y = 0, fill = Preparation2))+
  geom_text(aes(label = classification), hjust = 0, fontface = "italic", size = 2.75)+
  ylim(0, 0.5)+
  coord_flip()+
  theme_void()

library(ggalign)
ggalign::align_plots(a, b, guides = "t")



