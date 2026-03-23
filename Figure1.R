# loading metadata
# in the GitHub repository, the file is called "metadata.txt"
meta <- read.table("metadata.txt", header = T, sep = "\t", dec = ".", quote = "")

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

# panel A ====
a <- ggplot(meta %>% filter(Preparation %in% c("Natural olives", "Alkali treated olives", "Specialties")), aes(x = Preparation, y = `Fermentation (days)`, color = Preparation))+
  geom_violin(alpha = 0)+
  geom_jitter(aes(shape = Variety3))+
  ggsci::scale_color_jama()+
  ggpubr::geom_pwc()+
  labs(x = "")+
  ggpubr::theme_classic2()+
  theme(axis.text.x = element_text(size = 13, angle = 30, hjust = 1, vjust = 1), axis.text.y = element_text(size = 11), axis.title.y = element_text(angle = 0, vjust = .5,  size = 13))+
  guides(color = 'none', shape = 'none')

b <- ggplot(meta %>% filter(Preparation %in% c("Natural olives", "Alkali treated olives", "Specialties")), aes(x = Preparation, y = pH, color = Preparation))+
  geom_violin(alpha = 0)+
  geom_jitter(aes(shape = Variety3))+
  ggsci::scale_color_jama()+
  ggpubr::geom_pwc()+labs(x = "", shape = '')+
  ggpubr::theme_classic2()+
  theme(axis.text.x = element_text(size = 13, angle = 30, hjust = 1, vjust = 1), axis.text.y = element_text(size = 11), axis.title.y = element_text(angle = 0, vjust = .5,  size = 13))+
  guides(color = 'none')

c <- ggplot(meta %>% filter(Preparation %in% c("Natural olives", "Alkali treated olives", "Specialties")), aes(x = Preparation, y = g_L_lactic_acid, color = Preparation))+
  geom_violin(alpha = 0)+
  geom_jitter(aes(shape = Variety3))+
  ggsci::scale_color_jama()+
  ggpubr::geom_pwc()+labs(x = "", shape = '')+
  ggpubr::theme_classic2()+
  theme(axis.text.x = element_text(size = 13, angle = 30, hjust = 1, vjust = 1), axis.text.y = element_text(size = 11), axis.title.y = element_text(angle = 0, vjust = .5,  size = 13))+
  guides(color = 'none', shape = 'none')

a+c+b


# panel B ====
# loading MetaPhlAn4 species-level relative abundances
# in the GitHub repository, the file is called "species.xlsx"
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


tmp <- species
tmp$var <- paste0(tmp$Preparation, " - ", tmp$Fermentation_class)
tmp <- tmp %>% filter(!(var %in% c("Natural olives - before fermentation", "Dehydrated olives - before fermentation", "Alkali treated olives - before fermentation", "Alkali treated olives - 240 < days ≤ 365", "Olives trated with lactate - 170 < days ≤ 240", "Olives darkened by oxidation - 170 < days ≤ 240")))

# plotting species showing relative abundance > 0.1% in at least 20% of samples
namess <- c()
for (i in names_species) {
  if(sum(species[,i] > .1) >= 0.2*nrow(species)) {
    namess <- c(namess, i)
  }
}
namess <- namess[-c(14, 7, 16)]

tmp_20perc <- tmp %>% group_by(var) %>% select(all_of(namess)) %>% summarise_all(median)
tmp_20perc <- pivot_longer(tmp_20perc, cols = colnames(tmp_20perc)[-1])
tmp_20perc$var2 <- gsub(" - .*", "", tmp_20perc$var)
tmp_20perc$var3 <- gsub(".* - ", "", tmp_20perc$var)
samplesize <- as.data.frame(table(tmp$var))
colnames(samplesize)[1] <- "var"
tmp_20perc <- left_join(tmp_20perc, samplesize)
tmp_20perc$var <- paste0(tmp_20perc$var, " (n = ", tmp_20perc$Freq, ")")

library(hrbrthemes)
library(viridis)
ggplot(tmp_20perc, aes(x = factor(var, levels = c("Natural olives - days ≤ 170 (n = 22)", "Natural olives - 170 < days ≤ 240 (n = 45)", "Natural olives - 240 < days ≤ 365 (n = 64)", "Natural olives - 365 < days ≤ 850 (n = 55)", "Alkali treated olives - days ≤ 170 (n = 26)", "Alkali treated olives - 170 < days ≤ 240 (n = 22)", "Alkali treated olives - 365 < days ≤ 850 (n = 3)", "Specialties - before fermentation (n = 11)", "Specialties - days ≤ 170 (n = 7)", "Specialties - 170 < days ≤ 240 (n = 6)")), y = name, fill = value))+
  geom_tile(color = "black")+
  geom_text(aes(label = paste0(round(value, 1), " %")), alpha = ifelse(tmp_20perc$value < 0.1, 0, 1), size = 3)+
  labs(x = "", y = "", fill = "median\nrelative\nabundance")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  scale_fill_gradient(high = "#d67d54", low = "#ffffff")+
  scale_x_discrete(label = function(x) {gsub(".* - ", "", x)})+
  #scale_x_discrete(labels = paste0(ll$var3," (n = ", ll$Freq, ")"))+
  facet_grid(~var2, scales = "free", axis.labels = "all_x")

# panel C ====
viral_contigs <- readxl::read_xlsx("selected_viral_contig_info.xlsx", sheet = 1) # in the GitHub repository, this file is "selected_viral_contig_info.xlsx"
colnames(viral_contigs)[2] <- "Sample"

viral_contigs$taxonomy2 <- gsub("^(([^ ]+ ){2}).*$", "\\1", viral_contigs$taxonomy)
viral_contigs$taxonomy2 <- gsub(" \\(.*", "", viral_contigs$taxonomy2)

viral_cont_subs_byline <- filter(viral_contigs, taxonomy2 %in% viral_cont_subs$taxonomy2)

host_prediction <- read.table("virome/Host_prediction_to_genome_m90.csv", header = T, sep = ",", dec =".")
host_prediction <- host_prediction %>% group_by(Virus) %>% arrange(desc(Confidence.score)) %>% filter(row_number()==1) 
# host prediction was performed only on the representative for each cluster, therefore I need to add this info

host_prediction <- host_prediction %>% rename(Contigs = Virus)
host_prediction <- left_join(host_prediction, select(viral_contigs, all_of(c("Contigs", "Cluster"))))
viral_cont_subs_byline <- left_join(viral_cont_subs_byline, host_prediction, by = "Cluster") # host associated: column "Host.taxonomy"

# most of the "Host.taxonomy" (those not NAs) coincide with "taxonomy", so I can use the latter column

# for each of the species I want to check the % of contigs predicted to be lytic or lysogenic, to understand how this can shape the microbiome succession over the fermentation
# viral_cont_subs_byline <- viral_cont_subs_byline %>% rename(Sample = SampleID)
viral_cont_subs_byline <- left_join(viral_cont_subs_byline, meta)

viral_cont_subs_plot1 <- viral_cont_subs_byline %>% group_by(taxonomy2) %>% summarise(lytic_perc = sum(Vibrant_type == "lytic")/n()*100, lysogenic_perc = sum(Vibrant_type == "lysogenic")/n()*100, NA_perc = sum(Vibrant_type == "NA")/n()*100, overall_number = n())
viral_cont_subs_plot2 <- viral_cont_subs_byline %>% group_by(taxonomy2, Fermentation_class, Preparation2) %>% summarise(rpkm = sum(RPKM))

viral_cont_subs_plot1 <- viral_cont_subs_plot1 %>% pivot_longer(cols = c(lytic_perc, lysogenic_perc, NA_perc))
viral_cont_subs_plot1$name <- gsub("_perc", "", viral_cont_subs_plot1$name)


plot1_vir <- viral_cont_subs_plot1 %>% mutate(taxonomy2 = fct_reorder(taxonomy2, overall_number)) %>% filter(taxonomy2 != "Homo sapiens ") %>% ggplot(aes(x = name, y = taxonomy2, size = value, color = name))+
  geom_point()+
  geom_text(aes(label = paste0(round(value, digits = 1), " %")), size = 2.75, color = "black", position = position_nudge(y = 0.35))+
  theme_classic2()+
  labs(x = "", y = "", size = "% of viral contigs", color = "Vibrant label")+
  scale_size(range = c(-1, 5))+
  scale_color_bmj()+
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 30), 
        axis.text.y = element_text(face = "italic"))+
  guides(color = guide_none())

fctrs <- (viral_cont_subs_plot1 %>% mutate(taxonomy2 = fct_reorder(taxonomy2, overall_number)))$taxonomy2
plot2_vir <- viral_cont_subs_plot2 %>% filter(taxonomy2 != "Homo sapiens ") %>% ggplot(aes(x = factor(Fermentation_class, levels = c("before fermentation", "days ≤ 170", "170 < days ≤ 240", "240 < days ≤ 365", "365 < days ≤ 850")), y = factor(taxonomy2, levels = levels(fctrs)), fill = rpkm))+
  geom_tile()+
  labs(x = "", y = "", fill = "RPKM")+
  theme_classic2()+
  scale_fill_gradientn(colours = pal_jco()(2))+
  facet_wrap(~Preparation2, scales = "free_x", nrow = 1)+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 90))


align_plots(plot1_vir, plot2_vir, guides = "r")
