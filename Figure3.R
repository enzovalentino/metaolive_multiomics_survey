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
## to complete ##
