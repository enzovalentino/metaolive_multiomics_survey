# loading metadata
# in the GitHub repository, the file is called "metadata.txt"
meta <- read.table("metadata.txt", header = T, sep = "\t", dec = ".", quote = "")

# loading libraries
if(!require(ggflags)){
    install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))
    library(ggflags)
}

if(!require(maps)){
    install.packages("maps")
    library(maps)
}

if(!require(mapproj)){
    install.packages("mapproj")
    library(mapproj)
}

if(!require(patchwork)){
    install.packages("patchwork")
    library(patchwork)
}

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}

# Panel A ====
contr_countries <- as.data.frame(cbind(meta$Sample, meta$Group, meta$geo_loc_name, meta$Variety))
for (i in 1:nrow(contr_countries)) {
  if (is.na(contr_countries[i,"V3"]) && (contr_countries[i,"V4"] %in% c("Oliva di Gaeta", "Termite di Bitetto", "Bella di Daunia", "Olive verdi di Paternò", "Leccino", "Nocellara del Belice", "Peranzana"))) {
    contr_countries[i,"V3"] <- "Italy"
  } else if (is.na(contr_countries[i,"V3"]) && (contr_countries[i,"V4"] %in% c("Halkidiki"))) {
    contr_countries[i,"V3"] <- "Greece"
  }
}

contr_countries$V3 <- gsub('^.*?\\:', '', contr_countries$V3)
contr_countries$V3 <- gsub(':.*', '', contr_countries$V3)
countries <- unique(contr_countries$V3) # per countries da colorare

world_coordinates <- ggplot2::map_data("world")

centers <- world_coordinates %>% filter(region %in% countries) %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))


a <- ggplot()+
  geom_map(data = world_coordinates, map = world_coordinates, 
                  aes(x = long, y = lat, map_id = region), 
                  #alpha = ifelse(world_coordinates$region %in% countries, 1, .25),
                  fill = ifelse(world_coordinates$region %in% countries, "#8dc645", "#f8ddaf"),
                  color = "black", 
                  linewidth = .1
)+
  geom_segment(data = centers, aes(xend = long, yend = lat, x = -30, y = lat), linewidth = .25)+
  theme_void()+
  #theme(panel.background = element_rect(fill = "#1b75bc"))+
  coord_quickmap(xlim = c(-10, 40), ylim = c(20, 50))

# n. campioni per ogni paese, con bandierina
tmp <- as.data.frame(table(contr_countries$V3))
colnames(tmp) <- c("region", "Frequency")
tmp$flags <- c("cy", "eg", "gr", "it", "es")

library(ggflags)
b <- ggplot(tmp, aes(x = 1, y = factor(region, levels = rev(c("Italy", "Spain", "Greece", "Cyprus", "Egypt"))), country = flags, size = Frequency)) +
  geom_flag(size = 15) +
  geom_text(aes(x = 1.05, y = region, label = paste0("n = ", Frequency)),  size = 5)+
  scale_country()+
  theme_void()+
  guides(color = 'none', country = 'none', size = 'none')
b+a

# Panel B ====
# radar plot
data <- meta %>% select(all_of(c("Type", "Preparation2", "Variety"))) %>% group_by(Type, Preparation2, Variety) %>% count() %>% ungroup()
data = data %>% arrange(Type, n)
colnames(data) <- c("group", "Preparation2", "individual", "value")
data$group <- as.factor(data$group)
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+5, label=paste0(individual, " (n = ", value, ")"), hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(.75,0,0.2), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
