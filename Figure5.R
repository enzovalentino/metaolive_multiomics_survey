## Figure 5 A
# loading input table
# in the GitHub repository, the file is called 'fig5a.xlsx'
fig5atab <- readxl::read_xlsx("fig5a.xlsx")
fig5atab <- as.data.frame(fig5atab)
fig5atab$variety <- gsub("_.*", "", fig5atab$Observation)
fig5atab$variety <- gsub("ITn", "Itrana nera", fig5atab$variety)
fig5atab$variety <- gsub("OdG", "Oliva di Gaeta", fig5atab$variety)
fig5atab$Observation <- gsub(".*_", "", fig5atab$Observation)

ggplot(fig5atab, aes(x = F1, y = F2))+
  geom_point(aes(color = Observation, shape = variety), size = 4, position = position_jitter(w=10, h=3.5), alpha = .85)+
  geom_point(data = (fig5atab %>% group_by(Prior) %>% summarise(F1 = mean(F1), F2 = mean(F2))), aes(x = F1, y = F2), color = "black", fill = "black", size = 4, shape = 23)+
  scale_color_manual(values = c( "240 < days ≤ 365" = "#4FAE62", "365 < days ≤ 850" = "#C02D45"))+
  labs(x = "F1 (98.45 %)",
       y = "F2 (1.55 %)", 
       color = "", 
       linetype = "",
       shape = "")+
  theme_classic2()+
  theme(legend.position = "top", legend.box = "vertical",
        axis.title = element_text(size = 14), legend.text = element_text(size = 11))

## Figure 5 B
# loading input table
# in the GitHub repository, the file is called 'figure5b.xlsx'
fig5btab <- readxl::read_xlsx("figure5b.xlsx")
fig5btab <- as.data.frame(fig5btab)
fig5btab$Observation <- gsub("Hal", "Halkidiki", fig5btab$Observation)
fig5btab$Observation <- gsub("Bel", "Bella di Daunia", fig5btab$Observation)
fig5btab$Observation <- gsub("Nob", "Nocellara del Belice", fig5btab$Observation)



ggplot(fig5btab, aes(x = F1, y = F2))+
  geom_point(aes(color = Observation, shape = Observation), size = 4, position = position_jitter(w=150, h=3.5), alpha = .85)+
  geom_point(data = (fig5btab %>% group_by(Observation) %>% summarise(F1 = mean(F1), F2 = mean(F2))), aes(x = F1, y = F2), color = "black", fill = "black", size = 4, shape = 23)+
  scale_color_manual(values = c("Bella di Daunia" = "#fae85c", "Halkidiki" = "#c6c6c6", "Nocellara del Belice" = "#967ccc"))+
  labs(x = "F1 (99.97 %)",
       y = "F2 (0.03 %)", 
       color = "", 
       linetype = "",
       shape = "")+
  theme_classic2()+
  theme(legend.position = "top", legend.box = "vertical",
        axis.title = element_text(size = 14), legend.text = element_text(size = 11))
