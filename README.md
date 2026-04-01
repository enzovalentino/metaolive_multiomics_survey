# METAOlive multiomics survey
This repository reports the datasets and the commands used for making statistical analysis and plots for the paper (Currently Under Review..)

## Plots
### Figure 1
Figure 1 of the paper is made of three panels: 
1. **panel a** reports a violin plot of the length of fermentation (in days) of table olives, the concentration of lactic acid (expressed as g/L) and the pH. In the violins, samples are color-coded according to the process, while their shape denotes the olive variety.
2. **panel b** reports the median relative abundance of the top 15 species in groups of samples splitted by treatment and fermentation class.
3. **panel c** reports the proportion of virus-associated contigs predicted as lytic or lysogenic, as well as their average RPKM abundance in samples splitted by fermentation class.

**Figure 1** can be reproduced with the script *Figure1.R* 


### Figure 2
Figure 2 of the paper is made of three panels:
1. **panel a** statistically compares alpha diversity indices between naturally fermented table olives, splitted by fermentation class. The upper plot presents alpha diversity indices computed only on LAB species abundances.
2. **panel b** compares the RPKM abundance of genes encoded by LAB and linked with resistance to pH.
3. **panel c** is a stacked bar plot of the top 10 Species-level Genome Bins detected.

**Figure 2** can be reproduced with the script *Figure2.R*

### Figure 3
Figure 3 of the paper is made of three panels: 
1. **panels a and c** report Principal Coordinate Analyses of Natural and Alkali-treated olives, respectively. The PCoAs are based on the Bray-Curtis dissimilarity computed on the species-level composition. Samples are color-coded by variety, whereas the shape denotes the fermentation group.
2. **panels b and d** report the p-values of the pairwise Permutational MANOVA, computed between each pair of varieties. 

**Figure 3** can be reproduced with the script *Figure3.R*.

### Figure 4
Figure 4 of the paper is made of 4 panels: 
1. **panel a** is a pie chart indicating how the different volatile organic compounds distribute across categories (e.g., acids, alcohols, esters, etc.).
2. **panel b** is a Principal Components Analysis (PCA) based on VOCs classes concentrations where points are color-coded according to the fermentation stage.
3. **panel c** is a hierarchical clustering based on the Euclidean distance computed on volatile compounds intensities, with samples grouped by fermentation stage.
4. **panel d** is hierarchical clustering based on the Euclidean distance computed on volatile compounds intensities, with samples grouped by processing method.

**Figure 4** can be reproduced with the script *Figure4.R*.

### Figure 5
Figure 5 of the paper shows the results of Linear Discriminant Analyses (LDA) performed on selected olive cultivars. It is divided into 3 panels: 
1. **panel a** shows the results of the LDA performed on Oliva di Gaeta and Itrana nera samples, across two fermentation stages.
2. **panel b** shows the results of the LDA performed on Bella di Daunia, Nocellara del Belice and Halkidiki olive samples (green olives varieties).
3. **panel c** shows a hierarchical clustering based on the Euclidean distance computed on intensities of VOC compounds in Bella di Daunia, Nocellara del Belice, and Halkidiki olives.

**Figure 5** can be reproduced with the script *Figure5.R*.

### Figure 6
Figure 6 shows the potential of olives microbiome to produce VOC compounds, as well as correlations between relative abundance of taxa and intensities of VOC compounds. It is divided into three panels:
1. **panel a** is a correlation plot showing significant correlation between the abundance of VOCs (pink nodes) and species taxonomy (green nodes).
2. **panel b** is a bubble plot showing, for each species, the # of significant correlations with specific VOCs classes.
3. **panel c** is a hierarchical clustering based on the Minkowski dissimilarity computed on the # of genes from each metagenome matching to genes involved in flavour formation. In addition, the taxonomic assignment of matches to each functional category is reported as proportions.

**Figure 6** can be reproduced with the script *Figure6.R*.

### Figure 7
Figure 7 reports the geographical origin of olive samples along with their distribution across varieties. 

**Figure 7** can be reproduced with the script *Figure7.R*.
