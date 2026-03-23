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
