rm(list=ls())

# Load R libraries
# If you did not experience the installation, please first install the same name package from R packages
library(tidyverse)
library(RColorBrewer)
library(gplots)
library(ggplot2)
library(readxl)
library(ggcorrplot)
library(corrplot)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(violinplotter)
library(spatstat)
library(ggpubr)
library(tidyverse)
library(RColorBrewer)
library(tidyr)

# upload the dataset
FigS4 <- readRDS("FigS4.rds")

#START Supplementary Figure S4
################################################################################################################################################################
f1<- FigS4 %>%  
  ggplot(aes(x=`project`, y= `density.overall.CD3`, fill=`project`)) + 
  xlab("project") +
  ylab("density.overall.CD3")+
  geom_violin()+
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 8000))+
  guides(fill="none")  

#Save
ggsave(file="FigS4.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   
##########################################################################################################################################################
#END Supplementary Figure S4

oxplot(alpha=0.7, outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 8000))+
  guides(fill="none")  

#Save
ggsave(file="FigS4.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   
##########################################################################################################################################################
#END Supplementary Figure S4

