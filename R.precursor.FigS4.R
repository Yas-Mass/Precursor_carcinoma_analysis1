Supplementary Figure S4
#######################
rm(list=ls())

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





