# Figure 5
##########


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
Fig5AtoB <- readRDS("Fig5AtoB.rds")
tumorlevel_spatialdata.rds <- readRDS("tumorlevel_spatialdata.rds") # Place the "tumorlevel_spatialdata.rds" in the dataset file (new file)


#START Figure 5
##########################################################################################################################################################
#Fig 5A for density.epithelial.CD3CD4.MKI67.Negative
Fig5AtoB$"density.laminapropria.CD3CD4.proliferating" <- as.numeric(Fig5AtoB$"density.laminapropria.CD3CD4.proliferating")
Fig5AtoB$"density.laminapropria.CD3CD8.proliferating" <- as.numeric(Fig5AtoB$"density.laminapropria.CD3CD8.proliferating")

#Fig5A
f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.epithelial.CD3CD4.nonproliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 250))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5A.1.epithelial.CD3CD4.nonproliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.epithelial.CD3CD8.nonproliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 250))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5A.2.epithelial.CD3CD8.nonproliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  



f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.laminapropria.CD3CD4.nonproliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 4000))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5A.3.laminapropria.CD3CD4.nonproliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.laminapropria.CD3CD8.nonproliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 4000))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5A.4.laminapropria.CD3CD8.nonproliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  



#Fig5B
f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.epithelial.CD3CD4.proliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 40))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5B.1.epithelial.CD3CD4.proliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.epithelial.CD3CD8.proliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 40))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5B.2.epithelial.CD3CD8.proliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  



f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.laminapropria.CD3CD4.proliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 100))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5B.3.laminapropria.CD3CD4.proliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

f1<-ggboxplot(Fig5AtoB, x = "d.path", y = "density.laminapropria.CD3CD8.proliferating",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 100))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4ca"))+
  scale_fill_manual(values=c("indianred1","darkolivegreen2", "bisque",  "dodgerblue2"))
ggsave(file="Fig5B.4.laminapropria.CD3CD8.proliferating.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal vs, classical [non serrated])
d1 <- Fig5AtoB %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD8.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD4.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD8.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD4.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD8.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD4.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD8.proliferating ~ d.path , data = d1)
print (test_result)

d1 <- Fig5AtoB %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD8.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD4.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD8.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD4.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD8.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD4.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD8.proliferating ~ d.path , data = d1)
print (test_result)

d1 <- Fig5AtoB %>% filter(d.path %in% c("1normal", "4ca"))
test_result <- wilcox.test(density.epithelial.CD3CD4.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD8.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD4.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD8.nonproliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD4.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.epithelial.CD3CD8.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD4.proliferating ~ d.path , data = d1)
print (test_result)
test_result <- wilcox.test(density.laminapropria.CD3CD8.proliferating ~ d.path , data = d1)
print (test_result)

#Fig5F-H
################################################################################################################################################################
library(tidyverse)
library(readxl)
library(ggplot2)
#Remove all files
rm(list=ls())

#Open tumorlevel_spatialdata and modify it to draw spatial boxplots
#Open file
tumorlevel_spatialdata <- readRDS("dataset//tumorlevel_spatialdata.rds")
#Select columns
df = subset(tumorlevel_spatialdata, select = -c(n_MSI_ID))
#Remove old file
rm(tumorlevel_spatialdata)



# STEP1 START
################################################################################################################################################################
# Rename and select colunms
tumorlevel_spatialdata <- df %>% gather(key = measurement, value=value, -TMA, -new.diagnosis)

# Create two separate tables for gcross calculated without AUC and with AUC

# Without AUC
tumorlevel_spatial_gcross_singlevalues <- tumorlevel_spatialdata %>% filter(measurement %in% c("cd3cd4mki67n_epithelial_km10", "cd3cd4mki67n_epithelial_km20",
                                                                                               "cd3cd4mki67n_epithelial_km30", "epithelial_cd3cd4mki67n_km10", 
                                                                                               "epithelial_cd3cd4mki67n_km20", "epithelial_cd3cd4mki67n_km30",
                                                                                               "cd3cd4mki67n_epithelial_nnd", "epithelial_cd3cd4mki67n_nnd", 
                                                                                               "cd3cd8mki67n_epithelial_km10", "cd3cd8mki67n_epithelial_km20", 
                                                                                               "cd3cd8mki67n_epithelial_km30", "epithelial_cd3cd8mki67n_km10", 
                                                                                               "epithelial_cd3cd8mki67n_km20","epithelial_cd3cd8mki67n_km30", 
                                                                                               "cd3cd8mki67n_epithelial_nnd", "epithelial_cd3cd8mki67n_nnd", 
                                                                                               "cd3dnmki67n_epithelial_km10", "cd3dnmki67n_epithelial_km20",
                                                                                               "cd3dnmki67n_epithelial_km30", "epithelial_cd3dnmki67n_km10",
                                                                                               "epithelial_cd3dnmki67n_km20", "epithelial_cd3dnmki67n_km30", 
                                                                                               "cd3dnmki67n_epithelial_nnd", "epithelial_cd3dnmki67n_nnd",
                                                                                               
                                                                                               "cd3cd4mki67p_epithelial_km10", "cd3cd4mki67p_epithelial_km20",
                                                                                               "cd3cd4mki67p_epithelial_km30", "epithelial_cd3cd4mki67p_km10", 
                                                                                               "epithelial_cd3cd4mki67p_km20", "epithelial_cd3cd4mki67p_km30",
                                                                                               "cd3cd4mki67p_epithelial_nnd", "epithelial_cd3cd4mki67p_nnd", 
                                                                                               "cd3cd8mki67p_epithelial_km10", "cd3cd8mki67p_epithelial_km20", 
                                                                                               "cd3cd8mki67p_epithelial_km30", "epithelial_cd3cd8mki67p_km10", 
                                                                                               "epithelial_cd3cd8mki67p_km20","epithelial_cd3cd8mki67p_km30", 
                                                                                               "cd3cd8mki67p_epithelial_nnd", "epithelial_cd3cd8mki67p_nnd", 
                                                                                               "cd3dnmki67p_epithelial_km10", "cd3dnmki67p_epithelial_km20",
                                                                                               "cd3dnmki67p_epithelial_km30", "epithelial_cd3dnmki67p_km10",
                                                                                               "epithelial_cd3dnmki67p_km20", "epithelial_cd3dnmki67p_km30", 
                                                                                               "cd3dnmki67p_epithelial_nnd", "epithelial_cd3dnmki67p_nnd",
                                                                                               "epithelial_cd3cd4distancebetweencd3cd4mki67pcd3cd4mki67n_nnd",
                                                                                               "epithelial_cd3cd8distancebetweencd3cd8mki67pcd3cd8mki67n_nnd"))

# Edit table
tumorlevel_spatial_gcross_singlevalues <- tumorlevel_spatial_gcross_singlevalues%>%
  separate(measurement, into = c("measurement_from", "measurement_to", "distance"), sep = "_")
tumorlevel_spatial_gcross_singlevalues$distance <- gsub("km", "", tumorlevel_spatial_gcross_singlevalues$distance)

# Save
saveRDS(tumorlevel_spatial_gcross_singlevalues, file="dataset//tumorlevel_spatial_gcross_singlevalues.rds")

# Focusing MSI status and Staging
tumorlevel_spatial_gcross_singlevalues <- tumorlevel_spatial_gcross_singlevalues %>%
  mutate(diagnosis.MSI=ifelse(`new.diagnosis` == "CAInMh", "CAnMh",
                              ifelse(`new.diagnosis` == "CAIMh", "CAMh", 
                                     ifelse(`new.diagnosis` == "CAnInMh", "CAnMh",
                                            ifelse(`new.diagnosis` == "CAnIMh", "CAMh", 
                                                   ifelse(`new.diagnosis` == "CAIxMh", "CAxMh",
                                                          ifelse(`new.diagnosis` == "CAnIxMh", "CAxMh", 
                                                                 ifelse(`new.diagnosis` == "NinCAInMh", "NinCAnMh",
                                                                        ifelse(`new.diagnosis` == "NinCAIMh", "NinCAMh", 
                                                                               ifelse(`new.diagnosis` == "NinCAnInMh", "NinCAnMh",
                                                                                      ifelse(`new.diagnosis` == "NinCAnIMh", "NinCAMh", 
                                                                                             ifelse(`new.diagnosis` == "NinCAIxMh", "NinCAxMh",
                                                                                                    ifelse(`new.diagnosis` == "NinCAnIxMh", "NinCAxMh",new.diagnosis)))))))))))))

tumorlevel_spatial_gcross_singlevalues <- tumorlevel_spatial_gcross_singlevalues %>%
  mutate(diagnosis.Stage=ifelse(`new.diagnosis` == "CAInMh", "CAI",
                                ifelse(`new.diagnosis` == "CAIMh", "CAI", 
                                       ifelse(`new.diagnosis` == "CAnInMh", "CAnI",
                                              ifelse(`new.diagnosis` == "CAnIMh", "CAnI", 
                                                     ifelse(`new.diagnosis` == "CAIxMh", "CAI",
                                                            ifelse(`new.diagnosis` == "CAnIxMh", "CAnI", 
                                                                   ifelse(`new.diagnosis` == "NinCAInMh", "NinCAI",
                                                                          ifelse(`new.diagnosis` == "NinCAIMh", "NinCAI", 
                                                                                 ifelse(`new.diagnosis` == "NinCAnInMh", "NinCAnI",
                                                                                        ifelse(`new.diagnosis` == "NinCAnIMh", "NinCAnI", 
                                                                                               ifelse(`new.diagnosis` == "NinCAIxMh", "NinCAI",
                                                                                                      ifelse(`new.diagnosis` == "NinCAnIxMh", "NinCAnI",new.diagnosis)))))))))))))

tumorlevel_spatial_gcross_singlevalues <- tumorlevel_spatial_gcross_singlevalues %>%
  mutate(diagnosis.Path=ifelse(`new.diagnosis` == "CAInMh", "CA",
                               ifelse(`new.diagnosis` == "CAIMh", "CA", 
                                      ifelse(`new.diagnosis` == "CAnInMh", "CA",
                                             ifelse(`new.diagnosis` == "CAnIMh", "CA", 
                                                    ifelse(`new.diagnosis` == "CAIxMh", "CA",
                                                           ifelse(`new.diagnosis` == "CAnIxMh", "CA", 
                                                                  ifelse(`new.diagnosis` == "NinCAInMh", "NinCA",
                                                                         ifelse(`new.diagnosis` == "NinCAIMh", "NinCA", 
                                                                                ifelse(`new.diagnosis` == "NinCAnInMh", "NinCA",
                                                                                       ifelse(`new.diagnosis` == "NinCAnIMh", "NinCA", 
                                                                                              ifelse(`new.diagnosis` == "NinCAIxMh", "NinCA",
                                                                                                     ifelse(`new.diagnosis` == "NinCAnIxMh", "NinCA",
                                                                                                            ifelse(`new.diagnosis` == "TA", "Classical", 
                                                                                                                   ifelse(`new.diagnosis` == "TVA", "Classical",
                                                                                                                          ifelse(`new.diagnosis` == "VA", "Classical", 
                                                                                                                                 ifelse(`new.diagnosis` == "HP", "Serrated",
                                                                                                                                        ifelse(`new.diagnosis` == "SSL", "Serrated", 
                                                                                                                                               ifelse(`new.diagnosis` == "SSLD", "Serrated",
                                                                                                                                                      ifelse(`new.diagnosis` == "TSA", "Serrated",new.diagnosis))))))))))))))))))))

# Focusing MSI status 
tumorlevel_spatial_gcross_singlevalues <- filter(tumorlevel_spatial_gcross_singlevalues, !is.na(new.diagnosis))
tumorlevel_spatial_gcross_singlevalues <- filter(tumorlevel_spatial_gcross_singlevalues, !is.na(diagnosis.MSI))
tumorlevel_spatial_gcross_singlevalues <- filter(tumorlevel_spatial_gcross_singlevalues, !is.na(diagnosis.Stage))
tumorlevel_spatial_gcross_singlevalues <- filter(tumorlevel_spatial_gcross_singlevalues, !is.na(diagnosis.Path))
################################################################################################################################################################
# STEP1 END


# STEP2 START Difference across tissue categories -> Fig5G and H
################################################################################################################################################################
# from Tumor to mki67 negative T cell 
dodge <- position_dodge(width=0.5)
tumorlevel_spatial_gcross_singlevalues$diagnosis.Path <- factor(tumorlevel_spatial_gcross_singlevalues$diagnosis.Path, levels=c("N", "Classical", "Serrated", "CA", "NinCA")) 

dodge <- position_dodge(width=0.5)
tumorlevel_spatial_gcross_singlevalues$diagnosis.Path <- factor(tumorlevel_spatial_gcross_singlevalues$diagnosis.Path, levels=c("N", "Classical", "Serrated", "CA")) 
tumorlevel_spatial_gcross_singlevalues%>%
  filter (measurement_to %in% c("cd3cd4mki67n", "cd3cd8mki67n") & measurement_from %in% c("epithelial")) %>%
  filter(distance=="nnd", measurement_to %in% c("cd3cd4mki67n", "cd3cd8mki67n"), measurement_from %in% c("epithelial")) %>%
  filter(diagnosis.Path %in% c("N", "Classical", "Serrated", "CA")) %>%
  ggplot(aes(x=diagnosis.Path, y=value, fill= factor(measurement_to)))+
  geom_boxplot(width = 0.8, alpha=0.8, outlier.shape = NA, position = position_dodge(0.9))+
  labs(title= "Immune cell type", x= "To CD3CD4 nd CD3CD8 mki67 negative distance from epithelial cell", y="NND")+
  #scale_colour_gradientn(colours = terrain.colors(10), name="diagnosis.Path")+
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=360))+
  scale_y_continuous(limits = c(0, 500))+
  scale_fill_manual(values=c("cyan3", "indianred1"))

#Save
ggsave("NND_difference between CD3CD4 and CD3CD8.mki negative_Path.Fig5G.png", plot = last_plot(), device = "png", path = "output//spatial images//box plots",
       scale = 1, width = 200, height = 100, units = "mm",
       dpi = 300, limitsize = TRUE)



# from Tumor to mki67 positive T cell 
dodge <- position_dodge(width=0.5)
tumorlevel_spatial_gcross_singlevalues$diagnosis.Path <- factor(tumorlevel_spatial_gcross_singlevalues$diagnosis.Path, levels=c("N", "Classical", "Serrated", "CA", "NinCA")) 

dodge <- position_dodge(width=0.5)
tumorlevel_spatial_gcross_singlevalues$diagnosis.Path <- factor(tumorlevel_spatial_gcross_singlevalues$diagnosis.Path, levels=c("N", "Classical", "Serrated", "CA")) 
tumorlevel_spatial_gcross_singlevalues%>%
  filter (measurement_to %in% c("cd3cd4mki67p", "cd3cd8mki67p") & measurement_from %in% c("epithelial")) %>%
  filter(distance=="nnd", measurement_to %in% c("cd3cd4mki67p", "cd3cd8mki67p"), measurement_from %in% c("epithelial")) %>%
  filter(diagnosis.Path %in% c("N", "Classical", "Serrated", "CA")) %>%
  ggplot(aes(x=diagnosis.Path, y=value, fill= factor(measurement_to)))+
  geom_boxplot(width = 0.8, alpha=0.8, outlier.shape = NA, position = position_dodge(0.9))+
  labs(title= "Immune cell type", x= "To CD3CD4 nd CD3CD8 mki67 positive distance from epithelial cell", y="NND")+
  #scale_colour_gradientn(colours = terrain.colors(10), name="diagnosis.Path")+
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=360))+
  scale_y_continuous(limits = c(0, 500))+
  scale_fill_manual(values=c("cyan3", "indianred1"))

#Save
ggsave("NND_difference between CD3CD4 and CD3CD8.mki positive_Path.Fig5H.png", plot = last_plot(), device = "png", path = "output//spatial images//box plots",
       scale = 1, width = 200, height = 100, units = "mm",
       dpi = 300, limitsize = TRUE)


# Wilcoxon
################################################################################
w1 <- tumorlevel_spatial_gcross_singlevalues %>% 
  filter(distance=="nnd", 
         measurement_to %in% c("cd3cd4mki67n", "cd3cd4mki67p", "cd3cd8mki67n", "cd3cd8mki67p"), 
         measurement_from %in% c("epithelial")) %>%
  filter(diagnosis.Path %in% c("N", "Classical", "Serrated", "CA"))


# CD3CD4 mki67 negative vs. CD3CD8 mki67 negative -> Fig5G
################################################################################

# Normal
w2 <- w1 %>% filter(diagnosis.Path=="N", 
                    measurement_to %in% c("cd3cd4mki67n", "cd3cd8mki67n"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res

# Classical
w2 <- w1 %>% filter(diagnosis.Path=="Classical", 
                    measurement_to %in% c("cd3cd4mki67n", "cd3cd8mki67n"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res

# Serrated
w2 <- w1 %>% filter(diagnosis.Path=="Serrated", 
                    measurement_to %in% c("cd3cd4mki67n", "cd3cd8mki67n"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res

# CA
w2 <- w1 %>% filter(diagnosis.Path=="CA", 
                    measurement_to %in% c("cd3cd4mki67n", "cd3cd8mki67n"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res


# CD3CD4 mki67 positive vs. CD3CD8 mki67 positive -> Fig5H
################################################################################

# Normal
w2 <- w1 %>% filter(diagnosis.Path=="N", 
                    measurement_to %in% c("cd3cd4mki67p", "cd3cd8mki67p"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res

# Classical
w2 <- w1 %>% filter(diagnosis.Path=="Classical", 
                    measurement_to %in% c("cd3cd4mki67p", "cd3cd8mki67p"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res

# Serrated
w2 <- w1 %>% filter(diagnosis.Path=="Serrated", 
                    measurement_to %in% c("cd3cd4mki67p", "cd3cd8mki67p"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res

# CA
w2 <- w1 %>% filter(diagnosis.Path=="CA", 
                    measurement_to %in% c("cd3cd4mki67p", "cd3cd8mki67p"), 
                    measurement_from %in% c("epithelial")) 
res <- wilcox.test(value ~ measurement_to, data = w2, alternative = "two.sided")
res

################################################################################################################################################################
# STEP2 END






# STEP3 START for Fig5F
################################################################################################################################################################
tumorlevel_spatial_gcross_singlevalues <- tumorlevel_spatial_gcross_singlevalues %>%
  mutate(measurement_to.new=ifelse(`measurement_to` ==  "cd3cd4mki67p", "cd3mki67p",
                                   ifelse(`measurement_to` == "cd3cd8mki67p", "cd3mki67p",
                                          ifelse(`measurement_to` == "cd3cd4mki67n", "cd3mki67n",
                                                 ifelse(`measurement_to` == "cd3cd8mki67n", "cd3mki67n", `measurement_to`)))))

# from Tumor to mki67 positive T cell 
dodge <- position_dodge(width=0.5)
tumorlevel_spatial_gcross_singlevalues$diagnosis.Path <- factor(tumorlevel_spatial_gcross_singlevalues$diagnosis.Path, levels=c("N", "Classical", "Serrated", "CA", "NinCA")) 

dodge <- position_dodge(width=0.5)
tumorlevel_spatial_gcross_singlevalues$diagnosis.Path <- factor(tumorlevel_spatial_gcross_singlevalues$diagnosis.Path, levels=c("N", "Classical", "Serrated", "CA")) 
tumorlevel_spatial_gcross_singlevalues%>%
  filter (measurement_to.new %in% c("cd3mki67n", "cd3mki67p") & measurement_from %in% c("epithelial")) %>%
  filter(distance=="nnd", measurement_to.new %in% c("cd3mki67n", "cd3mki67p"), measurement_from %in% c("epithelial")) %>%
  filter(diagnosis.Path %in% c("N", "Classical", "Serrated", "CA")) %>%
  ggplot(aes(x=diagnosis.Path, y=value, fill= factor(measurement_to.new)))+
  geom_boxplot(width = 0.8, alpha=0.8, outlier.shape = NA, position = position_dodge(0.9))+
  labs(title= "Immune cell type", x= "To CD3 mki67 positive distance from epithelial cell", y="NND")+
  #scale_colour_gradientn(colours = terrain.colors(10), name="diagnosis.Path")+
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=360))+
  scale_y_continuous(limits = c(0, 500))+
  scale_fill_manual(values=c("cyan3", "indianred1"))

#Save
ggsave("NND_difference between CD3.mki negative and positive_Path.Fig5F.png", plot = last_plot(), device = "png", path = "output//spatial images//box plots",
       scale = 1, width = 200, height = 100, units = "mm",
       dpi = 300, limitsize = TRUE)

# Wilcoxon
################################################################################
w1 <- tumorlevel_spatial_gcross_singlevalues %>% 
  filter(distance=="nnd", 
         measurement_to.new %in% c("cd3mki67n", "cd3mki67p"), 
         measurement_from %in% c("epithelial")) %>%
  filter(diagnosis.Path %in% c("N", "Classical", "Serrated", "CA"))


# CD3 mki67 negative vs. CD3 mki67 positive, Fig5F
################################################################################
# Normal
w2 <- w1 %>% filter(diagnosis.Path=="N", 
                    measurement_to.new %in% c("cd3mki67n", "cd3mki67p"), 
                    measurement_from %in% c("epithelial")) 

stats <- w2 %>%
  group_by(measurement_to.new) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    IQR_value = IQR(value, na.rm = TRUE),
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE)
  )

print(stats)

res <- wilcox.test(value ~ measurement_to.new, data = w2, alternative = "two.sided")
res

# Non-serrated
w2 <- w1 %>% filter(diagnosis.Path=="Classical", 
                    measurement_to.new %in% c("cd3mki67n", "cd3mki67p"), 
                    measurement_from %in% c("epithelial")) 

stats <- w2 %>%
  group_by(measurement_to.new) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    IQR_value = IQR(value, na.rm = TRUE),
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE)
  )

print(stats)

res <- wilcox.test(value ~ measurement_to.new, data = w2, alternative = "two.sided")
res

# Serrated
w2 <- w1 %>% filter(diagnosis.Path=="Serrated", 
                    measurement_to.new %in% c("cd3mki67n", "cd3mki67p"), 
                    measurement_from %in% c("epithelial")) 

stats <- w2 %>%
  group_by(measurement_to.new) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    IQR_value = IQR(value, na.rm = TRUE),
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE)
  )

print(stats)


res <- wilcox.test(value ~ measurement_to.new, data = w2, alternative = "two.sided")
res

# CA
w2 <- w1 %>% filter(diagnosis.Path=="CA", 
                    measurement_to.new %in% c("cd3mki67n", "cd3mki67p"), 
                    measurement_from %in% c("epithelial")) 

stats <- w2 %>%
  group_by(measurement_to.new) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    IQR_value = IQR(value, na.rm = TRUE),
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE)
  )

print(stats)

res <- wilcox.test(value ~ measurement_to.new, data = w2, alternative = "two.sided")
res
##########################################################################################################################################################
# STEP3 END 

# Figure 5 END
