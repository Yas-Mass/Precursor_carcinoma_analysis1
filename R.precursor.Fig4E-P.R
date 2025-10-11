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
Fig4.1 <- readRDS("Fig4.1.rds")
Fig4.2 <- readRDS("Fig4.2.rds")

#START Figure 4
################################################################################################################################################################

#Load Fig4.1 for Fig4E to 4H####################################################
f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.epithelial.CD3CD4",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 200))+theme_bw()+ scale_x_discrete(limit = c("N", "TA", "TVA", "VA"))+
  scale_fill_manual(values=c("bisque" ,"lightskyblue1", "deepskyblue2", "blue"))


#Save
ggsave(file="Fig4E.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, classical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TA"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TVA"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "VA"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.epithelial.CD3CD8",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 250))+theme_bw()+ scale_x_discrete(limit = c("N", "TA", "TVA", "VA"))+
  scale_fill_manual(values=c("bisque" ,"lightskyblue1", "deepskyblue2", "blue"))


#Save
ggsave(file="Fig4F.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, claasical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TA"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TVA"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "VA"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.laminapropria.CD3CD4",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 4000))+theme_bw()+ scale_x_discrete(limit = c("N", "TA", "TVA", "VA"))+
  scale_fill_manual(values=c("bisque" ,"lightskyblue1", "deepskyblue2", "blue"))


#Save
ggsave(file="Fig4G.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, claasical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TVA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "VA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.laminapropria.CD3CD8",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 600))+theme_bw()+ scale_x_discrete(limit = c("N", "TA", "TVA", "VA"))+
  scale_fill_manual(values=c("bisque" ,"lightskyblue1", "deepskyblue2", "blue"))


#Save
ggsave(file="Fig4H.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, claasical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TVA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "VA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ diagnosis , data = d1)
print (test_result)



#Load Fig4.1 for Fig4I to 4L####################################################
f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.epithelial.CD3CD4",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 200))+theme_bw()+ scale_x_discrete(limit = c("N", "HP", "SSL", "TSA"))+
  scale_fill_manual(values=c("darkolivegreen2", "bisque", "green3", "forestgreen"))
ggsave(file="Fig4I.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, classical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "HP"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "SSL"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TSA"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.epithelial.CD3CD8",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 250))+theme_bw()+ scale_x_discrete(limit = c("N", "HP", "SSL", "TSA"))+
  scale_fill_manual(values=c("darkolivegreen2", "bisque", "green3", "forestgreen"))


#Save
ggsave(file="Fig4J.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, claasical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "HP"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "SSL"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TSA"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.laminapropria.CD3CD4",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 4000))+theme_bw()+ scale_x_discrete(limit = c("N", "HP", "SSL", "TSA"))+
  scale_fill_manual(values=c("darkolivegreen2", "bisque", "green3", "forestgreen"))


#Save
ggsave(file="Fig4K.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, claasical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "HP"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "SSL"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TSA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ diagnosis , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.1, x = "diagnosis", y = "density.laminapropria.CD3CD8",
              fill = "diagnosis", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "diagnosis")+
  scale_y_continuous(limits = c(0, 600))+theme_bw()+ scale_x_discrete(limit = c("N", "HP", "SSL", "TSA"))+
  scale_fill_manual(values=c("darkolivegreen2", "bisque", "green3", "forestgreen"))


#Save
ggsave(file="Fig4L.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal [N] vs, precursors, non-serrated, claasical pathway)
d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "HP"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "SSL"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ diagnosis , data = d1)
print (test_result)

d1 <- Fig4.1 %>% filter(diagnosis %in% c("N", "TSA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ diagnosis , data = d1)
print (test_result)



#Load Fig4.2 for Fig4M to 4P####################################################
f1<-ggboxplot(Fig4.2, x = "d.dysplasia", y = "density.epithelial.CD3CD4",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,200))+
  scale_fill_manual(values=c("indianred1","bisque", "darkolivegreen2", "dodgerblue2"))
ggsave(file="Fig4M.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.dysplasia , data = d1)
print (test_result)


f1<-ggboxplot(Fig4.2, x = "d.dysplasia", y = "density.epithelial.CD3CD8",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,150))+
  scale_fill_manual(values=c("indianred1","bisque", "darkolivegreen2", "dodgerblue2"))

#Save
ggsave(file="Fig4N.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.dysplasia , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.2, x = "d.dysplasia", y = "density.laminapropria.CD3CD4",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,4000))+
  scale_fill_manual(values=c("indianred1","bisque", "darkolivegreen2", "dodgerblue2"))

#Save
ggsave(file="Fig4O.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.dysplasia , data = d1)
print (test_result)

f1<-ggboxplot(Fig4.2, x = "d.dysplasia", y = "density.laminapropria.CD3CD8",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,800))+
  scale_fill_manual(values=c("indianred1","bisque", "darkolivegreen2", "dodgerblue2"))

#Save
ggsave(file="Fig4P.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.dysplasia , data = d1)
print (test_result)

d1 <- Fig4.2 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.dysplasia , data = d1)
print (test_result)
##########################################################################################################################################################
#END Figure 4