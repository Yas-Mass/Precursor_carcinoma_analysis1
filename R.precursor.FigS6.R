rm(list=ls())

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
FigS6 <- readRDS("FigS6.rds")


#START Supplementary Figure S6
################################################################################################################################################################

#Load FigS6 for FigS6A to S6C####################################################
f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.epithelial.CD3CD4.naive",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,15))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6A.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD4.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD4.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD4.naive ~ d.dysplasia , data = d1)
print (test_result)

f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.epithelial.CD3CD4.memory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,12.5))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6B.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD4.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD4.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD4.memory ~ d.dysplasia , data = d1)
print (test_result)


f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.epithelial.CD3CD4.regulatory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,10))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6C.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory ~ d.dysplasia , data = d1)
print (test_result)


#Load FigS6 for FigS6D to S6F####################################################
f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.epithelial.CD3CD8.naive",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,30))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6D.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD8.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD8.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD8.naive ~ d.dysplasia , data = d1)
print (test_result)

f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.epithelial.CD3CD8.memory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,30))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6E.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD8.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD8.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD8.memory ~ d.dysplasia , data = d1)
print (test_result)


f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.epithelial.CD3CD8.regulatory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,30))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6F.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory ~ d.dysplasia , data = d1)
print (test_result)


#Load FigS6 for FigS6G to S6I####################################################
FigS6$"density.laminapropria.CD3CD4.naive" <- as.numeric(FigS6$"density.laminapropria.CD3CD4.naive")
f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.laminapropria.CD3CD4.naive",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,800))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6G.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD4.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.naive ~ d.dysplasia , data = d1)
print (test_result)

f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.laminapropria.CD3CD4.memory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,2000))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6H.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD4.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.memory ~ d.dysplasia , data = d1)
print (test_result)


f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.laminapropria.CD3CD4.regulatory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,400))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6I.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory ~ d.dysplasia , data = d1)
print (test_result)



#Load FigS6 for FigS6J to S6L####################################################
FigS6$"density.laminapropria.CD3CD8.naive" <- as.numeric(FigS6$"density.laminapropria.CD3CD8.naive")
f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.laminapropria.CD3CD8.naive",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,400))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6J.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD8.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.naive ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.naive ~ d.dysplasia , data = d1)
print (test_result)

f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.laminapropria.CD3CD8.memory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,300))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6K.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD8.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.memory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.memory ~ d.dysplasia , data = d1)
print (test_result)


f1<-ggboxplot(FigS6, x = "d.dysplasia", y = "density.laminapropria.CD3CD8.regulatory",
              fill = "d.dysplasia", width=0.75, #bxp.errorbar = TRUE,
              ylab = "density", xlab = "d.dysplasia")+
  theme_bw()+
  #scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limit = c("1normal","2lgd","3hgd","4carcinoma"))+
  scale_y_continuous(limits = c(0,60))+
  scale_fill_manual(values=c("indianred1","bisque","darkolivegreen2","dodgerblue2"))

#Save
ggsave(file="FigS6L.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")   

#Mann-whitney's U test (normal [N] vs, dysplasia and carcinoma) # 1normal, 2lgd(LGD), 3hgd(HGD), 4carcinoma
d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "2lgd")) 
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "3hgd"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory ~ d.dysplasia , data = d1)
print (test_result)

d1 <- FigS6 %>% filter(d.dysplasia %in% c("1normal", "4carcinoma"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory ~ d.dysplasia , data = d1)
print (test_result)
##########################################################################################################################################################
#END Supplementary Figure S6

