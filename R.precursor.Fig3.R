# Figure 3
##########################
rm(list=ls())

# upload the dataset
Fig3BtoG.MannUtest <- readRDS("Fig3BtoG.MannUtest.rds")
Fig3BtoG <- readRDS("Fig3BtoG.rds")
Fig3H <- readRDS("Fig3H.rds")


#START Figure 3
################################################################################################################################################################

#Figure 3B to G: T-cell subset densities in the intraepithelial and stromal regions
################################################################################
#Figure 3B for TA, Tubular adenoma
d1<-filter(Fig3BtoG, Diagnosis=="TA")
f1<-ggplot(d1, aes(x=Tcell, y=Density, fill=Region))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 2000))+
  theme_bw()
ggsave(file="Fig3B.TA.grouped.boxplot.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path = "output")  

#Mann-whitney's U test
#CD3CD4 Epithelial vs Stroma for TA, tubular adenoma
d2<-filter(Fig3BtoG.MannUtest, Diagnosis=="TA")

#CD3CD4 Epithelial vs Stroma
d3<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD4", "Stroma_CD3CD4"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d3, exact = FALSE)
w1
#CD3CD8 Epithelial vs Stroma
d4<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD8", "Stroma_CD3CD8"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d4, exact = FALSE)
w1
#CD3DN Epithelial vs Stroma
d5<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3DN", "Stroma_CD3DN"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d5, exact = FALSE)
w1



#Figure 3C for TVA, Tubular villous adenoma
d1<-filter(Fig3BtoG, Diagnosis=="TVA")
f1<-ggplot(d1, aes(x=Tcell, y=Density, fill=Region))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 2000))+
  theme_bw()
ggsave(file="Fig3C.TVA.grouped.boxplot.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path = "output")  

#Mann-whitney's U test
#CD3CD4 Epithelial vs Stroma for TVA
d2<-filter(Fig3BtoG.MannUtest, Diagnosis=="TVA")

#CD3CD4 Epithelial vs Stroma
d3<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD4", "Stroma_CD3CD4"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d3, exact = FALSE)
w1
#CD3CD8 Epithelial vs Stroma
d4<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD8", "Stroma_CD3CD8"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d4, exact = FALSE)
w1
#CD3DN Epithelial vs Stroma
d5<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3DN", "Stroma_CD3DN"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d5, exact = FALSE)
w1



#Figure 3D for VA, Villous adenoma
d1<-filter(Fig3BtoG, Diagnosis=="VA")
f1<-ggplot(d1, aes(x=Tcell, y=Density, fill=Region))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 2000))+
  theme_bw()
ggsave(file="Fig3D.VA.grouped.boxplot.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path = "output")  

#Mann-whitney's U test
#CD3CD4 Epithelial vs Stroma for VA
d2<-filter(Fig3BtoG.MannUtest, Diagnosis=="VA")

#CD3CD4 Epithelial vs Stroma
d3<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD4", "Stroma_CD3CD4"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d3, exact = FALSE)
w1
#CD3CD8 Epithelial vs Stroma
d4<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD8", "Stroma_CD3CD8"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d4, exact = FALSE)
w1
#CD3DN Epithelial vs Stroma
d5<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3DN", "Stroma_CD3DN"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d5, exact = FALSE)
w1


#Figure 3E for HP, Hyperplastic polyp
d1<-filter(Fig3BtoG, Diagnosis=="HP")
f1<-ggplot(d1, aes(x=Tcell, y=Density, fill=Region))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 2000))+
  theme_bw()
ggsave(file="Fig3E.HP.grouped.boxplot.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path = "output")  

#Mann-whitney's U test
#CD3CD4 Epithelial vs Stroma for HP
d2<-filter(Fig3BtoG.MannUtest, Diagnosis=="HP")

#CD3CD4 Epithelial vs Stroma
d3<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD4", "Stroma_CD3CD4"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d3, exact = FALSE)
w1
#CD3CD8 Epithelial vs Stroma
d4<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD8", "Stroma_CD3CD8"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d4, exact = FALSE)
w1
#CD3DN Epithelial vs Stroma
d5<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3DN", "Stroma_CD3DN"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d5, exact = FALSE)
w1



#Figure 3E for SSL
d1<-filter(Fig3BtoG, Diagnosis=="SSL")
f1<-ggplot(d1, aes(x=Tcell, y=Density, fill=Region))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 2000))+
  theme_bw()
ggsave(file="Fig3F.SSL.grouped.boxplot.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path = "output")  

#Mann-whitney's U test
#CD3CD4 Epithelial vs Stroma for SSL
d2<-filter(Fig3BtoG.MannUtest, Diagnosis=="SSL")

#CD3CD4 Epithelial vs Stroma
d3<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD4", "Stroma_CD3CD4"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d3, exact = FALSE)
w1
#CD3CD8 Epithelial vs Stroma
d4<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD8", "Stroma_CD3CD8"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d4, exact = FALSE)
w1
#CD3DN Epithelial vs Stroma
d5<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3DN", "Stroma_CD3DN"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d5, exact = FALSE)
w1



#Figure 3G for TSA
d1<-filter(Fig3BtoG, Diagnosis=="TSA")
f1<-ggplot(d1, aes(x=Tcell, y=Density, fill=Region))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 2000))+
  theme_bw()
ggsave(file="Fig3G.TSA.grouped.boxplot.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path = "output")  

#Mann-whitney's U test
#CD3CD4 Epithelial vs Stroma for TSA
d2<-filter(Fig3BtoG.MannUtest, Diagnosis=="TSA")

#CD3CD4 Epithelial vs Stroma
d3<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD4", "Stroma_CD3CD4"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d3, exact = FALSE)
w1
#CD3CD8 Epithelial vs Stroma
d4<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3CD8", "Stroma_CD3CD8"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d4, exact = FALSE)
w1
#CD3DN Epithelial vs Stroma
d5<-d2 %>% filter(Tcell.cat1 %in% c("Epithelial_CD3DN", "Stroma_CD3DN"))
w1 <- wilcox.test(Density ~ Tcell.cat1, data = d5, exact = FALSE)
w1


#Figure 3H: Correlation map
################################################################################
#Calculate spearman correlations
corr <- cor(Fig3H, method= "spearman", use="complete.obs")
#Draw correlogram 
ggcorrplot(corr, method=c("square"), type= "lower", lab= "TRUE", lab_size= 6, tl.srt=90, colors=c("blue2", "white", "red1", title="Correlations between overall immune cells"))
#Save 
ggsave("Fig3H.png", plot = last_plot(), device = "png", path = "output", scale = 1, width = 400, height = 300, units = "mm", dpi = 400, limitsize = TRUE)
################################################################################################################################################################
#END Figure 3

#Save 
ggsave("Fig3H.png", plot = last_plot(), device = "png", path = "output", scale = 1, width = 400, height = 300, units = "mm", dpi = 400, limitsize = TRUE)
################################################################################################################################################################
#END Figure 3


