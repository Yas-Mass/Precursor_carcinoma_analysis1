# Supplementary Figure S8
#########################
rm(list=ls())

# upload the dataset
FigS8AtoD <- readRDS("FigS8AtoD.rds")
FigS8EtoH <- readRDS("FigS8EtoH.rds")


#START Supplementary Figure S8
################################################################################################################################################################

#for size (Supplementary Figure S8 A to D [size]: Density.epithelial.CD3CD4)
#######################################
f1<-ggboxplot(FigS8AtoD, x = "size.3cate", y = "density.epithelial.CD3CD4",
              fill = "d.size", width=0.75,
              ylab = "density", xlab = "size.3cate")+
  theme_bw()+
  scale_fill_discrete(limit = c("2classical","3serrated"))+
  scale_x_discrete(limit = c("0", "1", "2"))+
  scale_y_continuous(limits = c(0,300))+
  #scale_fill_brewer(palette="Set1")+
  scale_fill_manual(values=c("dodgerblue2","darkolivegreen2"))
ggsave(file="FigS8A.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")  

#Mann-whitney's U test 
d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "1" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "2" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "1" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "2" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

FigS8AtoD$"density.epithelial.CD3CD8" <- as.numeric(FigS8AtoD$"density.epithelial.CD3CD8")
f1<-ggboxplot(FigS8AtoD, x = "size.3cate", y = "density.epithelial.CD3CD8",
              fill = "d.size", width=0.75,
              ylab = "density", xlab = "size.3cate")+
  theme_bw()+
  scale_fill_discrete(limit = c("2classical","3serrated"))+
  scale_x_discrete(limit = c("0", "1", "2"))+
  scale_y_continuous(limits = c(0,400))+
  #scale_fill_brewer(palette="Set1")+
  scale_fill_manual(values=c("dodgerblue2","darkolivegreen2"))
ggsave(file="FigS8B.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")  

#Mann-whitney's U test 
d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "1" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "2" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "1" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "2" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ size.3cate , data = d1)
print (test_result)

FigS8AtoD$"density.laminapropria.CD3CD4" <- as.numeric(FigS8AtoD$"density.laminapropria.CD3CD4")
f1<-ggboxplot(FigS8AtoD, x = "size.3cate", y = "density.laminapropria.CD3CD4",
              fill = "d.size", width=0.75,
              ylab = "density", xlab = "size.3cate")+
  theme_bw()+
  scale_fill_discrete(limit = c("2classical","3serrated"))+
  scale_x_discrete(limit = c("0", "1", "2"))+
  scale_y_continuous(limits = c(0,6000))+
  #scale_fill_brewer(palette="Set1")+
  scale_fill_manual(values=c("dodgerblue2","darkolivegreen2"))
ggsave(file="FigS8C.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")  

#Mann-whitney's U test 
d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "1" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "2" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "1" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "2" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ size.3cate , data = d1)
print (test_result)

FigS8AtoD$"density.laminapropria.CD3CD8" <- as.numeric(FigS8AtoD$"density.laminapropria.CD3CD8")
f1<-ggboxplot(FigS8AtoD, x = "size.3cate", y = "density.laminapropria.CD3CD8",
              fill = "d.size", width=0.75,
              ylab = "density", xlab = "size.3cate")+
  theme_bw()+
  scale_fill_discrete(limit = c("2classical","3serrated"))+
  scale_x_discrete(limit = c("0", "1", "2"))+
  scale_y_continuous(limits = c(0,600))+
  #scale_fill_brewer(palette="Set1")+
  scale_fill_manual(values=c("dodgerblue2","darkolivegreen2"))
ggsave(file="FigS8D.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, path= "output")  

#Mann-whitney's U test 
d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "1" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("2classical"))

#Size.3cate "0" vs. "2" in classical pathway, non-serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "1" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "1"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ size.3cate , data = d1)
print (test_result)

d1 <- FigS8AtoD %>% filter(d.size %in% c("3serrated"))

#Size.3cate "0" vs. "2" in classical pathway, serrated lesions
d1 <- d1 %>% filter(size.3cate %in% c("0", "2"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ size.3cate , data = d1)
print (test_result)







#for age (Supplementary Figure S8 E to H [age]: Density.epithelial.CD3CD4)
######################################
#density.epithelial.CD3CD4 
f1<-ggboxplot(FigS8EtoH, x = "age.4cate2", y = "density.epithelial.CD3CD4",
              fill = "d.age", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "age.4cate2")+
  scale_y_continuous(limits = c(0, 200))+
  theme_bw()+ 
  scale_x_discrete(limit = c("0","1","2","3"))+
  scale_fill_manual(values=c( "bisque", "dodgerblue2", "darkolivegreen2"))
ggsave(file="FigS8E.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal vs, classical [non serrated])
d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.age , data = d1)
print (test_result)



#density.epithelial.CD3CD8 
f1<-ggboxplot(FigS8EtoH, x = "age.4cate2", y = "density.epithelial.CD3CD8",
              fill = "d.age", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "age.4cate2")+
  scale_y_continuous(limits = c(0, 200))+
  theme_bw()+ 
  scale_x_discrete(limit = c("0","1","2","3"))+
  scale_fill_manual(values=c( "bisque", "dodgerblue2", "darkolivegreen2"))
ggsave(file="FigS8F.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal vs, classical [non serrated])
d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.age , data = d1)
print (test_result)



#density.laminapropria.CD3CD4
FigS8EtoH$"density.laminapropria.CD3CD4" <- as.numeric(FigS8EtoH$"density.laminapropria.CD3CD4")
f1<-ggboxplot(FigS8EtoH, x = "age.4cate2", y = "density.laminapropria.CD3CD4",
              fill = "d.age", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "age.4cate2")+
  scale_y_continuous(limits = c(0, 6000))+
  theme_bw()+ 
  scale_x_discrete(limit = c("0","1","2","3"))+
  scale_fill_manual(values=c( "bisque", "dodgerblue2", "darkolivegreen2"))
ggsave(file="FigS8G.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal vs, classical [non serrated])
d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.age , data = d1)
print (test_result)



#density.laminapropria.CD3CD8
FigS8EtoH$"density.laminapropria.CD3CD8" <- as.numeric(FigS8EtoH$"density.laminapropria.CD3CD8")
f1<-ggboxplot(FigS8EtoH, x = "age.4cate2", y = "density.laminapropria.CD3CD8",
              fill = "d.age", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "age.4cate2")+
  scale_y_continuous(limits = c(0, 1000))+
  theme_bw()+ 
  scale_x_discrete(limit = c("0","1","2","3"))+
  scale_fill_manual(values=c( "bisque", "dodgerblue2", "darkolivegreen2"))
ggsave(file="FigS8H.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal vs, classical [non serrated])
d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("0"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("1"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("2"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "2non-serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)

d1 <- FigS8EtoH %>% filter(age.4cate2 %in% c("3"))

d1 <- d1 %>% filter(d.age %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.age , data = d1)
print (test_result)
#######################################################################################################################################
#END Supplementary Figure S8


