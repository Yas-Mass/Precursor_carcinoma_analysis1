df0 <- FigS7.AtoD
df1 <- FigS7.EtoH

# START Supplementary Foigure S7 
################################################################################

# START Stage analysis
# "density.epithelial.CD3CD4"
#############################
f2<-ggboxplot(df0, x = "d.stage", y = "density.epithelial.CD3CD4",
              fill = "d.stage", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 200))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.s1", "4CA.s2"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.epithelial.CD3CD4.stage.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD4")  



# "density.epithelial.CD3CD8"
#############################
f2<-ggboxplot(df0, x = "d.stage", y = "density.epithelial.CD3CD8",
              fill = "d.stage", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 250))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.s1", "4CA.s2"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.epithelial.CD3CD8.stage.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD8")  


# "density.laminapropria.CD3CD4"
################################
f2<-ggboxplot(df0, x = "d.stage", y = "density.laminapropria.CD3CD4",
              fill = "d.stage", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 4000))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.s1", "4CA.s2"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.laminapropria.CD3CD4.stage.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD4")  



# "density.laminapropria.CD3CD8"
################################
f2<-ggboxplot(df0, x = "d.stage", y = "density.laminapropria.CD3CD8",
              fill = "d.stage", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 600))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.s1", "4CA.s2"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.laminapropria.CD3CD8.stage.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD8")  
################################################################################
# Stage END



# START MSI analysis
################################################################################

# "density.epithelial.CD3CD4"
#############################
f2<-ggboxplot(df1, x = "d.msi", y = "density.epithelial.CD3CD4",
              fill = "d.msi", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.msi")+
  scale_y_continuous(limits = c(0, 200))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.mh", "4CA.nmh"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.epithelial.CD3CD4.msi.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD4")  



# "density.epithelial.CD3CD8"
#############################
f2<-ggboxplot(df1, x = "d.msi", y = "density.epithelial.CD3CD8",
              fill = "d.msi", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.msi")+
  scale_y_continuous(limits = c(0, 250))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.mh", "4CA.nmh"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.epithelial.CD3CD8.msi.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD8")  


# "density.laminapropria.CD3CD4"
################################
f2<-ggboxplot(df1, x = "d.msi", y = "density.laminapropria.CD3CD4",
              fill = "d.msi", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.msi")+
  scale_y_continuous(limits = c(0, 4000))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.mh", "4CA.nmh"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.laminapropria.CD3CD4.msi.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD4")  



# "density.laminapropria.CD3CD8"
################################
f2<-ggboxplot(df1, x = "d.msi", y = "density.laminapropria.CD3CD8",
              fill = "d.msi", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.msi")+
  scale_y_continuous(limits = c(0, 600))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA.mh", "4CA.nmh"))+
  scale_fill_manual(values=c("indianred1", "indianred1", "darkolivegreen2", "bisque", "dodgerblue2"))

#Save
ggsave(file="density.laminapropria.CD3CD8.msi.png", f2, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output//CD3CD8")  
################################################################################
# MSI END





#wilcox.test START
############################################################################################################################

# For Stage
################################################################################
N <- filter(df0, d.stage == "1normal")
classical <- filter(df0, d.stage == "2classical")
serrated <- filter(df0, d.stage == "3serrated")
CA.s1 <- filter(df0, d.stage == "4CA.s1")
CA.s2 <- filter(df0, d.stage == "4CA.s2")

wilcox.test(N$density.epithelial.CD3CD4, classical$density.epithelial.CD3CD4)
wilcox.test(N$density.epithelial.CD3CD4, serrated$density.epithelial.CD3CD4)
wilcox.test(N$density.epithelial.CD3CD4, CA.s1$density.epithelial.CD3CD4)
wilcox.test(N$density.epithelial.CD3CD4, CA.s2$density.epithelial.CD3CD4)

wilcox.test(N$density.epithelial.CD3CD8, classical$density.epithelial.CD3CD8)
wilcox.test(N$density.epithelial.CD3CD8, serrated$density.epithelial.CD3CD8)
wilcox.test(N$density.epithelial.CD3CD8, CA.s1$density.epithelial.CD3CD8)
wilcox.test(N$density.epithelial.CD3CD8, CA.s2$density.epithelial.CD3CD8)

wilcox.test(N$density.laminapropria.CD3CD4, classical$density.laminapropria.CD3CD4)
wilcox.test(N$density.laminapropria.CD3CD4, serrated$density.laminapropria.CD3CD4)
wilcox.test(N$density.laminapropria.CD3CD4, CA.s1$density.laminapropria.CD3CD4)
wilcox.test(N$density.laminapropria.CD3CD4, CA.s2$density.laminapropria.CD3CD4)

wilcox.test(N$density.laminapropria.CD3CD8, classical$density.laminapropria.CD3CD8)
wilcox.test(N$density.laminapropria.CD3CD8, serrated$density.laminapropria.CD3CD8)
wilcox.test(N$density.laminapropria.CD3CD8, CA.s1$density.laminapropria.CD3CD8)
wilcox.test(N$density.laminapropria.CD3CD8, CA.s2$density.laminapropria.CD3CD8)

wilcox.test(CA.s2$density.epithelial.CD3CD4, N$density.epithelial.CD3CD4)
wilcox.test(CA.s2$density.epithelial.CD3CD4, classical$density.epithelial.CD3CD4)
wilcox.test(CA.s2$density.epithelial.CD3CD4, serrated$density.epithelial.CD3CD4)
wilcox.test(CA.s2$density.epithelial.CD3CD4, CA.s1$density.epithelial.CD3CD4)

wilcox.test(CA.s2$density.epithelial.CD3CD8, N$density.epithelial.CD3CD8)
wilcox.test(CA.s2$density.epithelial.CD3CD8, classical$density.epithelial.CD3CD8)
wilcox.test(CA.s2$density.epithelial.CD3CD8, serrated$density.epithelial.CD3CD8)
wilcox.test(CA.s2$density.epithelial.CD3CD8, CA.s1$density.epithelial.CD3CD8)

wilcox.test(CA.s2$density.laminapropria.CD3CD4, N$density.laminapropria.CD3CD4)
wilcox.test(CA.s2$density.laminapropria.CD3CD4, classical$density.laminapropria.CD3CD4)
wilcox.test(CA.s2$density.laminapropria.CD3CD4, serrated$density.laminapropria.CD3CD4)
wilcox.test(CA.s2$density.laminapropria.CD3CD4, CA.s1$density.laminapropria.CD3CD4)

wilcox.test(CA.s2$density.laminapropria.CD3CD8, N$density.laminapropria.CD3CD8)
wilcox.test(CA.s2$density.laminapropria.CD3CD8, classical$density.laminapropria.CD3CD8)
wilcox.test(CA.s2$density.laminapropria.CD3CD8, serrated$density.laminapropria.CD3CD8)
wilcox.test(CA.s2$density.laminapropria.CD3CD8, CA.s1$density.laminapropria.CD3CD8)

#For MSI
################################################################################
N<-filter(df1, d.msi=="1normal")
classical<-filter(df1, d.msi=="2classical")
serrated<-filter(df1, d.msi=="3serrated")
CA.mh<-filter(df1, d.msi=="4CA.mh")
CA.nmh<-filter(df1, d.msi=="4CA.nmh")

wilcox.test(N$density.epithelial.CD3CD4, classical$density.epithelial.CD3CD4)
wilcox.test(N$density.epithelial.CD3CD4, serrated$density.epithelial.CD3CD4)
wilcox.test(N$density.epithelial.CD3CD4, CA.mh$density.epithelial.CD3CD4)
wilcox.test(N$density.epithelial.CD3CD4, CA.nmh$density.epithelial.CD3CD4)

wilcox.test(N$density.epithelial.CD3CD8, classical$density.epithelial.CD3CD8)
wilcox.test(N$density.epithelial.CD3CD8, serrated$density.epithelial.CD3CD8)
wilcox.test(N$density.epithelial.CD3CD8, CA.mh$density.epithelial.CD3CD8)
wilcox.test(N$density.epithelial.CD3CD8, CA.nmh$density.epithelial.CD3CD8)

wilcox.test(N$density.laminapropria.CD3CD4, classical$density.laminapropria.CD3CD4)
wilcox.test(N$density.laminapropria.CD3CD4, serrated$density.laminapropria.CD3CD4)
wilcox.test(N$density.laminapropria.CD3CD4, CA.mh$density.laminapropria.CD3CD4)
wilcox.test(N$density.laminapropria.CD3CD4, CA.nmh$density.laminapropria.CD3CD4)

wilcox.test(N$density.laminapropria.CD3CD8, classical$density.laminapropria.CD3CD8)
wilcox.test(N$density.laminapropria.CD3CD8, serrated$density.laminapropria.CD3CD8)
wilcox.test(N$density.laminapropria.CD3CD8, CA.mh$density.laminapropria.CD3CD8)
wilcox.test(N$density.laminapropria.CD3CD8, CA.nmh$density.laminapropria.CD3CD8)




wilcox.test(CA.nmh$density.epithelial.CD3CD4, N$density.epithelial.CD3CD4)
wilcox.test(CA.nmh$density.epithelial.CD3CD4, classical$density.epithelial.CD3CD4)
wilcox.test(CA.nmh$density.epithelial.CD3CD4, serrated$density.epithelial.CD3CD4)
wilcox.test(CA.nmh$density.epithelial.CD3CD4, CA.mh$density.epithelial.CD3CD4)

wilcox.test(CA.nmh$density.epithelial.CD3CD8, N$density.epithelial.CD3CD8)
wilcox.test(CA.nmh$density.epithelial.CD3CD8, classical$density.epithelial.CD3CD8)
wilcox.test(CA.nmh$density.epithelial.CD3CD8, serrated$density.epithelial.CD3CD8)
wilcox.test(CA.nmh$density.epithelial.CD3CD8, CA.mh$density.epithelial.CD3CD8)

wilcox.test(CA.nmh$density.laminapropria.CD3CD4, N$density.laminapropria.CD3CD4)
wilcox.test(CA.nmh$density.laminapropria.CD3CD4, classical$density.laminapropria.CD3CD4)
wilcox.test(CA.nmh$density.laminapropria.CD3CD4, serrated$density.laminapropria.CD3CD4)
wilcox.test(CA.nmh$density.laminapropria.CD3CD4, CA.mh$density.laminapropria.CD3CD4)

wilcox.test(CA.nmh$density.laminapropria.CD3CD8, N$density.laminapropria.CD3CD8)
wilcox.test(CA.nmh$density.laminapropria.CD3CD8, classical$density.laminapropria.CD3CD8)
wilcox.test(CA.nmh$density.laminapropria.CD3CD8, serrated$density.laminapropria.CD3CD8)
wilcox.test(CA.nmh$density.laminapropria.CD3CD8, CA.mh$density.laminapropria.CD3CD8)
############################################################################################################################
#wilcox.test END
