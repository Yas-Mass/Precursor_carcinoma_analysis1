# Figure 4 A to D
#################

# START for Figure 4 A to D
##################################################################################
d1 <- Figure4AD.and.FigureS5.non.FOXP3

# Mann-whitney's U test (carcinoma, 4CA vs.......)

# Fig4A Eithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4 ~ d.path , data = d2)
print (test_result)


# Fig4B Epithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8 ~ d.path , data = d2)
print (test_result)


# Fig4C Lamina Propria CD3CD4
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4 ~ d.path , data = d2)
print(test_result)


# Fig4D Lamina Propria CD3CD8
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8 ~ d.path , data = d2)
print(test_result)
################################################################################
# END for Figure 4 A to D


