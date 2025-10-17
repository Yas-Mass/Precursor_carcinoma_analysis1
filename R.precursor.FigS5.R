# Supplementary Figure S5
#########################
rm(list=ls())

# upload the dataset
d1 <- FigureS5.FOXP3levels
colnames (d1)

# STEP 1 
# Load FigS5 for FOXP3 high and low #########################################################################################

# epithelial.CD3CD4.regulatory.low 
###################################
f1<-ggboxplot(d1, x = "d.path", y = "density.epithelial.CD3CD4.regulatory.low",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 15))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA"))+
  scale_fill_manual(values=c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file="FigS5.epithelial.CD3CD4.regulatory.low.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)



#Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.low ~ d.path , data = d2)
print (test_result)

# epithelial.CD3CD4.regulatory.high 
###################################
f1<-ggboxplot(d1, x = "d.path", y = "density.epithelial.CD3CD4.regulatory.high",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 15))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA"))+
  scale_fill_manual(values=c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file="FigS5.epithelial.CD3CD4.regulatory.high.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)


#Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory.high ~ d.path , data = d2)
print (test_result)

# epithelial.CD3CD8.regulatory.low 
###################################
f1<-ggboxplot(d1, x = "d.path", y = "density.epithelial.CD3CD8.regulatory.low",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 15))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA"))+
  scale_fill_manual(values=c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file="FigS5.epithelial.CD3CD8.regulatory.low.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)


#Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.low ~ d.path , data = d2)
print (test_result)

# epithelial.CD3CD8.regulatory.high 
###################################
f1<-ggboxplot(d1, x = "d.path", y = "density.epithelial.CD3CD8.regulatory.high",
              fill = "d.path", width=0.75, #lwd=0.5,
              ylab = "density", xlab = "d.stage")+
  scale_y_continuous(limits = c(0, 15))+theme_bw()+ scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA"))+
  scale_fill_manual(values=c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file="FigS5.epithelial.CD3CD8.regulatory.high.png", f1, scale = 1, width = 450, height = 160, units = "mm",dpi = 300, limitsize = TRUE, bg='transparent', path= "output")  

#Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)


#Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory.high ~ d.path , data = d2)
print (test_result)

# laminapropria.CD3CD4.regulatory.low 
###################################
f1 <- ggboxplot(d1, x = "d.path", y = "density.laminapropria.CD3CD4.regulatory.low",
                fill = "d.path", width=0.75, 
                ylab = "density", xlab = "d.stage") +
  scale_y_continuous(limits = c(0, 200)) + theme_bw() + 
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA")) +
  scale_fill_manual(values = c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file = "FigS5.laminapropria.CD3CD4.regulatory.low.png", f1, scale = 1, width = 450, height = 160, units = "mm", dpi = 300, limitsize = TRUE, bg = 'transparent', path = "output")  

# Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)


# Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.low ~ d.path, data = d2)
print(test_result)

# laminapropria.CD3CD4.regulatory.high 
###################################
f1 <- ggboxplot(d1, x = "d.path", y = "density.laminapropria.CD3CD4.regulatory.high",
                fill = "d.path", width=0.75,
                ylab = "density", xlab = "d.stage") +
  scale_y_continuous(limits = c(0, 200)) + theme_bw() + 
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA")) +
  scale_fill_manual(values = c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file = "FigS5.laminapropria.CD3CD4.regulatory.high.png", f1, scale = 1, width = 450, height = 160, units = "mm", dpi = 300, limitsize = TRUE, bg = 'transparent', path = "output")  

# Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)


# Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory.high ~ d.path, data = d2)
print(test_result)

# laminapropria.CD3CD8.regulatory.low 
###################################
f1 <- ggboxplot(d1, x = "d.path", y = "density.laminapropria.CD3CD8.regulatory.low",
                fill = "d.path", width=0.75,
                ylab = "density", xlab = "d.stage") +
  scale_y_continuous(limits = c(0, 40)) + theme_bw() + 
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA")) +
  scale_fill_manual(values = c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file = "FigS5.laminapropria.CD3CD8.regulatory.low.png", f1, scale = 1, width = 450, height = 160, units = "mm", dpi = 300, limitsize = TRUE, bg = 'transparent', path = "output")  

# Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)


# Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.low ~ d.path, data = d2)
print(test_result)

# laminapropria.CD3CD8.regulatory.high 
###################################
f1 <- ggboxplot(d1, x = "d.path", y = "density.laminapropria.CD3CD8.regulatory.high",
                fill = "d.path", width=0.75,
                ylab = "density", xlab = "d.stage") +
  scale_y_continuous(limits = c(0, 40)) + theme_bw() + 
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", "4CA", "5NinCA")) +
  scale_fill_manual(values = c("indianred1", "lightgoldenrod2", "bisque", "dodgerblue2", "darkolivegreen2"))

ggsave(file = "FigS5.laminapropria.CD3CD8.regulatory.high.png", f1, scale = 1, width = 450, height = 160, units = "mm", dpi = 300, limitsize = TRUE, bg = 'transparent', path = "output")  

# Mann-whitney's U test (normal, 1normal vs.......)
d2 <- d1 %>% filter(d.path %in% c("1normal", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "4CA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("1normal", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)


# Mann-whitney's U test (carcinoma, 4CA vs.......)
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory.high ~ d.path, data = d2)
print(test_result)


vars_to_summarize <- c(
  "density.epithelial.CD3CD4.regulatory",
  "density.epithelial.CD3CD4.regulatory.low",
  "density.epithelial.CD3CD4.regulatory.high",
  "density.epithelial.CD3CD8.regulatory",
  "density.epithelial.CD3CD8.regulatory.low",
  "density.epithelial.CD3CD8.regulatory.high",
  "density.laminapropria.CD3CD4.regulatory",
  "density.laminapropria.CD3CD4.regulatory.low",
  "density.laminapropria.CD3CD4.regulatory.high",
  "density.laminapropria.CD3CD8.regulatory",
  "density.laminapropria.CD3CD8.regulatory.low",
  "density.laminapropria.CD3CD8.regulatory.high"
)

summary_stats_multi <- d1 %>%
  select(d.path, all_of(vars_to_summarize)) %>%
  pivot_longer(
    cols = all_of(vars_to_summarize),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(d.path, variable) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    se = sd / sqrt(n),
    CI_lower = mean - qt(0.975, df = n - 1) * se,
    CI_upper = mean + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  ) %>%
  select(-sd, -se)

write_xlsx(summary_stats_multi, path = "summary_stats_multiple_vars.xlsx")
print("Excelた: summary_stats_multiple_vars.xlsx")

# Load FigS5 for FOXP3 high and low ####################################################################################
# STEP 1 END 







# STEP2 START
########################################################################################################################

# START for Figure S5 Naive
##################################################################################
d1 <- Figure4AD.and.FigureS5.non.FOXP3

# Mann-whitney's U test (carcinoma, 4CA vs.......)

# Eithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD4.naive ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.naive ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.naive ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.naive ~ d.path , data = d2)
print (test_result)


# Epithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD8.naive ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8.naive ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8.naive ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.naive ~ d.path , data = d2)
print (test_result)


# Lamina Propria CD3CD4
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.naive ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.naive ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.naive ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.naive ~ d.path , data = d2)
print(test_result)


# Lamina Propria CD3CD8
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.naive ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.naive ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.naive ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.naive ~ d.path , data = d2)
print(test_result)
################################################################################
# END for FigureS5 Naive







# START for Figure S5 Memory
##################################################################################
d1 <- Figure4AD.and.FigureS5.non.FOXP3

# Mann-whitney's U test (carcinoma, 4CA vs.......)

# Eithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD4.memory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.memory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.memory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.memory ~ d.path , data = d2)
print (test_result)


# Epithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD8.memory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8.memory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8.memory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.memory ~ d.path , data = d2)
print (test_result)


# Lamina Propria CD3CD4
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.memory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.memory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.memory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.memory ~ d.path , data = d2)
print(test_result)


# Lamina Propria CD3CD8
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.memory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.memory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.memory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.memory ~ d.path , data = d2)
print(test_result)
################################################################################
# END for Figure S5 Memory





# START for Figure S5 Regulatory
##################################################################################
d1 <- Figure4AD.and.FigureS5.non.FOXP3

# Mann-whitney's U test (carcinoma, 4CA vs.......)

# Eithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD4.regulatory ~ d.path , data = d2)
print (test_result)


# Epithelial
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory ~ d.path , data = d2)
print (test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.epithelial.CD3CD8.regulatory ~ d.path , data = d2)
print (test_result)


# Lamina Propria CD3CD4
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD4.regulatory ~ d.path , data = d2)
print(test_result)


# Lamina Propria CD3CD8
d2 <- d1 %>% filter(d.path %in% c("4CA", "1normal"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "2classical"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "3serrated"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory ~ d.path , data = d2)
print(test_result)

d2 <- d1 %>% filter(d.path %in% c("4CA", "5NinCA"))
test_result <- wilcox.test(density.laminapropria.CD3CD8.regulatory ~ d.path , data = d2)
print(test_result)
################################################################################
# END for Figure S5 Regulatory



