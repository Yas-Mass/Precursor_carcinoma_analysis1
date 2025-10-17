# Supplementary Figure S9
#########################
rm(list=ls())


# MSI analysis START
########################################################################################################################################
# upload the dataset
df1 <- FigureS9

# "density.overall.CD3CD4"
#############################
# Create output directory if it doesn't exist
dir.create("output/CD3CD4", recursive = TRUE, showWarnings = FALSE)

# Filter for location3 values 0,1,2
df_subset <- df1[df1$location3 %in% c(0, 1, 2), ]

# Recode location3 as factor with descriptive labels
df_subset$location3 <- factor(df_subset$location3,
                              levels = c(0, 1, 2),
                              labels = c("Proximal Colon", "Distal Colon", "Rectum"))

# Create boxplot faceted by location3
f2 <- ggboxplot(df_subset,
                x = "d.path.msi",
                y = "density.overall.CD3CD4",
                fill = "d.path.msi",
                width = 0.75,
                ylab = "Density of overall CD3CD4",
                xlab = "d.path.msi") +
  facet_wrap(~location3) +
  scale_y_continuous(limits = c(0, 8000)) +
  theme_bw() +
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", 
                             "3serrated.SSL", "4CA.mh", "4CA.nmh")) +
  scale_fill_manual(values = c("indianred1", "indianred1", "darkolivegreen2", 
                               "bisque", "darkgreen", "dodgerblue2")) +
  theme(
    strip.text = element_text(size = 30, family = "Arial", face = "bold"),      # facet label font
    axis.text.x = element_text(size = 30, family = "Arial", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 30, family = "Arial"),
    axis.title = element_text(size = 30, family = "Arial"),
    legend.title = element_text(size = 30, family = "Arial"),
    legend.text = element_text(size = 30, family = "Arial")
  )

# Save the plot to file
ggsave(filename = "density.overall.CD3CD4.msi.by_location3.png",
       plot = f2,
       scale = 1,
       width = 600,
       height = 200,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,
       path = "output/CD3CD4")




# "density.overall.CD3CD8"
#############################
# Create output directory if it doesn't exist
dir.create("output/CD3CD8", recursive = TRUE, showWarnings = FALSE)

# Filter for location3 values 0,1,2
df_subset <- df1[df1$location3 %in% c(0, 1, 2), ]

# Recode location3 as factor with descriptive labels
df_subset$location3 <- factor(df_subset$location3,
                              levels = c(0, 1, 2),
                              labels = c("Proximal Colon", "Distal Colon", "Rectum"))

# Create boxplot faceted by location3
f2 <- ggboxplot(df_subset,
                x = "d.path.msi",
                y = "density.overall.CD3CD8",
                fill = "d.path.msi",
                width = 0.75,
                ylab = "Density of overall CD3CD8",
                xlab = "d.path.msi") +
  facet_wrap(~location3) +
  scale_y_continuous(limits = c(0, 2000)) +
  theme_bw() +
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", 
                             "3serrated.SSL", "4CA.mh", "4CA.nmh")) +
  scale_fill_manual(values = c("indianred1", "indianred1", "darkolivegreen2", 
                               "bisque", "darkgreen", "dodgerblue2")) +
  theme(
    strip.text = element_text(size = 30, family = "Arial", face = "bold"),      # facet label font
    axis.text.x = element_text(size = 30, family = "Arial", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 30, family = "Arial"),
    axis.title = element_text(size = 30, family = "Arial"),
    legend.title = element_text(size = 30, family = "Arial"),
    legend.text = element_text(size = 30, family = "Arial")
  )

# Save the plot to file
ggsave(filename = "density.overall.CD3CD8.msi.by_location3.png",
       plot = f2,
       scale = 1,
       width = 600,
       height = 200,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,
       path = "output/CD3CD8")





# "density.overall.CD3CD4.regulatory"
#############################
# Create output directory if it doesn't exist
dir.create("output/CD3CD4", recursive = TRUE, showWarnings = FALSE)

# Filter for location3 values 0,1,2
df_subset <- df1[df1$location3 %in% c(0, 1, 2), ]

# Recode location3 as factor with descriptive labels
df_subset$location3 <- factor(df_subset$location3,
                              levels = c(0, 1, 2),
                              labels = c("Proximal Colon", "Distal Colon", "Rectum"))

# Create boxplot faceted by location3
f2 <- ggboxplot(df_subset,
                x = "d.path.msi",
                y = "density.overall.CD3CD4.regulatory",
                fill = "d.path.msi",
                width = 0.75,
                ylab = "Density of overall CD3CD4.regulatory",
                xlab = "d.path.msi") +
  facet_wrap(~location3) +
  scale_y_continuous(limits = c(0, 600)) +
  theme_bw() +
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", 
                             "3serrated.SSL", "4CA.mh", "4CA.nmh")) +
  scale_fill_manual(values = c("indianred1", "indianred1", "darkolivegreen2", 
                               "bisque", "darkgreen", "dodgerblue2")) +
  theme(
    strip.text = element_text(size = 30, family = "Arial", face = "bold"),      # facet label font
    axis.text.x = element_text(size = 30, family = "Arial", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 30, family = "Arial"),
    axis.title = element_text(size = 30, family = "Arial"),
    legend.title = element_text(size = 30, family = "Arial"),
    legend.text = element_text(size = 30, family = "Arial")
  )

# Save the plot to file
ggsave(filename = "density.overall.CD3CD4.regulatory.msi.by_location3.png",
       plot = f2,
       scale = 1,
       width = 600,
       height = 200,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,
       path = "output/CD3CD4")




# "density.overall.CD3CD8.regulatory"
#############################
# Create output directory if it doesn't exist
dir.create("output/CD3CD8", recursive = TRUE, showWarnings = FALSE)

# Filter for location3 values 0,1,2
df_subset <- df1[df1$location3 %in% c(0, 1, 2), ]

# Recode location3 as factor with descriptive labels
df_subset$location3 <- factor(df_subset$location3,
                              levels = c(0, 1, 2),
                              labels = c("Proximal Colon", "Distal Colon", "Rectum"))

# Create boxplot faceted by location3
f2 <- ggboxplot(df_subset,
                x = "d.path.msi",
                y = "density.overall.CD3CD8.regulatory",
                fill = "d.path.msi",
                width = 0.75,
                ylab = "Density of overall CD3CD8.regulatory",
                xlab = "d.path.msi") +
  facet_wrap(~location3) +
  scale_y_continuous(limits = c(0, 400)) +
  theme_bw() +
  scale_x_discrete(limit = c("1normal", "2classical", "3serrated", 
                             "3serrated.SSL", "4CA.mh", "4CA.nmh")) +
  scale_fill_manual(values = c("indianred1", "indianred1", "darkolivegreen2", 
                               "bisque", "darkgreen", "dodgerblue2")) +
  theme(
    strip.text = element_text(size = 30, family = "Arial", face = "bold"),      # facet label font
    axis.text.x = element_text(size = 30, family = "Arial", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 30, family = "Arial"),
    axis.title = element_text(size = 30, family = "Arial"),
    legend.title = element_text(size = 30, family = "Arial"),
    legend.text = element_text(size = 30, family = "Arial")
  )

# Save the plot to file
ggsave(filename = "density.overall.CD3CD8.regulatory.msi.by_location3.png",
       plot = f2,
       scale = 1,
       width = 600,
       height = 200,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,
       path = "output/CD3CD8")




#For MSI p values, proximal
#################################
# location3 == "0" 
df1_subset <- filter(df1, location3 == "0")

N <- filter(df1_subset, d.path.msi == "1normal")
classical <- filter(df1_subset, d.path.msi == "2classical")
serrated <- filter(df1_subset, d.path.msi == "3serrated")
serrated.SSL <- filter(df1_subset, d.path.msi == "3serrated.SSL")
CA.mh <- filter(df1_subset, d.path.msi == "4CA.mh")
CA.nmh <- filter(df1_subset, d.path.msi == "4CA.nmh")

wilcox.test(N$density.overall.CD3CD4, classical$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, serrated$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, serrated.SSL$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, CA.mh$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, CA.nmh$density.overall.CD3CD4)

wilcox.test(CA.nmh$density.overall.CD3CD4, N$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, classical$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, serrated$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, serrated.SSL$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, CA.mh$density.overall.CD3CD4)

wilcox.test(N$density.overall.CD3CD8, classical$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, serrated$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, serrated.SSL$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, CA.mh$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, CA.nmh$density.overall.CD3CD8)

wilcox.test(CA.nmh$density.overall.CD3CD8, N$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, classical$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, serrated$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, serrated.SSL$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, CA.mh$density.overall.CD3CD8)

wilcox.test(N$density.overall.CD3CD4.regulatory, classical$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, serrated$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, serrated.SSL$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, CA.mh$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, CA.nmh$density.overall.CD3CD4.regulatory)

wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, N$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, classical$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, serrated$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, serrated.SSL$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, CA.mh$density.overall.CD3CD4.regulatory)

wilcox.test(N$density.overall.CD3CD8.regulatory, classical$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, serrated$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, serrated.SSL$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, CA.mh$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, CA.nmh$density.overall.CD3CD8.regulatory)

wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, N$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, classical$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, serrated$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, serrated.SSL$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, CA.mh$density.overall.CD3CD8.regulatory)








# location3 == "1" 
df1_subset <- filter(df1, location3 == "1")

N <- filter(df1_subset, d.path.msi == "1normal")
classical <- filter(df1_subset, d.path.msi == "2classical")
serrated <- filter(df1_subset, d.path.msi == "3serrated")
serrated.SSL <- filter(df1_subset, d.path.msi == "3serrated.SSL")
CA.mh <- filter(df1_subset, d.path.msi == "4CA.mh")
CA.nmh <- filter(df1_subset, d.path.msi == "4CA.nmh")

wilcox.test(N$density.overall.CD3CD4, classical$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, serrated$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, serrated.SSL$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, CA.mh$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, CA.nmh$density.overall.CD3CD4)

wilcox.test(CA.nmh$density.overall.CD3CD4, N$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, classical$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, serrated$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, serrated.SSL$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, CA.mh$density.overall.CD3CD4)

wilcox.test(N$density.overall.CD3CD8, classical$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, serrated$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, serrated.SSL$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, CA.mh$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, CA.nmh$density.overall.CD3CD8)

wilcox.test(CA.nmh$density.overall.CD3CD8, N$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, classical$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, serrated$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, serrated.SSL$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, CA.mh$density.overall.CD3CD8)

wilcox.test(N$density.overall.CD3CD4.regulatory, classical$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, serrated$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, serrated.SSL$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, CA.mh$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, CA.nmh$density.overall.CD3CD4.regulatory)

wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, N$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, classical$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, serrated$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, serrated.SSL$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, CA.mh$density.overall.CD3CD4.regulatory)

wilcox.test(N$density.overall.CD3CD8.regulatory, classical$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, serrated$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, serrated.SSL$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, CA.mh$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, CA.nmh$density.overall.CD3CD8.regulatory)

wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, N$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, classical$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, serrated$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, serrated.SSL$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, CA.mh$density.overall.CD3CD8.regulatory)



# location3 == "2" 
df1_subset <- filter(df1, location3 == "2")

N <- filter(df1_subset, d.path.msi == "1normal")
classical <- filter(df1_subset, d.path.msi == "2classical")
serrated <- filter(df1_subset, d.path.msi == "3serrated")
serrated.SSL <- filter(df1_subset, d.path.msi == "3serrated.SSL")
CA.mh <- filter(df1_subset, d.path.msi == "4CA.mh")
CA.nmh <- filter(df1_subset, d.path.msi == "4CA.nmh")

wilcox.test(N$density.overall.CD3CD4, classical$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, serrated$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, CA.mh$density.overall.CD3CD4)
wilcox.test(N$density.overall.CD3CD4, CA.nmh$density.overall.CD3CD4)

wilcox.test(CA.nmh$density.overall.CD3CD4, N$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, classical$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, serrated$density.overall.CD3CD4)
wilcox.test(CA.nmh$density.overall.CD3CD4, CA.mh$density.overall.CD3CD4)

wilcox.test(N$density.overall.CD3CD8, classical$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, serrated$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, CA.mh$density.overall.CD3CD8)
wilcox.test(N$density.overall.CD3CD8, CA.nmh$density.overall.CD3CD8)

wilcox.test(CA.nmh$density.overall.CD3CD8, N$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, classical$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, serrated$density.overall.CD3CD8)
wilcox.test(CA.nmh$density.overall.CD3CD8, CA.mh$density.overall.CD3CD8)

wilcox.test(N$density.overall.CD3CD4.regulatory, classical$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, serrated$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, CA.mh$density.overall.CD3CD4.regulatory)
wilcox.test(N$density.overall.CD3CD4.regulatory, CA.nmh$density.overall.CD3CD4.regulatory)

wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, N$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, classical$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, serrated$density.overall.CD3CD4.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD4.regulatory, CA.mh$density.overall.CD3CD4.regulatory)

wilcox.test(N$density.overall.CD3CD8.regulatory, classical$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, serrated$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, CA.mh$density.overall.CD3CD8.regulatory)
wilcox.test(N$density.overall.CD3CD8.regulatory, CA.nmh$density.overall.CD3CD8.regulatory)

wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, N$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, classical$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, serrated$density.overall.CD3CD8.regulatory)
wilcox.test(CA.nmh$density.overall.CD3CD8.regulatory, CA.mh$density.overall.CD3CD8.regulatory)
################################################################################
# MSI END
# END Supplementary Figure S9


