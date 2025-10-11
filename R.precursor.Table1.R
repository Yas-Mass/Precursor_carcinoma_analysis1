d1 <- Table1

# density.epithelial.CD3CD4 START
######################################################################################################################
d1_sub <- d1 %>%
  filter(d.stage %in% c("1normal", "2classical", "3serrated"),
         !is.na(density.epithelial.CD3CD4)) %>%
  mutate(d.stage = factor(d.stage, levels = c("1normal", "2classical", "3serrated")))

summary_stats <- d1_sub %>%
  group_by(d.stage) %>%
  summarise(
    n = n(),
    median = median(density.epithelial.CD3CD4, na.rm = TRUE),
    IQR_lower = quantile(density.epithelial.CD3CD4, 0.25, na.rm = TRUE),
    IQR_upper = quantile(density.epithelial.CD3CD4, 0.75, na.rm = TRUE),
    min = min(density.epithelial.CD3CD4, na.rm = TRUE),
    max = max(density.epithelial.CD3CD4, na.rm = TRUE),
    mean = mean(density.epithelial.CD3CD4, na.rm = TRUE)
  )
print(summary_stats)

d1_sub2 <- d1 %>%
  filter(
    d.stage %in% c("1normal","2classical","3serrated"),
    !is.na(density.epithelial.CD3CD4)
  ) %>%
  mutate(
    d.stage        = factor(d.stage, levels = c("1normal","2classical","3serrated")),
    
    location3_fac  = fct_explicit_na(factor(location3),  na_level = "Missing"),
    dysplasia2_fac = fct_explicit_na(factor(dysplasia2), na_level = "Missing"),
    age4_fac       = fct_explicit_na(factor(age4),       na_level = "Missing"),
    size3_fac      = fct_explicit_na(factor(size3),      na_level = "Missing"),
    gender2_fac    = fct_explicit_na(factor(gender2),    na_level = "Missing")
  ) %>%
  mutate(
    location3_fac  = fct_relevel(location3_fac,  "Missing"),
    dysplasia2_fac = fct_relevel(dysplasia2_fac, "Missing"),
    age4_fac       = fct_relevel(age4_fac,       "Missing"),
    size3_fac      = fct_relevel(size3_fac,      "Missing"),
    gender2_fac    = fct_relevel(gender2_fac,    "Missing")
  )

model_clean <- lm(
  density.epithelial.CD3CD4 ~
    d.stage        +  
    location3_fac  +  
    dysplasia2_fac +  
    age4_fac       +  
    size3_fac      + 
    gender2_fac,      
  data = d1_sub2
)

summary(model_clean)

newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("0", 3), levels = levels(d1_sub2$dysplasia2_fac)), 
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

preds <- predict(model_clean, newdata, interval = "confidence", level = 0.95)

adjusted_df <- bind_cols(newdata, as.data.frame(preds)) %>%
  rename(AdjustedMean = fit, LowerCI = lwr, UpperCI = upr)

iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Median = median(density.epithelial.CD3CD4, na.rm = TRUE),
    Q1 = quantile(density.epithelial.CD3CD4, 0.25, na.rm = TRUE),
    Q3 = quantile(density.epithelial.CD3CD4, 0.75, na.rm = TRUE)
  )

adjusted_summary <- adjusted_df %>%
  left_join(iqr_df, by = "d.stage") %>%
  mutate(Type = "Adjusted") %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3, Type)

raw_summary <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    AdjustedMean = mean(density.epithelial.CD3CD4, na.rm = TRUE),
    n = sum(!is.na(density.epithelial.CD3CD4)),
    sd = sd(density.epithelial.CD3CD4, na.rm = TRUE),
    se = sd / sqrt(n),
    LowerCI = AdjustedMean - qt(0.975, df = n - 1) * se,
    UpperCI = AdjustedMean + qt(0.975, df = n - 1) * se,
    Median = median(density.epithelial.CD3CD4, na.rm = TRUE),
    Q1 = quantile(density.epithelial.CD3CD4, 0.25, na.rm = TRUE),
    Q3 = quantile(density.epithelial.CD3CD4, 0.75, na.rm = TRUE)
  ) %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3) %>%
  mutate(Type = "Raw")

final_df <- bind_rows(raw_summary, adjusted_summary) %>%
  arrange(d.stage, Type)

final_df <- final_df %>%
  mutate(LowerCI = ifelse(LowerCI < 0, 0, LowerCI))

print(final_df)

ggplot(adjusted_summary, aes(x = d.stage, y = AdjustedMean)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, color = "blue") +
  theme_minimal() +
  xlab("(d.stage)") +
  ylab("M 95%CI）") +
  ggtitle("M 95%CI")

d1_sub2 <- d1_sub2 %>%
  mutate(predicted = predict(model_clean, newdata = d1_sub2))

adjusted_iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Adjusted_Median = median(predicted, na.rm = TRUE),
    Adjusted_Q1 = quantile(predicted, 0.25, na.rm = TRUE),
    Adjusted_Q3 = quantile(predicted, 0.75, na.rm = TRUE)
  )

print(adjusted_iqr_df)

residuals <- residuals(model_clean)

hist(residuals, breaks = 30, main = "Residuals Histogram", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

shapiro.test(residuals)

print(levels(d1_sub2$location3_fac))
print(levels(d1_sub2$dysplasia2_fac))
print(levels(d1_sub2$age4_fac))
print(levels(d1_sub2$size3_fac))
print(levels(d1_sub2$gender2_fac))

str(newdata)
print(newdata)

preds <- tryCatch({
  predict(model_clean, newdata, interval = "confidence", level = 0.95)
}, error = function(e) {
  message("predict()error: ", e$message)
  NULL
})

if (!is.null(preds)) {
  print("predict() normal。")
  print(head(preds))
} else {
  print("predict() not normal")
}

newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("Missing", 3), levels = levels(d1_sub2$dysplasia2_fac)),
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

print(adjusted_df)

group1 <- d1_sub2$density.epithelial.CD3CD4[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.epithelial.CD3CD4[d1_sub2$d.stage == "2classical"]

wilcox.test(group1, group2, alternative = "two.sided")

group1 <- d1_sub2$density.epithelial.CD3CD4[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.epithelial.CD3CD4[d1_sub2$d.stage == "3serrated"]

wilcox.test(group1, group2, alternative = "two.sided")
##########################################################################################################
# density.epithelial.CD3CD4 END





# density.epithelial.CD3CD8 START
######################################################################################################################
d1_sub <- d1 %>%
  filter(d.stage %in% c("1normal", "2classical", "3serrated"),
         !is.na(density.epithelial.CD3CD8)) %>%
  mutate(d.stage = factor(d.stage, levels = c("1normal", "2classical", "3serrated")))

summary_stats <- d1_sub %>%
  group_by(d.stage) %>%
  summarise(
    n = n(),
    median = median(density.epithelial.CD3CD8, na.rm = TRUE),
    IQR_lower = quantile(density.epithelial.CD3CD8, 0.25, na.rm = TRUE),
    IQR_upper = quantile(density.epithelial.CD3CD8, 0.75, na.rm = TRUE),
    min = min(density.epithelial.CD3CD8, na.rm = TRUE),
    max = max(density.epithelial.CD3CD8, na.rm = TRUE),
    mean = mean(density.epithelial.CD3CD8, na.rm = TRUE)
  )
print(summary_stats)

d1_sub2 <- d1 %>%
  filter(
    d.stage %in% c("1normal","2classical","3serrated"),
    !is.na(density.epithelial.CD3CD8)
  ) %>%
  mutate(
    d.stage        = factor(d.stage, levels = c("1normal","2classical","3serrated")),
    
    location3_fac  = fct_explicit_na(factor(location3),  na_level = "Missing"),
    dysplasia2_fac = fct_explicit_na(factor(dysplasia2), na_level = "Missing"),
    age4_fac       = fct_explicit_na(factor(age4),       na_level = "Missing"),
    size3_fac      = fct_explicit_na(factor(size3),      na_level = "Missing"),
    gender2_fac    = fct_explicit_na(factor(gender2),    na_level = "Missing")
  ) %>%
  mutate(

    location3_fac  = fct_relevel(location3_fac,  "Missing"),
    dysplasia2_fac = fct_relevel(dysplasia2_fac, "Missing"),
    age4_fac       = fct_relevel(age4_fac,       "Missing"),
    size3_fac      = fct_relevel(size3_fac,      "Missing"),
    gender2_fac    = fct_relevel(gender2_fac,    "Missing")
  )

model_clean <- lm(
  density.epithelial.CD3CD8 ~
    d.stage        +  
    location3_fac  +  
    dysplasia2_fac +  
    age4_fac       +  
    size3_fac      +  
    gender2_fac,     
  data = d1_sub2
)

summary(model_clean)


newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("0", 3), levels = levels(d1_sub2$dysplasia2_fac)),  # Missingなしのため0に変更
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

preds <- predict(model_clean, newdata, interval = "confidence", level = 0.95)

adjusted_df <- bind_cols(newdata, as.data.frame(preds)) %>%
  rename(AdjustedMean = fit, LowerCI = lwr, UpperCI = upr)

iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Median = median(density.epithelial.CD3CD8, na.rm = TRUE),
    Q1 = quantile(density.epithelial.CD3CD8, 0.25, na.rm = TRUE),
    Q3 = quantile(density.epithelial.CD3CD8, 0.75, na.rm = TRUE)
  )

adjusted_summary <- adjusted_df %>%
  left_join(iqr_df, by = "d.stage") %>%
  mutate(Type = "Adjusted") %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3, Type)

raw_summary <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    AdjustedMean = mean(density.epithelial.CD3CD8, na.rm = TRUE),
    n = sum(!is.na(density.epithelial.CD3CD8)),
    sd = sd(density.epithelial.CD3CD8, na.rm = TRUE),
    se = sd / sqrt(n),
    LowerCI = AdjustedMean - qt(0.975, df = n - 1) * se,
    UpperCI = AdjustedMean + qt(0.975, df = n - 1) * se,
    Median = median(density.epithelial.CD3CD8, na.rm = TRUE),
    Q1 = quantile(density.epithelial.CD3CD8, 0.25, na.rm = TRUE),
    Q3 = quantile(density.epithelial.CD3CD8, 0.75, na.rm = TRUE)
  ) %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3) %>%
  mutate(Type = "Raw")

final_df <- bind_rows(raw_summary, adjusted_summary) %>%
  arrange(d.stage, Type)

final_df <- final_df %>%
  mutate(LowerCI = ifelse(LowerCI < 0, 0, LowerCI))

print(final_df)

ggplot(adjusted_summary, aes(x = d.stage, y = AdjustedMean)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, color = "blue") +
  theme_minimal() +
  xlab("(d.stage)") +
  ylab("M ± 95%CI") +
  ggtitle("M 95%CI")

d1_sub2 <- d1_sub2 %>%
  mutate(predicted = predict(model_clean, newdata = d1_sub2))

adjusted_iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Adjusted_Median = median(predicted, na.rm = TRUE),
    Adjusted_Q1 = quantile(predicted, 0.25, na.rm = TRUE),
    Adjusted_Q3 = quantile(predicted, 0.75, na.rm = TRUE)
  )

print(adjusted_iqr_df)

residuals <- residuals(model_clean)

hist(residuals, breaks = 30, main = "Residuals Histogram", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

shapiro.test(residuals)

print(levels(d1_sub2$location3_fac))
print(levels(d1_sub2$dysplasia2_fac))
print(levels(d1_sub2$age4_fac))
print(levels(d1_sub2$size3_fac))
print(levels(d1_sub2$gender2_fac))

str(newdata)
print(newdata)

preds <- tryCatch({
  predict(model_clean, newdata, interval = "confidence", level = 0.95)
}, error = function(e) {
  message("predict() error: ", e$message)
  NULL
})

if (!is.null(preds)) {
  print("predict() normal")
  print(head(preds))
} else {
  print("predict() not normal")
}

newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("Missing", 3), levels = levels(d1_sub2$dysplasia2_fac)),
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

print(adjusted_df)

group1 <- d1_sub2$density.epithelial.CD3CD8[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.epithelial.CD3CD8[d1_sub2$d.stage == "2classical"]
wilcox.test(group1, group2, alternative = "two.sided")

group1 <- d1_sub2$density.epithelial.CD3CD8[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.epithelial.CD3CD8[d1_sub2$d.stage == "3serrated"]
wilcox.test(group1, group2, alternative = "two.sided")
##########################################################################################################
# density.epithelial.CD3CD8 END




# density.laminapropria.CD3CD4 START
######################################################################################################################
d1_sub <- d1 %>%
  filter(d.stage %in% c("1normal", "2classical", "3serrated"),
         !is.na(density.laminapropria.CD3CD4)) %>%
  mutate(d.stage = factor(d.stage, levels = c("1normal", "2classical", "3serrated")))

summary_stats <- d1_sub %>%
  group_by(d.stage) %>%
  summarise(
    n = n(),
    median = median(density.laminapropria.CD3CD4, na.rm = TRUE),
    IQR_lower = quantile(density.laminapropria.CD3CD4, 0.25, na.rm = TRUE),
    IQR_upper = quantile(density.laminapropria.CD3CD4, 0.75, na.rm = TRUE),
    min = min(density.laminapropria.CD3CD4, na.rm = TRUE),
    max = max(density.laminapropria.CD3CD4, na.rm = TRUE),
    mean = mean(density.laminapropria.CD3CD4, na.rm = TRUE)
  )
print(summary_stats)

d1_sub2 <- d1 %>%
  filter(
    d.stage %in% c("1normal","2classical","3serrated"),
    !is.na(density.laminapropria.CD3CD4)
  ) %>%
  mutate(
    d.stage        = factor(d.stage, levels = c("1normal","2classical","3serrated")),
    
    location3_fac  = fct_explicit_na(factor(location3),  na_level = "Missing"),
    dysplasia2_fac = fct_explicit_na(factor(dysplasia2), na_level = "Missing"),
    age4_fac       = fct_explicit_na(factor(age4),       na_level = "Missing"),
    size3_fac      = fct_explicit_na(factor(size3),      na_level = "Missing"),
    gender2_fac    = fct_explicit_na(factor(gender2),    na_level = "Missing")
  ) %>%
  mutate(
    location3_fac  = fct_relevel(location3_fac,  "Missing"),
    dysplasia2_fac = fct_relevel(dysplasia2_fac, "Missing"),
    age4_fac       = fct_relevel(age4_fac,       "Missing"),
    size3_fac      = fct_relevel(size3_fac,      "Missing"),
    gender2_fac    = fct_relevel(gender2_fac,    "Missing")
  )

model_clean <- lm(
  density.laminapropria.CD3CD4 ~
    d.stage        +  
    location3_fac  +  
    dysplasia2_fac +  
    age4_fac       +  
    size3_fac      +  
    gender2_fac,      
  data = d1_sub2
)

summary(model_clean)

newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("0", 3), levels = levels(d1_sub2$dysplasia2_fac)),  
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

preds <- predict(model_clean, newdata, interval = "confidence", level = 0.95)

adjusted_df <- bind_cols(newdata, as.data.frame(preds)) %>%
  rename(AdjustedMean = fit, LowerCI = lwr, UpperCI = upr)

iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Median = median(density.laminapropria.CD3CD4, na.rm = TRUE),
    Q1 = quantile(density.laminapropria.CD3CD4, 0.25, na.rm = TRUE),
    Q3 = quantile(density.laminapropria.CD3CD4, 0.75, na.rm = TRUE)
  )

adjusted_summary <- adjusted_df %>%
  left_join(iqr_df, by = "d.stage") %>%
  mutate(Type = "Adjusted") %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3, Type)

raw_summary <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    AdjustedMean = mean(density.laminapropria.CD3CD4, na.rm = TRUE),
    n = sum(!is.na(density.laminapropria.CD3CD4)),
    sd = sd(density.laminapropria.CD3CD4, na.rm = TRUE),
    se = sd / sqrt(n),
    LowerCI = AdjustedMean - qt(0.975, df = n - 1) * se,
    UpperCI = AdjustedMean + qt(0.975, df = n - 1) * se,
    Median = median(density.laminapropria.CD3CD4, na.rm = TRUE),
    Q1 = quantile(density.laminapropria.CD3CD4, 0.25, na.rm = TRUE),
    Q3 = quantile(density.laminapropria.CD3CD4, 0.75, na.rm = TRUE)
  ) %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3) %>%
  mutate(Type = "Raw")

final_df <- bind_rows(raw_summary, adjusted_summary) %>%
  arrange(d.stage, Type)

final_df <- final_df %>%
  mutate(LowerCI = ifelse(LowerCI < 0, 0, LowerCI))

print(final_df)

ggplot(adjusted_summary, aes(x = d.stage, y = AdjustedMean)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, color = "blue") +
  theme_minimal() +
  xlab("(d.stage)") +
  ylab("M ± 95%CI") +
  ggtitle("M 95%CI")

d1_sub2 <- d1_sub2 %>%
  mutate(predicted = predict(model_clean, newdata = d1_sub2))

adjusted_iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Adjusted_Median = median(predicted, na.rm = TRUE),
    Adjusted_Q1 = quantile(predicted, 0.25, na.rm = TRUE),
    Adjusted_Q3 = quantile(predicted, 0.75, na.rm = TRUE)
  )

print(adjusted_iqr_df)

residuals <- residuals(model_clean)

hist(residuals, breaks = 30, main = "Residuals Histogram", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

shapiro.test(residuals)

print(levels(d1_sub2$location3_fac))
print(levels(d1_sub2$dysplasia2_fac))
print(levels(d1_sub2$age4_fac))
print(levels(d1_sub2$size3_fac))
print(levels(d1_sub2$gender2_fac))

str(newdata)
print(newdata)

preds <- tryCatch({
  predict(model_clean, newdata, interval = "confidence", level = 0.95)
}, error = function(e) {
  message("predict() error: ", e$message)
  NULL
})

if (!is.null(preds)) {
  print("predict() normal")
  print(head(preds))
} else {
  print("predict() not normal")
}

newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("Missing", 3), levels = levels(d1_sub2$dysplasia2_fac)),
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

print(adjusted_df)


group1 <- d1_sub2$density.laminapropria.CD3CD4[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.laminapropria.CD3CD4[d1_sub2$d.stage == "2classical"]
wilcox.test(group1, group2, alternative = "two.sided")

group1 <- d1_sub2$density.laminapropria.CD3CD4[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.laminapropria.CD3CD4[d1_sub2$d.stage == "3serrated"]
wilcox.test(group1, group2, alternative = "two.sided")
##########################################################################################################
# density.laminapropria.CD3CD4 END






# density.laminapropria.CD3CD8 START
######################################################################################################################
d1_sub <- d1 %>%
  filter(d.stage %in% c("1normal", "2classical", "3serrated"),
         !is.na(density.laminapropria.CD3CD8)) %>%
  mutate(d.stage = factor(d.stage, levels = c("1normal", "2classical", "3serrated")))

summary_stats <- d1_sub %>%
  group_by(d.stage) %>%
  summarise(
    n = n(),
    median = median(density.laminapropria.CD3CD8, na.rm = TRUE),
    IQR_lower = quantile(density.laminapropria.CD3CD8, 0.25, na.rm = TRUE),
    IQR_upper = quantile(density.laminapropria.CD3CD8, 0.75, na.rm = TRUE),
    min = min(density.laminapropria.CD3CD8, na.rm = TRUE),
    max = max(density.laminapropria.CD3CD8, na.rm = TRUE),
    mean = mean(density.laminapropria.CD3CD8, na.rm = TRUE)
  )
print(summary_stats)

d1_sub2 <- d1 %>%
  filter(
    d.stage %in% c("1normal","2classical","3serrated"),
    !is.na(density.laminapropria.CD3CD8)
  ) %>%
  mutate(
    d.stage        = factor(d.stage, levels = c("1normal","2classical","3serrated")),
    
    location3_fac  = fct_explicit_na(factor(location3),  na_level = "Missing"),
    dysplasia2_fac = fct_explicit_na(factor(dysplasia2), na_level = "Missing"),
    age4_fac       = fct_explicit_na(factor(age4),       na_level = "Missing"),
    size3_fac      = fct_explicit_na(factor(size3),      na_level = "Missing"),
    gender2_fac    = fct_explicit_na(factor(gender2),    na_level = "Missing")
  ) %>%
  mutate(
    
    location3_fac  = fct_relevel(location3_fac,  "Missing"),
    dysplasia2_fac = fct_relevel(dysplasia2_fac, "Missing"),
    age4_fac       = fct_relevel(age4_fac,       "Missing"),
    size3_fac      = fct_relevel(size3_fac,      "Missing"),
    gender2_fac    = fct_relevel(gender2_fac,    "Missing")
  )


model_clean <- lm(
  density.laminapropria.CD3CD8 ~
    d.stage        +  
    location3_fac  +  
    dysplasia2_fac +  
    age4_fac       +  
    size3_fac      +  
    gender2_fac,      
  data = d1_sub2
)

summary(model_clean)

newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("0", 3), levels = levels(d1_sub2$dysplasia2_fac)),  
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

preds <- predict(model_clean, newdata, interval = "confidence", level = 0.95)

adjusted_df <- bind_cols(newdata, as.data.frame(preds)) %>%
  rename(AdjustedMean = fit, LowerCI = lwr, UpperCI = upr)

iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Median = median(density.laminapropria.CD3CD8, na.rm = TRUE),
    Q1 = quantile(density.laminapropria.CD3CD8, 0.25, na.rm = TRUE),
    Q3 = quantile(density.laminapropria.CD3CD8, 0.75, na.rm = TRUE)
  )

adjusted_summary <- adjusted_df %>%
  left_join(iqr_df, by = "d.stage") %>%
  mutate(Type = "Adjusted") %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3, Type)

raw_summary <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    AdjustedMean = mean(density.laminapropria.CD3CD8, na.rm = TRUE),
    n = sum(!is.na(density.laminapropria.CD3CD8)),
    sd = sd(density.laminapropria.CD3CD8, na.rm = TRUE),
    se = sd / sqrt(n),
    LowerCI = AdjustedMean - qt(0.975, df = n - 1) * se,
    UpperCI = AdjustedMean + qt(0.975, df = n - 1) * se,
    Median = median(density.laminapropria.CD3CD8, na.rm = TRUE),
    Q1 = quantile(density.laminapropria.CD3CD8, 0.25, na.rm = TRUE),
    Q3 = quantile(density.laminapropria.CD3CD8, 0.75, na.rm = TRUE)
  ) %>%
  select(d.stage, AdjustedMean, LowerCI, UpperCI, Median, Q1, Q3) %>%
  mutate(Type = "Raw")

final_df <- bind_rows(raw_summary, adjusted_summary) %>%
  arrange(d.stage, Type)

final_df <- final_df %>%
  mutate(LowerCI = ifelse(LowerCI < 0, 0, LowerCI))

print(final_df)

ggplot(adjusted_summary, aes(x = d.stage, y = AdjustedMean)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, color = "blue") +
  theme_minimal() +
  xlab("(d.stage)") +
  ylab("M ± 95%CI") +
  ggtitle("M 95%CI")

d1_sub2 <- d1_sub2 %>%
  mutate(predicted = predict(model_clean, newdata = d1_sub2))

adjusted_iqr_df <- d1_sub2 %>%
  group_by(d.stage) %>%
  summarise(
    Adjusted_Median = median(predicted, na.rm = TRUE),
    Adjusted_Q1 = quantile(predicted, 0.25, na.rm = TRUE),
    Adjusted_Q3 = quantile(predicted, 0.75, na.rm = TRUE)
  )

print(adjusted_iqr_df)

residuals <- residuals(model_clean)

hist(residuals, breaks = 30, main = "Residuals Histogram", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

shapiro.test(residuals)

print(levels(d1_sub2$location3_fac))
print(levels(d1_sub2$dysplasia2_fac))
print(levels(d1_sub2$age4_fac))
print(levels(d1_sub2$size3_fac))
print(levels(d1_sub2$gender2_fac))

str(newdata)
print(newdata)

preds <- tryCatch({
  predict(model_clean, newdata, interval = "confidence", level = 0.95)
}, error = function(e) {
  message("predict() error: ", e$message)
  NULL
})

if (!is.null(preds)) {
  print("predict() normal")
  print(head(preds))
} else {
  print("predict() not normal")
}

newdata <- data.frame(
  d.stage = factor(c("1normal", "2classical", "3serrated"),
                   levels = levels(d1_sub2$d.stage)),
  location3_fac  = factor(rep("Missing", 3), levels = levels(d1_sub2$location3_fac)),
  dysplasia2_fac = factor(rep("Missing", 3), levels = levels(d1_sub2$dysplasia2_fac)),
  age4_fac       = factor(rep("Missing", 3), levels = levels(d1_sub2$age4_fac)),
  size3_fac      = factor(rep("Missing", 3), levels = levels(d1_sub2$size3_fac)),
  gender2_fac    = factor(rep("Missing", 3), levels = levels(d1_sub2$gender2_fac))
)

print(adjusted_df)

group1 <- d1_sub2$density.laminapropria.CD3CD8[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.laminapropria.CD3CD8[d1_sub2$d.stage == "2classical"]
wilcox.test(group1, group2, alternative = "two.sided")

group1 <- d1_sub2$density.laminapropria.CD3CD8[d1_sub2$d.stage == "1normal"]
group2 <- d1_sub2$density.laminapropria.CD3CD8[d1_sub2$d.stage == "3serrated"]
wilcox.test(group1, group2, alternative = "two.sided")
##########################################################################################################
# density.laminapropria.CD3CD8 END