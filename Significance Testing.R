setwd("~/Dropbox (MIT)/Zaccor:Pless - ML/data")
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(randomForest)

## Reading in models from `Random Forest.R` 
## Caution, takes a few seconds to appear in global environment
for(i in 1:145){
  assign(paste("Self_", i, sep = ""), readRDS(paste("../models/Self.2_", i, ".rds", sep = "")))
  assign(paste("Gross_", i, sep = ""), readRDS(paste("../models/Gross.2_", i, ".rds", sep = "")))
  assign(paste("Imp_", i, sep = ""), readRDS(paste("../models/Imp.2_", i, ".rds", sep = "")))
}

## Predicting Treatment Group
#control <- fread(file = "df_control.csv", header = TRUE, sep = ",")
#house_means <- control %>%
#  group_by(day, sid) %>%
#  summarise(total_gross = sum(gross_kwh)) %>%
#  group_by(sid) %>%
#  summarise(size_est = mean(total_gross))
#control <- left_join(control, house_means, by = c("sid" = "sid"))

df <- fread(file = "df_test.csv", header = TRUE, sep = ",")
df <- df %>% filter(gen_kwh > 0)
df$month <- as.factor(df$month)
df$dow <- as.factor(df$dow)
df$hour <- as.factor(df$hour)
df$sid <- as.factor(df$sid)
df <- rownames_to_column(df, "index")

house_means <- df %>%
  group_by(day, sid) %>%
  summarise(total_gross = sum(gross_kwh)) %>%
  group_by(sid) %>%
  summarise(size_est = mean(total_gross))

df <- left_join(df, house_means, by = c("sid" = "sid"))

summary(df$size_est)
cohorts <- unique(df$optCohort)

## Generating actual prediction vectors, sorted by index
for(i in 1:length(cohorts)){
  cohort_i <- df %>% filter(optCohort == i) %>% arrange(index)
  
  assign(paste("pred_self_", i, sep = ""), as.vector(as.matrix(predict(get(paste("Self_", i, sep = "")), cohort_i))))
  assign(paste("pred_gross_", i, sep = ""), as.vector(as.matrix(predict(get(paste("Gross_", i, sep = "")), cohort_i))))
  assign(paste("pred_imp_", i, sep = ""), as.vector(as.matrix(predict(get(paste("Imp_", i, sep = "")), cohort_i))))
}

## Aggregating across all households to view Average Treatment Effect
self_deltas <- c()
gross_deltas <- c()
imp_deltas <- c()
for(i in 1:length(cohorts)){
  actual_self = as.vector(as.matrix(df %>% filter(optCohort == i) %>% arrange(index) %>% select(selfcons_kwh)))
  actual_gross = as.vector(as.matrix(df %>% filter(optCohort == i) %>% arrange(index) %>% select(gross_kwh)))
  actual_imp = as.vector(as.matrix(df %>% filter(optCohort == i) %>% arrange(index) %>% select(import_kwh)))
  
  self_deltas <- append(self_deltas, get(paste("pred_self_", i, sep = ""))-actual_self)
  gross_deltas <- append(gross_deltas, get(paste("pred_gross_", i, sep = ""))-actual_gross)
  imp_deltas <- append(imp_deltas, get(paste("pred_imp_", i, sep = ""))-actual_imp)
}

summary(self_deltas)
summary(gross_deltas)
summary(imp_deltas)
## All three distributions are approximately normal. 
hist(self_deltas, xlim = c(-0.3,0.3))
hist(gross_deltas, xlim = c(-1,1))
hist(imp_deltas, xlim = c(-1,1))

## Just Predicted, not deltas.
self_preds <- c()
gross_preds <- c()
imp_preds <- c()
for(i in 1:length(cohorts)){
  self_preds <- append(self_preds, get(paste("pred_self_", i, sep = "")))
  gross_preds <- append(gross_preds, get(paste("pred_gross_", i, sep = "")))
  imp_preds <- append(imp_preds, get(paste("pred_imp_", i, sep = "")))
  ## This one is quick.
}

## Order these properly for paired sample t test
self_actuals_df <- df %>% arrange(optCohort, index) %>% select(index, optCohort, selfcons_kwh)
gross_actuals_df <- df %>% arrange(optCohort, index) %>% select(index, optCohort, gross_kwh)
imp_actuals_df <- df %>% arrange(optCohort, index) %>% select(index, optCohort, import_kwh)

## Append preds to full df so that we can test by hours with daylight and group by household
df <- df %>% arrange(optCohort, index)
df <- cbind(df, self_preds)
df <- cbind(df, gross_preds)
df <- cbind(df, imp_preds)

self_pred_means <- c()
gross_pred_means <- c()
imp_pred_means <- c()
self_act_means <- c()
gross_act_means <- c()
imp_act_means <- c()
for(i in 1:length(cohorts)){
  df_i <- df %>% filter(optCohort == i, gen_kwh > 0)
  self_pred_means <- append(self_pred_means, mean(df_i$self_preds))
  gross_pred_means <- append(gross_pred_means, mean(df_i$gross_preds))
  imp_pred_means <- append(imp_pred_means, mean(df_i$imp_preds))
  
  self_act_means <- append(self_act_means, mean(df_i$selfcons_kwh))
  gross_act_means <- append(gross_act_means, mean(df_i$gross_kwh))
  imp_act_means <- append(imp_act_means, mean(df_i$import_kwh))
}

hist(self_act_means)
hist(self_pred_means)

self_pred_sums <- c()
gross_pred_sums <- c()
imp_pred_sums <- c()
self_act_sums <- c()
gross_act_sums <- c()
imp_act_sums <- c()
for(i in 1:length(cohorts)){
    df_i <- df %>% filter(optCohort == i, gen_kwh>0)
    self_pred_sums <- append(self_pred_sums, sum(df_i$self_preds))
    gross_pred_sums <- append(gross_pred_sums, sum(df_i$gross_preds))
    imp_pred_sums <- append(imp_pred_sums, sum(df_i$imp_preds))
    
    self_act_sums <- append(self_act_sums, sum(df_i$selfcons_kwh))
    gross_act_sums <- append(gross_act_sums, sum(df_i$gross_kwh))
    imp_act_sums <- append(imp_act_sums, sum(df_i$import_kwh))
}

hist(self_act_sums)
hist(self_pred_sums)

hh_selfs <- data.frame(self_pred_sums, self_act_sums) %>%
  gather("self_pred_sums", "self_act_sums", key = Type, value = kWh)
hh_gross <- data.frame(gross_pred_sums, gross_act_sums) %>%
  gather("gross_pred_sums", "gross_act_sums", key = Type, value = kWh)
hh_imps <- data.frame(imp_pred_sums, imp_act_sums) %>%
  gather("imp_pred_sums", "imp_act_sums", key = Type, value = kWh)

ggdensity(hh_selfs, x = "kWh",
          add = "mean", rug = TRUE,
          color = "Type", fill = "Type",
          palette = c("#0073C2FF", "#FC4E07"))
ggdensity(hh_gross, x = "kWh",
          add = "mean", rug = TRUE,
          color = "Type", fill = "Type",
          palette = c("#0073C2FF", "#FC4E07"))
ggdensity(hh_imps, x = "kWh",
          add = "mean", rug = TRUE,
          color = "Type", fill = "Type",
          palette = c("#0073C2FF", "#FC4E07"))


hist(self_pred_sums - self_act_sums, main = "HH Agg Self Cons ATEs", xlab = "Predicted - Actual")
hist(gross_pred_sums - gross_act_sums, main = "HH Agg Gross Cons ATEs", xlab = "Predicted - Actual")
hist(imp_pred_sums - imp_act_sums, main = "HH Agg Import ATEs", xlab = "Predicted - Actual")

## Paired Sample T-Test on Actual vs Predicted:
t.test(self_pred_sums, self_act_sums, alternative = "less", paired = TRUE, var.equal = FALSE, conf.level = 0.95)
t.test(gross_pred_sums, gross_act_sums, alternative = "greater", paired = TRUE, var.equal = FALSE, conf.level = 0.95)
t.test(imp_pred_sums, imp_act_sums, alternative = "greater", paired = TRUE, var.equal = FALSE, conf.level = 0.95)

mean(self_pred_sums)
mean(self_act_sums)


date_ranges <- df %>%
  select(sid, day) %>%
  group_by(sid) %>%
  summarise(date_rng = as.numeric(date(max(day)) - date(min(day))))

mean(date_ranges$date_rng)




## Do not run these lines, meaningless conclusions for now:
## Percentage ATE
self_deltas_pct <- c()
gross_deltas_pct <- c()
imp_deltas_pct <- c()
for(i in 1:length(cohorts)){
  actual_self = as.vector(as.matrix(df %>% filter(optCohort == i) %>% arrange(index) %>% select(selfcons_kwh)))
  actual_gross = as.vector(as.matrix(df %>% filter(optCohort == i) %>% arrange(index) %>% select(gross_kwh)))
  actual_imp = as.vector(as.matrix(df %>% filter(optCohort == i) %>%  arrange(index) %>%select(import_kwh)))
  
  self_deltas_pct <- append(self_deltas_pct, 100*(actual_self/get(paste("pred_self_", i, sep = ""))-1))
  gross_deltas_pct <- append(gross_deltas_pct, 100*(actual_gross/get(paste("pred_gross_", i, sep = ""))-1))
  imp_deltas_pct <- append(imp_deltas_pct, 100*(actual_imp/get(paste("pred_imp_", i, sep = ""))-1))
  
  print(i)
}

summary(self_deltas_pct)
summary(gross_deltas_pct)
summary(imp_deltas_pct)

hist(self_deltas_pct[self_deltas_pct > -20 & self_deltas_pct < 20], breaks = 12, 
     xlab = "% Change", main = "Estimated ATEs on Self Consumption")

hist(gross_deltas_pct[gross_deltas_pct < 140], breaks = 8, 
     xlab = "% Change", main = "Estimated ATEs on Gross Consumption")

hist(imp_deltas_pct[imp_deltas_pct > -100 & imp_deltas_pct < 200], breaks = 14,
     xlab = "% Change", main = "Estimated ATEs on Imported kWh")

hist(imp_deltas_pct)