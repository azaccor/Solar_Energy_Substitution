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

## This is used for significance testing, add second for loop to expand by a FE variable.
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

## This is used for significance testing, second for loop added to expand by a FE variable.
# self_pred_sums <- c()
# gross_pred_sums <- c()
# imp_pred_sums <- c()
# self_act_sums <- c()
# gross_act_sums <- c()
# imp_act_sums <- c()
# for(i in 1:length(cohorts)){
#   for(j in 1:7){
#     df_i <- df %>% filter(optCohort == i, dow == j, gen_kwh>0)
#     self_pred_sums <- append(self_pred_sums, sum(df_i$self_preds))
#     gross_pred_sums <- append(gross_pred_sums, sum(df_i$gross_preds))
#     imp_pred_sums <- append(imp_pred_sums, sum(df_i$imp_preds))
#     
#     self_act_sums <- append(self_act_sums, sum(df_i$selfcons_kwh))
#     gross_act_sums <- append(gross_act_sums, sum(df_i$gross_kwh))
#     imp_act_sums <- append(imp_act_sums, sum(df_i$import_kwh))
#   }
# }

## Basic histograms
hist(self_act_sums)
hist(self_pred_sums)

## Overlapped density plots
hh_selfs <- data.frame(self_pred_sums, self_act_sums) %>%
  gather("self_pred_sums", "self_act_sums", key = Type, value = kWh)
hh_gross <- data.frame(gross_pred_sums, gross_act_sums) %>%
  gather("gross_pred_sums", "gross_act_sums", key = Type, value = kWh)
hh_imps <- data.frame(imp_pred_sums, imp_act_sums) %>%
  gather("imp_pred_sums", "imp_act_sums", key = Type, value = kWh)

ggdensity(hh_selfs, x = "kWh",
          add = "mean", rug = TRUE,
          color = "Type", fill = "Type",
          palette = c("#FC4E07", "#0073C2FF")) +
  ylab("Density") +
  xlab("Self Consumption in kWh") +
  ggtitle("Density Graph for Total Household Self Consumption")
  
ggdensity(hh_gross, x = "kWh",
          add = "mean", rug = TRUE,
          color = "Type", fill = "Type",
          palette = c("#FC4E07", "#0073C2FF")) +
  ylab("Density") +
  xlab("Gross Consumption in kWh") +
  ggtitle("Density Graph for Total Household Gross Consumption")

ggdensity(hh_imps, x = "kWh",
          add = "mean", rug = TRUE,
          color = "Type", fill = "Type",
          palette = c("#FC4E07", "#0073C2FF")) +
  ylab("Density") +
  xlab("Imports in kWh") +
  ggtitle("Density Graph for Total Household Imports")

## Back to good old fashioned histograms
hist(self_pred_sums - self_act_sums, main = "HH Agg Self Cons ATEs", xlab = "Predicted - Actual")
hist(gross_pred_sums - gross_act_sums, main = "HH Agg Gross Cons ATEs", xlab = "Predicted - Actual")
hist(imp_pred_sums - imp_act_sums, main = "HH Agg Import ATEs", xlab = "Predicted - Actual")

## Paired Sample T-Test on Actual vs Predicted on total by household:
t.test(self_pred_sums, self_act_sums, alternative = "less", paired = TRUE, var.equal = FALSE, conf.level = 0.95)
t.test(gross_pred_sums, gross_act_sums, alternative = "greater", paired = TRUE, var.equal = FALSE, conf.level = 0.95)
t.test(imp_pred_sums, imp_act_sums, alternative = "greater", paired = TRUE, var.equal = FALSE, conf.level = 0.95)

## Just to prove to ourselves what's going on here:
mean(self_pred_sums)
mean(self_act_sums)

## What is the average test period for a cohort? About 1.25 years.
date_ranges <- df %>%
  select(sid, day) %>%
  group_by(sid) %>%
  summarise(date_rng = as.numeric(date(max(day)) - date(min(day))))

mean(date_ranges$date_rng)

## Aggregate all results together by household and visualize similar to Burlig et al.
hh_hour_level <- as.data.frame(df %>%
  filter(gen_kwh>0) %>%
  group_by(optCohort, hour) %>%
  summarise(sps = sum(self_preds), gps = sum(gross_preds), ips = sum(imp_preds),
            sas = sum(selfcons_kwh), gas = sum(gross_kwh), ias = sum(import_kwh)) %>%
  group_by(hour) %>%
  summarise(sps = mean(sps), gps = mean(gps), ips = mean(ips), sas = mean(sas), gas = mean(gas), ias = mean(ias)))

hh_dow_level <- as.data.frame(df %>%
  filter(gen_kwh>0) %>%
  group_by(optCohort, dow) %>%
  summarise(sps = sum(self_preds), gps = sum(gross_preds), ips = sum(imp_preds),
            sas = sum(selfcons_kwh), gas = sum(gross_kwh), ias = sum(import_kwh)) %>%
  group_by(dow) %>%
  summarise(sps = mean(sps), gps = mean(gps), ips = mean(ips), sas = mean(sas), gas = mean(gas), ias = mean(ias)))

hh_month_level <- as.data.frame(df %>%
  filter(gen_kwh>0) %>%
  group_by(optCohort, month) %>%
  summarise(sps = sum(self_preds), gps = sum(gross_preds), ips = sum(imp_preds),
            sas = sum(selfcons_kwh), gas = sum(gross_kwh), ias = sum(import_kwh)) %>%
  group_by(month) %>%
  summarise(sps = mean(sps), gps = mean(gps), ips = mean(ips), sas = mean(sas), gas = mean(gas), ias = mean(ias)))

#hh_hour_level$type <- rep("hour", 24)
#hh_dow_level$type <- rep("dow", 7)
#hh_month_level$type <- rep("month", 12)
#colnames(hh_hour_level) <- c("var", "sps", "gps", "ips", "sas", "gas", "ias", "type")
#colnames(hh_dow_level) <- c("var", "sps", "gps", "ips", "sas", "gas", "ias", "type")
#colnames(hh_month_level) <- c("var", "sps", "gps", "ips", "sas", "gas", "ias", "type")

#hh_all_levels <- rbind(hh_hour_level, hh_dow_level)
#hh_all_levels <- rbind(hh_all_levels, hh_month_level)

## Self Cons Plots
ggplot(hh_hour_level, aes(hour)) + 
  geom_point(aes(y = sas, colour = "Actual Self Consumption")) + 
  geom_point(aes(y = sps, colour = "Predicted Self Consumption")) +
  xlab("Hour of Day") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Hour of Day") +
  theme(legend.position="bottom")

ggplot(hh_dow_level, aes(dow)) + 
  geom_point(aes(y = sas, colour = "Actual Self Consumption")) + 
  geom_point(aes(y = sps, colour = "Predicted Self Consumption")) +
  xlab("Day of Week") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Day of Week") +
  theme(legend.position="bottom")

ggplot(hh_month_level, aes(month)) + 
  geom_point(aes(y = sas, colour = "Actual Self Consumption")) + 
  geom_point(aes(y = sps, colour = "Predicted Self Consumption")) +
  xlab("Month") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Month") +
  theme(legend.position="bottom")

## Gross Cons Plots
ggplot(hh_hour_level, aes(hour)) + 
  geom_point(aes(y = gas, colour = "Actual Gross Consumption")) + 
  geom_point(aes(y = gps, colour = "Predicted Gross Consumption")) +
  xlab("Hour of Day") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Hour of Day") +
  theme(legend.position="bottom")

ggplot(hh_dow_level, aes(dow)) + 
  geom_point(aes(y = gas, colour = "Actual Gross Consumption")) + 
  geom_point(aes(y = gps, colour = "Predicted Gross Consumption")) +
  xlab("Day of Week") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Day of Week") +
  theme(legend.position="bottom")

ggplot(hh_month_level, aes(month)) + 
  geom_point(aes(y = gas, colour = "Actual Gross Consumption")) + 
  geom_point(aes(y = gps, colour = "Predicted Gross Consumption")) +
  xlab("Month") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Month") +
  theme(legend.position="bottom")

## Imports Plots
ggplot(hh_hour_level, aes(hour)) + 
  geom_point(aes(y = ias, colour = "Actual Imports")) + 
  geom_point(aes(y = ips, colour = "Predicted Imports")) +
  xlab("Hour of Day") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Hour of Day") +
  theme(legend.position="bottom")

ggplot(hh_dow_level, aes(dow)) + 
  geom_point(aes(y = ias, colour = "Actual Imports")) + 
  geom_point(aes(y = ips, colour = "Predicted Imports")) +
  xlab("Day of Week") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Day of Week") +
  theme(legend.position="bottom")

ggplot(hh_month_level, aes(month)) + 
  geom_point(aes(y = ias, colour = "Actual Imports")) + 
  geom_point(aes(y = ips, colour = "Predicted Imports")) +
  xlab("Month") +
  ylab("Total kWh (Over Observation Period)") +
  ggtitle("Average Treatment Effect by Month") +
  theme(legend.position="bottom")


