library(did)
library(cdid)
library(pretrends)
library(readr)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(plm)


food_sbi = c("100", "101","102","103","104","105","106","107","108", "109")#,
#food processing
# "110", #drink processing
# "120", #tobacco processing
# "462", #Groothandel in Landbouwproducten
# "463") #Groothandel in Voedings- en Genotmiddelen

plot_did =  function(cof, outcome) cof %>% ggplot(aes(x=t, y = estimate, color = method)) +
  geom_point(aes(x=t,y=estimate), position = position_dodge2(width=0.8), size=1)+
  geom_linerange(aes(x=t, ymin= conf.low, ymax= conf.high),
                 position = position_dodge2(width = 0.8), size =1) +
  labs(title = outcome, y= "ATT", x = "Relative Time") + theme(legend.position = 'bottom')

bind_plot = function(csa_c, c_did_c, outcome_c)  plot_did(
  bind_rows(csa_c, c_did_c), outcome_c)

#CSA did function
csa_did = function (outcome, dataset, did_type) aggte(att_gt(
  yname = outcome,
  tname = "year",
  idname = NULL,
  gname = "treatment_group",
  xformla = NULL,
  data = dataset,
  panel = F, # False for repeated cross-sections
  allow_unbalanced_panel = TRUE,
  control_group = c("notyettreated"),
  weightsname = "EindGewicht"),
  type= did_type, na.rm = T) 

#chained did function
cdid = function(outcome, dataset, did_type) aggte(att_gt_cdid(
  yname = outcome,
  tname = "year",
  idname = "id",
  gname = "treatment_group",
  xformla = NULL,
  data = dataset,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = "EindGewicht",
  alp = 0.1,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "Identity",
  base_period = "universal",
  print_details = FALSE,
  pl = FALSE,
  cores = 1), type= did_type, na.rm = T)

#chained did function with covariate
cdid_cov = function(outcome, dataset, did_type) aggte(att_gt_cdid(
  yname = outcome,
  tname = "year",
  idname = "id",
  gname = "treatment_group",
  xformla = ~small_size,
  data = dataset,
  control_group = c("notyettreated"),
  anticipation = 0,
  weightsname = "EindGewicht",
  alp = 0.1,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "Identity",
  base_period = "universal",
  print_details = FALSE,
  pl = FALSE,
  cores = 1), type= did_type, na.rm = T)



did_estimates = function(did_f, outcome, data) {
  a = glance(did_f(outcome, data, "dynamic"))
  cbind(rbind(
    tidy(did_f(outcome, data, "simple")) %>% 
      mutate(t = "average") %>%
      select(type, t, estimate, std.error, conf.low, conf.high),
    tidy(did_f(outcome, data, "group")) %>%
      rename(t= group) %>%
      select(type, t, estimate, std.error, conf.low, conf.high),
    tidy(did_f(outcome, data, "dynamic")) %>%
      rename(t= event.time) %>%
      select(type, t, estimate, std.error, conf.low, conf.high),
    tidy(did_f(outcome, data, "calendar")) %>%
      rename(t=time) %>%
      select(type, t, estimate, std.error, conf.low, conf.high)),
    a$nobs) %>% mutate(dependent_var = outcome)
}

power_slope = function(outcome, data){
  did_res = aggte(att_gt_cdid(
    yname = outcome,
    tname = "year",
    idname = "id",
    gname = "treatment_group",
    xformla = NULL,
    data = data,
    control_group = c("notyettreated"),
    anticipation = 0,
    weightsname = "EindGewicht",
    alp = 0.1,
    bstrap = TRUE,
    cband = TRUE,
    biters = 1000,
    clustervars = NULL,
    est_method = "Identity",
    base_period = "universal",
    print_details = FALSE,
    pl = FALSE,
    cores = 1), type= "dynamic", na.rm = T)
  
  es_inf_func = did_res$inf.function$dynamic.inf.func.e
  #recover variance-civariance matrix
  n = nrow(es_inf_func)
  V = t(es_inf_func) %*% es_inf_func / n / n
  
  referencePeriod = -1
  
  #how to contruct sigma
  slope5 = slope_for_power(sigma = V,
                           targetPower = c(0.5),
                           tVec = did_res$egt,
                           referencePeriod = referencePeriod)
  slope8 = slope_for_power(sigma = V,
                           targetPower = c(0.8),
                           tVec = did_res$egt,
                           referencePeriod = referencePeriod)
  return(c(slope5,slope8))
  
}
#combine output from regression
output_reg = function(x) cbind(tidy(x),glance(x))

m1 = read_csv("Data/cleaned/merged_data.csv")

# difference in differences 

# create data ----

#create g variable 
m1=m1[!m1$turnover_d == 0,]
m1=m1[!m1$wages == 0,]
m2=m1

m4 = m2[, c("TFP","l_wages", "l_labor_quantity", "turnover_d", "wages", "PERSONS110000",  "energy_intensity",
            "l_turnover", "energy_expenditure_d", "BEDRLST341000",
            "year", "id","EindGewicht", "treatment_group", "small_size", "food_processing")]


#without imputed non-treatments
m5 = m1
m5 = m5[!is.na(m5$wages), ]
m5 = m5[!m5$treatment_group == 2020, ]
m5 = m5[!m5$turnover_d == 0,]
m5 = m5[!(m5$year < 2020),]

m5 = m5[, c("TFP", "l_wages", "l_labor_quantity","turnover_d", "wages", "PERSONS110000",  "energy_intensity",
            "l_turnover", "energy_expenditure_d", "BEDRLST341000",
            "year", "id","EindGewicht", "treatment_group", "treatment_group_prod", "small_size", "food_processing", "sbi")]


m8 = m1
m8 = m8[!is.na(m8$wages), ]
m8 = m8[!m8$treatment_group_prod == 2020, ]
m8 = m8[!m8$turnover_d == 0,]
m8 = m8[!(m8$year < 2020),]

m8$treatment_group = m8$treatment_group_prod
m8 = m8[, c("TFP", "l_wages", "l_labor_quantity", "turnover_d", "wages", "PERSONS110000",  "energy_intensity",
            "l_turnover", "energy_expenditure_d", "BEDRLST341000",
            "year", "id","EindGewicht", "treatment_group", "small_size", "food_processing")]

#ai production
m6 = m1
m6$treatment_group = m6$treatment_group_prod
m6 = m6[, c("TFP", "l_wages", "l_labor_quantity", "turnover_d", "wages", "PERSONS110000",  "energy_intensity",
            "l_turnover", "energy_expenditure_d", "BEDRLST341000",
            "year", "id","EindGewicht", "treatment_group", "small_size", "food_processing")]

#only food processing


m7 = m1

m7 = m7[substr(m7$sbi, 1, 3) %in% food_sbi, ] #sbi_ps
m7 = m7[, c("TFP", "l_wages", "l_labor_quantity", "turnover_d", "wages", "PERSONS110000",  "energy_intensity",
            "l_turnover", "energy_expenditure_d", "BEDRLST341000",
            "year", "id","EindGewicht", "treatment_group", "small_size", "food_processing")]

#take out treatment in 2020

m9 = m4[!m4$treatment_group == 2020, ]
m10 = m6[!m6$treatment_group == 2020, ]



m11 = m2[, c("TFP", "l_wages", "l_labor_quantity", "turnover_d", "wages", "PERSONS110000",  "energy_intensity",
             "l_turnover", "energy_expenditure_d", "BEDRLST341000", "treat_off",
             "year", "id","EindGewicht", "treatment_group", "small_size", "food_processing", "ever_treat_off")]

exclude = m11 %>% filter(ever_treat_off == 1) %>% pull(id) %>% unique()
m11 = m11 %>% filter(!(id %in% exclude))






