#packages
library(tidyverse)
library(cobalt)
library(HonestDiD)
library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)

# summary stats data ----


ms1 = m1[m1$year %in% c(2019) , ]

sa1 = bal.tab(ever_adopt ~ small_size + food_processing,
        weights = "EindGewicht", data = ms1, un = TRUE, 
        disp = c("means"), stats = c("mean.diffs"))
#only weighted mean diffs
s1 = cbind(sa1$Balance[, 4:7])
s2 = sa1$Observations 


# discard duplicates
ms2 = m1[!duplicated(m1[, c("be.id")]) , ]
s3 = cbind(nrow(ms2),
rbind(
table(ms2$treatment_group),
table(ms2$treatment_group_prod)),
rbind(table(ms2$ever_treat_off)[2],NA))

colnames(s3) = c("units", "never_treated", "treated_2020", "treated_2021",
                 "treated_2022", "treated_2023", "stop_treatment")
rownames(s3) = c("AI","AI_production")

#create weighted means

#create subsets
m1_treat   = m1[m1$ever_adopt == 1,] 
m1_notreat = m1[m1$ever_adopt == 0,]
m1_treat_pre = m1_treat[m1_treat$treatment_group == (m1_treat$year +1), ]

#yearly weighted means + overall for all obs
s4 = rbind(
c(tapply(1:nrow(m1), m1$year, function(i) { weighted.mean(m1$l_turnover[i], m1$EindGewicht[i])}),
      weighted.mean(m1$l_turnover, m1$EindGewicht) ),
c(tapply(1:nrow(m1), m1$year, function(i) { weighted.mean(m1$l_labor_quantity[i], m1$EindGewicht[i])}),
  weighted.mean(m1$l_labor_quantity, m1$EindGewicht) ),
c(tapply(1:nrow(m1), m1$year, function(i) { weighted.mean(m1$energy_intensity[i], m1$EindGewicht[i])}),
  weighted.mean(m1$energy_intensity, m1$EindGewicht) ),
c(tapply(1:nrow(m1), m1$year, function(i) { weighted.mean(m1$TFP[i], m1$EindGewicht[i])}),
  weighted.mean(m1$TFP, m1$EindGewicht) )
)

colnames(s4)[11] = c("overall")
rownames(s4) = c("l_turnover","labour quantity","energy intensity","TFP")

s4_max =rbind( 
c(tapply(m1$l_turnover, m1$year, max), max(m1$l_turnover)),
c(tapply(m1$l_labor_quantity, m1$year, max), max(m1$l_labor_quantity)),
c(tapply(m1$energy_intensity, m1$year, max), max(m1$energy_intensity)),
c(tapply(m1$TFP, m1$year, max), max(m1$TFP)))

colnames(s4_max)[11] = c("overall")
rownames(s4_max) = c("l_turnover","labour quantity","energy intensity","TFP")

s4_sum =rbind( 
  c(tapply(m1$l_turnover, m1$year, sum), sum(m1$l_turnover)),
  c(tapply(m1$l_labor_quantity, m1$year, sum), sum(m1$l_labor_quantity)),
  c(tapply(m1$energy_intensity, m1$year, sum), sum(m1$energy_intensity)),
  c(tapply(m1$TFP, m1$year, sum), sum(m1$TFP)))

colnames(s4_sum)[11] = c("overall")
rownames(s4_sum) = c("l_turnover","labour quantity","energy intensity","TFP")

#yearly weighted means + overall for all treated + pre-treatment year

s5 = rbind(
  c(tapply(1:nrow(m1_treat), m1_treat$year, function(i) { weighted.mean(m1_treat$l_turnover[i], m1_treat$EindGewicht[i])}),
    weighted.mean(m1_treat$l_turnover, m1_treat$EindGewicht),
    weighted.mean(m1_treat_pre$l_turnover, m1_treat_pre$EindGewicht) ),
  c(tapply(1:nrow(m1_treat), m1_treat$year, 
           function(i) { weighted.mean(m1_treat$l_labor_quantity[i], m1_treat$EindGewicht[i])}),
    weighted.mean(m1_treat$l_labor_quantity, m1_treat$EindGewicht) ,
    weighted.mean(m1_treat_pre$l_labor_quantity, m1_treat_pre$EindGewicht)),
  c(tapply(1:nrow(m1_treat), m1_treat$year, function(i) { weighted.mean(m1_treat$energy_intensity[i], m1_treat$EindGewicht[i])}),
    weighted.mean(m1_treat$energy_intensity, m1_treat$EindGewicht),
    weighted.mean(m1_treat_pre$energy_intensity, m1_treat_pre$EindGewicht) ),
  c(tapply(1:nrow(m1_treat), m1_treat$year, function(i) { weighted.mean(m1_treat$TFP[i], m1_treat$EindGewicht[i])}),
    weighted.mean(m1_treat$TFP, m1_treat$EindGewicht),
    weighted.mean(m1_treat_pre$TFP, m1_treat_pre$EindGewicht) )
)

colnames(s5)[11:12] = c("overall","pre_treatmentperiod")
rownames(s5) = c("l_turnover","labour quantity","energy intensity","TFP")

s5_max =rbind( 
  c(tapply(m1_treat$l_turnover, m1_treat$year, max),
    max(m1_treat$l_turnover), max(m1_treat_pre$l_turnover)),
  c(tapply(m1_treat$l_labor_quantity, m1_treat$year, max),
    max(m1_treat$l_labor_quantity), max(m1_treat_pre$l_labor_quantity)),
  c(tapply(m1_treat$energy_intensity, m1_treat$year, max),
    max(m1_treat$energy_intensity), max(m1_treat_pre$energy_intensity)),
  c(tapply(m1_treat$TFP, m1_treat$year, max),
    max(m1_treat$TFP), max(m1_treat_pre$TFP)))

colnames(s5_max)[11:12] = c("overall","pre_treatmentperiod")
rownames(s5_max) = c("l_turnover","labour quantity","energy intensity","TFP")

s5_sum =rbind( 
  c(tapply(m1_treat$l_turnover, m1_treat$year, sum),
    sum(m1_treat$l_turnover), sum(m1_treat_pre$l_turnover)),
  c(tapply(m1_treat$l_labor_quantity, m1_treat$year,
           sum), sum(m1_treat$l_labor_quantity), sum(m1_treat_pre$l_labor_quantity)),
  c(tapply(m1_treat$energy_intensity, m1_treat$year,
           sum), sum(m1_treat$energy_intensity), sum(m1_treat_pre$energy_intensity)),
  c(tapply(m1_treat$TFP, m1_treat$year, sum),
    sum(m1_treat$TFP), sum(m1_treat_pre$TFP)))

colnames(s5_sum)[11:12] = c("overall","pre_treatmentperiod")
rownames(s5_sum) = c("l_turnover","labour quantity","energy intensity","TFP")


#yearly weighted means + overall for all untreated

s6 = rbind(
  c(tapply(1:nrow(m1_notreat), m1_notreat$year,
           function(i) { weighted.mean(m1_notreat$l_turnover[i], m1_notreat$EindGewicht[i])}),
    weighted.mean(m1_notreat$l_turnover, m1_notreat$EindGewicht) ),
  c(tapply(1:nrow(m1_notreat), m1_notreat$year,
           function(i) { weighted.mean(m1_notreat$l_labor_quantity[i], m1_notreat$EindGewicht[i])}),
    weighted.mean(m1_notreat$l_labor_quantity, m1_notreat$EindGewicht) ),
  c(tapply(1:nrow(m1_notreat), m1_notreat$year,
           function(i) { weighted.mean(m1_notreat$energy_intensity[i], m1_notreat$EindGewicht[i])}),
    weighted.mean(m1_notreat$energy_intensity, m1_notreat$EindGewicht) ),
  c(tapply(1:nrow(m1_notreat), m1_notreat$year,
           function(i) { weighted.mean(m1_notreat$TFP[i], m1_notreat$EindGewicht[i])}),
    weighted.mean(m1_notreat$TFP, m1_notreat$EindGewicht) )
)

colnames(s6)[11] = c("overall")
rownames(s6) = c("l_turnover","labour quantity","energy intensity","TFP")

s6_max =rbind( 
  c(tapply(m1_notreat$l_turnover, m1_notreat$year, max), max(m1_notreat$l_turnover)),
  c(tapply(m1_notreat$l_labor_quantity, m1_notreat$year, max), max(m1_notreat$l_labor_quantity)),
  c(tapply(m1_notreat$energy_intensity, m1_notreat$year, max), max(m1_notreat$energy_intensity)),
  c(tapply(m1_notreat$TFP, m1_notreat$year, max), max(m1_notreat$TFP)))

colnames(s6_max)[11] = c("overall")
rownames(s6_max) = c("l_turnover","labour quantity","energy intensity","TFP")

s6_sum =rbind( 
  c(tapply(m1_notreat$l_turnover, m1_notreat$year, sum), sum(m1_notreat$l_turnover)),
  c(tapply(m1_notreat$l_labor_quantity, m1_notreat$year, sum), sum(m1_notreat$l_labor_quantity)),
  c(tapply(m1_notreat$energy_intensity, m1_notreat$year, sum), sum(m1_notreat$energy_intensity)),
  c(tapply(m1_notreat$TFP, m1_notreat$year, sum), sum(m1_notreat$TFP)))

colnames(s6_sum)[11] = c("overall")
rownames(s6_sum) = c("l_turnover","labour quantity","energy intensity","TFP")

# results export ----

#AI - full sample 

AI_full = rbind(
rbind(
 did_estimates(cdid, "l_turnover", m4),
 did_estimates(cdid, "l_wages", m4),
 did_estimates(cdid, "l_labor_quantity", m4),
 did_estimates(cdid, "energy_intensity", m4),
 did_estimates(cdid, "TFP", m4),
 did_estimates(cdid, "BEDRLST341000", m4),
 did_estimates(cdid, "energy_expenditure_d", m4)) %>% mutate(estimm = "chain"),
#csa
rbind(
  did_estimates(csa_did, "l_turnover", m4),
  did_estimates(csa_did, "l_wages", m4),
  did_estimates(csa_did, "l_labor_quantity", m4),
  did_estimates(csa_did, "energy_intensity", m4),
  did_estimates(csa_did, "TFP", m4),
  did_estimates(csa_did, "BEDRLST341000", m4),
  did_estimates(csa_did, "energy_expenditure_d", m4)) %>% mutate(estimm = "CSA"),
#covariate
rbind(
  did_estimates(cdid_cov, "l_turnover", m4),
  did_estimates(cdid_cov, "l_wages", m4),
  did_estimates(cdid_cov, "l_labor_quantity", m4),
  did_estimates(cdid_cov, "energy_intensity", m4),
  did_estimates(cdid_cov, "TFP", m4),
  did_estimates(cdid_cov, "BEDRLST341000", m4),
  did_estimates(cdid_cov, "energy_expenditure_d", m4)) %>% mutate(estimm = "chain_covariate"))
# AI-as production
AI_prod_full = rbind(
rbind(
  did_estimates(cdid, "l_turnover", m6),
  did_estimates(cdid, "l_wages", m6),
  did_estimates(cdid, "l_labor_quantity", m6),
  did_estimates(cdid, "energy_intensity", m6),
  did_estimates(cdid, "TFP", m6),
  did_estimates(cdid, "BEDRLST341000", m6),
  did_estimates(cdid, "energy_expenditure_d", m6)) %>% mutate(estimm = "chain"),
#csa
rbind(
  did_estimates(csa_did, "l_turnover", m6),
  did_estimates(csa_did, "l_wages", m6),
  did_estimates(csa_did, "l_labor_quantity", m6),
  did_estimates(csa_did, "energy_intensity", m6),
  did_estimates(csa_did, "TFP", m6),
  did_estimates(csa_did, "BEDRLST341000", m6),
  did_estimates(csa_did, "energy_expenditure_d", m6)) %>% mutate(estimm = "CSA"),
#covariate
rbind(
  did_estimates(cdid_cov, "l_turnover", m6),
  did_estimates(cdid_cov, "l_wages", m6),
  did_estimates(cdid_cov, "l_labor_quantity", m6),
  did_estimates(cdid_cov, "energy_intensity", m6),
  did_estimates(cdid_cov, "TFP", m6),
  did_estimates(cdid_cov, "BEDRLST341000", m6),
  did_estimates(cdid_cov, "energy_expenditure_d", m6)) %>% mutate(estimm = "chain_covariate"))

#Only observed
AI_observed = rbind(
  rbind(
    did_estimates(cdid, "l_turnover", m5),
    did_estimates(cdid, "l_wages", m5),
    did_estimates(cdid, "l_labor_quantity", m5),
    did_estimates(cdid, "energy_intensity", m5),
    did_estimates(cdid, "TFP", m5),
    did_estimates(cdid, "BEDRLST341000", m5),
    did_estimates(cdid, "energy_expenditure_d", m5)) %>% mutate(estimm = "chain"),
  #csa
  rbind(
    did_estimates(csa_did, "l_turnover", m5),
    did_estimates(csa_did, "l_wages", m5),
    did_estimates(csa_did, "l_labor_quantity", m5),
    did_estimates(csa_did, "energy_intensity", m5),
    did_estimates(csa_did, "TFP", m5),
    did_estimates(csa_did, "BEDRLST341000", m5),
    did_estimates(csa_did, "energy_expenditure_d", m5)) %>% mutate(estimm = "CSA"),
  #covariate
  rbind(
    did_estimates(cdid_cov, "l_turnover", m5),
    did_estimates(cdid_cov, "l_wages", m5),
    did_estimates(cdid_cov, "l_labor_quantity", m5),
    did_estimates(cdid_cov, "energy_intensity", m5),
    did_estimates(cdid_cov, "TFP", m5),
    did_estimates(cdid_cov, "BEDRLST341000", m5),
    did_estimates(cdid_cov, "energy_expenditure_d", m5)) %>% mutate(estimm = "chain_covariate"))
#Only observed + AI-as production
AI_prod_observed = rbind(
  rbind(
    did_estimates(cdid, "l_turnover", m8),
    did_estimates(cdid, "l_wages", m8),
    did_estimates(cdid, "l_labor_quantity", m8),
    did_estimates(cdid, "energy_intensity", m8),
    did_estimates(cdid, "TFP", m8),
    did_estimates(cdid, "BEDRLST341000", m8),
    did_estimates(cdid, "energy_expenditure_d", m8)) %>% mutate(estimm = "chain"),
  #csa
  rbind(
    did_estimates(csa_did, "l_turnover", m8),
    did_estimates(csa_did, "l_wages", m8),
    did_estimates(csa_did, "l_labor_quantity", m8),
    did_estimates(csa_did, "energy_intensity", m8),
    did_estimates(csa_did, "TFP", m8),
    did_estimates(csa_did, "BEDRLST341000", m8),
    did_estimates(csa_did, "energy_expenditure_d", m8)) %>% mutate(estimm = "CSA"),
  #covariate
  rbind(
    did_estimates(cdid_cov, "l_turnover", m8),
    did_estimates(cdid_cov, "l_wages", m8),
    did_estimates(cdid_cov, "l_labor_quantity", m8),
    did_estimates(cdid_cov, "energy_intensity", m8),
    did_estimates(cdid_cov, "TFP", m8),
    did_estimates(cdid_cov, "BEDRLST341000", m8),
    did_estimates(cdid_cov, "energy_expenditure_d", m8)) %>% mutate(estimm = "chain_covariate"))

#sans 2020
AI_sans_20 = rbind(
  did_estimates(cdid, "l_turnover", m9),
  did_estimates(cdid, "l_labor_quantity", m9),
  did_estimates(cdid, "energy_intensity", m9),
  did_estimates(cdid, "TFP", m9)) %>% mutate(estimm = "chain")

#sans stopper
AI_sans_stopper = rbind(
  did_estimates(cdid, "l_turnover", m11),
  did_estimates(cdid, "l_labor_quantity", m11),
  did_estimates(cdid, "energy_intensity", m11),
  did_estimates(cdid, "TFP", m11)) %>% mutate(estimm = "chain")
#sans wholesale
AI_sans_wholesale = rbind(
  did_estimates(cdid, "l_turnover", m7),
  did_estimates(cdid, "l_labor_quantity", m7),
  did_estimates(cdid, "energy_intensity", m7),
  did_estimates(cdid, "TFP", m7)) %>% mutate(estimm = "chain")

# parallel trends power ----

power_slope_res = cbind(
rbind(
  power_slope("l_turnover", m4),
  power_slope("l_labor_quantity", m4),
  power_slope("energy_intensity", m4),
  power_slope("TFP", m4)),
#Only observed
  rbind(
  power_slope("l_turnover", m5),
  power_slope("l_labor_quantity", m5),
  power_slope("energy_intensity", m5),
  power_slope("TFP", m5)))

colnames(power_slope_res) = c("power_50_full", "power_80_full", "power_50_observed", "power_80_observed")
rownames(power_slope_res) = c("l_turnover","labour quantity","energy intensity","TFP")
# selection and parallel trends ----

m4_control = m4[m4$treatment_group == 0, ]

m4_control$l_turnover_demeaned = m4_control$l_turnover - mean(m4_control$l_turnover)
m4_control$l_labor_quantity_demeaned = m4_control$l_labor_quantity - mean(m4_control$l_labor_quantity)
m4_control$energy_intensity_demeaned = m4_control$energy_intensity - mean(m4_control$energy_intensity)
m4_control$TFP_demeaned = m4_control$TFP - mean(m4_control$TFP)

selection_reg = rbind(
output_reg(plm(l_turnover_demeaned ~ lag(l_turnover_demeaned), data = m4_control, model = "random", index = "id")),
output_reg(plm(l_labor_quantity_demeaned ~ lag(l_labor_quantity_demeaned), data = m4_control, model = "random", index = "id")),
output_reg(plm(energy_intensity_demeaned ~ lag(energy_intensity_demeaned), data = m4_control, model = "random", index = "id")),
output_reg(plm(TFP_demeaned ~ lag(TFP_demeaned), data = m4_control, model = "random", index = "id")))

# write data ----

#1
write.csv(s1, file= "output/output_02_04_2025/output_01_smd.csv")
#2
write.csv(s2, file= "output/output_02_04_2025/output_02_frequency_from_1.csv")
#3
write.csv(s3, file= "output/output_02_04_2025/output_03_frequency_treatment_groups.csv")
#4
write.csv(s4, file= "output/output_02_04_2025/output_04_mean_all.csv")
write.csv(s4_max, file= "output/output_02_04_2025/background/output_04_mean_max.csv")
write.csv(s4_sum, file= "output/output_02_04_2025/background/output_04_mean_sum.csv")
#5
write.csv(s5, file= "output/output_02_04_2025/output_05_mean_treat.csv")
write.csv(s5_max, file= "output/output_02_04_2025/background/output_05_mean_treat_max.csv")
write.csv(s5_sum, file= "output/output_02_04_2025/background/output_05_mean_treat_sum.csv")
#6
write.csv(s6, file= "output/output_02_04_2025/output_06_mean_notreat.csv")
write.csv(s6_max, file= "output/output_02_04_2025/background/output_06_mean_notreat_max.csv")
write.csv(s6_sum, file= "output/output_02_04_2025/background/output_06_mean_notreat_sum.csv")
#7
write.csv(AI_full, file= "output/output_02_04_2025/output_07_did.csv")
#8
write.csv(AI_prod_full, file= "output/output_02_04_2025/output_08_did.csv")
#9
write.csv(AI_observed, file= "output/output_02_04_2025/output_09_did.csv")
#10

write.csv(AI_prod_observed, file= "output/output_02_04_2025/output_10_did.csv")
#11
write.csv(AI_sans_20, file= "output/output_02_04_2025/output_11_did.csv")
#12
write.csv(AI_sans_stopper, file= "output/output_02_04_2025/output_12_did.csv")
#13
write.csv(AI_sans_wholesale, file= "output/output_02_04_2025/output_13_did.csv")
#14
write.csv(power_slope_res, file= "output/output_02_04_2025/output_14_power_analysis.csv")
#15
write.csv(selection_reg, file= "output/output_02_04_2025/output_15_autocorrelation.csv")
