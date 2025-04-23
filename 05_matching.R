# packages ----

library(Hmisc)
library(readr)
library(tidyverse)
library(lattice)
library(data.table)

#summary commands
# describe(abr)
# describe(ict)
# describe(ps)
# describe(m3$n.ai)
# describe(observed_units)

# import clean data -----
setwd("H:/project_1")
abr = read_csv("Data/cleaned/long_abr_panel.csv")
ict = read_csv("Data/cleaned/long_ict_panel.csv")
ps  = read_csv("Data/cleaned/long_ps_C.csv")
pi  = read_csv("Data/cleaned/price_indices.csv")

#change names
setnames(ps,  'BE.Id', 'be.id')
setnames(ps,  'sbi', 'sbi_ps')

ict_dub = ict[!duplicated(ict[, c("be.id","year")]), ]
ps_dub  = ps[!duplicated(ps[, c("be.id","year")]), ]



# merge ----

m1 = merge(ict, ps, by = c("be.id", "year"), all = T)
m2 = merge(m1, abr , by = c("be.id", "year"), all.x = T)
m2 = merge(m2, pi, by = "year", all.x = T)

# filter and create variables ----

# filter for relevant sector codes

relevant_sbi = c("100", "101","102","103","104","105","106","107","108", "109",
                 #food processing
                 "110", #drink processing
                 "120", #tobacco processing
                 "462", #Groothandel in Landbouwproducten
                 "463") #Groothandel in Voedings- en Genotmiddelen


m3 = m2[substr(m2$sbi_ps, 1, 3) %in% relevant_sbi, ] #sbi_ps

#covariates
#firms size
small_firms = c("10", "21", "22", "30", "40", "50", "60")
#large_firms = c("71", "72", "81", "82", "91", "92", "93")
m3$small_size = ifelse(m3$gk.SBS %in% small_firms, 1, 0)

#food processing
food_firms = c("100", "101","102","103","104","105","106","107","108", "109")
#large_firms = c("71", "72", "81", "82", "91", "92", "93")
m3$food_processing = ifelse(substr(m3$sbi_ps, 1, 3) %in% food_firms, 1, 0)


#create composite variables
m3 = as.data.table(m3)
m3[, capital_expenditure_d := AFSCHRG110000 / (capital_pi / 100)]
m3[, turnover_d := OMZETPS210000 / (food_cpi / 100) ]
m3[, l_turnover := log(turnover_d) ]
m3[, labour_expenditure_d := PERSLST100000 / (food_cpi / 100) ]
m3[, energy_expenditure_d := BEDRLST341000 / (energy_ppi / 100) ]
m3[, purchase_value_d := INKWRDE100000  / (food_ppi / 100) ]
m3[, TFP := turnover_d - (purchase_value_d + labour_expenditure_d)]
m3[, wages := labour_expenditure_d / PERSONS110000 ]
m3[, l_wages := log(wages) ]
m3[, energy_intensity := energy_expenditure_d / turnover_d ]
m3[, l_labor_quantity := log(PERSONS110000) ]


m3 = m3[!m3$turnover_d == 0,]
mi3 = m3[m3$labour_expenditure_d == 0,]
describe(mi3)
# change n.ai variable to discrete

m3$discrete_n.ai = with(m3, ifelse(n.ai > 0, 1, n.ai))

# discrete n.ai and ai.gebruikt

m3$ai = with(m3, 
             ifelse(is.na(discrete_n.ai) & is.na(ai.gebruikt), NA,
                    ifelse(is.na(discrete_n.ai), ai.gebruikt,
                           ifelse(is.na(ai.gebruikt), discrete_n.ai,ai.gebruikt))
             )
)


#AI production

m3$ai_prod = with(m3, 
                  ifelse(is.na(ai.productieproces) & is.na(artificial.intelligence.robots.procesautomatisering), NA,
                         ifelse(is.na(ai.productieproces), artificial.intelligence.robots.procesautomatisering,
                                ifelse(is.na(artificial.intelligence.robots.procesautomatisering), ai.productieproces, artificial.intelligence.robots.procesautomatisering))
                  ))
m3$ai_prod = with(m3, ifelse(is.na(ai_prod) & ai == 0, 0, ai_prod))
tapply(m3$ai_prod, m3$year, describe)

# filter units that are observed in at least once in the AI survey

observed_units = as.logical(ave(m3$ai, m3$be.id, 
                                FUN = function(x) !all(is.na(x))))
m4 = m3[observed_units, ]

# exclude duplicate firm year observation

m5 = m4[!duplicated(m4[, c("be.id","year")]), ]

# check command: length(unique(m5$be.id))

m5 = as.data.table(m5)


# create firm id

m5$id = 0
b = length(unique(m5$be.id))
for (i in 1:b){
  a = unique(m5$be.id)
  a = as.vector(a)
  m5$id[m5$be.id == a[i]] = i
}

#create treatment group


m5$treatment_group  = 
  ave(m5$ai * m5$year, #treatment time or 0 if not treated
      m5$be.id,           #by firm
      FUN = function(x){
        first_treatment = min(x[x > 0], na.rm = T)
        if(is.infinite(first_treatment)) 0 else first_treatment
      }
  )


m5$treat_off = 0
m5$treat_off =  ifelse(m5$treatment_group != 0 & m5$treatment_group < m5$year & (m5$ai == 0 & !is.na(m5$ai)), 1, 0)

#treatment group for ai production
m5$treatment_group_prod = 
  ave(m5$ai_prod * m5$year, #treatment time or 0 if not treated
      m5$be.id,           #by firm
      FUN = function(x){
        first_treatment = min(x[x > 0], na.rm = T)
        if(is.infinite(first_treatment)) 0 else first_treatment
      }
  )

# print(m1 %>% count(treatment_group_prod, year), n =40)
# m1 %>% count(treatment_group, year)



# m6 = m5
#create indicator for missing AI information, keeping all information prior to 2020
m7=m5
for (i in 1:nrow(m7)){
  m7$ai_1[i] = ifelse(2022 < m7$year[i]|m7$year[i] < 2020, 0, m7$ai[i])
}

# exclude observations with missing data
m7 = m7[!is.na(m7$ai_1), ]
m7 = m7[!is.nan(m7$wages), ]
m7 = m7[!is.na(m7$wages), ]
m7 = m7[!is.infinite(m7$wages), ]
m7 = m7[!is.na(m7$EindGewicht), ]
m7 = m7[EindGewicht >= 0]

describe(m7$stop_treat)

m7$ever_adopt = ave(m7$ai, m7$be.id, 
                    FUN = function(x) ifelse(!all(x == 0),1 , 0))
m7$ever_adopt = ave(m7$ever_adopt, m7$be.id, 
                    FUN = function(x) ifelse(is.na(x), 0, 1))
m7$ever_treat_off = ave(m7$treat_off, m7$be.id, 
                    FUN = function(x) ifelse(!all(x == 0),1 , 0))
# create outputs ----

fwrite(m7, "Data/cleaned/merged_data.csv")
fwrite(m3, "Data/cleaned/full_sample.csv")


sink(file = "statistics/describe_merged_data.txt")
describe(m7)
sink(file = NULL)
rm(list = ls(all = T))
