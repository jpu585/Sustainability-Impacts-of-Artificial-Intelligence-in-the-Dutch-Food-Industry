library(Hmisc)
library(data.table)
library(panelView)

setwd("H:/project_1")

data_dir = "H:/project_1/Data/raw1/ICT"
files    = list.files(data_dir, full.names = T)

df = spss.get(files[8], use.value.labels = F)
sink(file = "describe_ICT2022.txt")
describe(df)
sink(file = NULL)
variable_names = names(df)

print(variable_names)

variables_interest = c("be.id","totaal.werkzp", "ai.gebruikt", "ai.productieproces","artificial.intelligence.robots.procesautomatisering", "ai.robots.procesautomatisering", "n.ai", "sbi.digit1", "sbi", "sbi.2008", "PG.12", "PG.37")

#Step 4 check variables in file and return missing ones
check_variables = function(file, variables_interest) {
  #extract from filename
  year = as.numeric(gsub(".*?(\\d{4}).*", "\\1", basename(file)))
  
  #read file
  df = spss.get(file, use.value.labels = F)
  
  #extract variable names
  variable_names = names(df)
  
  #check for missing variables
  missing_var = setdiff(variables_interest, variable_names)
  
  #return lists
  list(
    year = year,
    missing = missing_var,
    all_vars = variable_names
  )
}

check_results = lapply(files, check_variables, variables_interest)

check_results[1] #manually check for artificial intelligence, ai, production, robots

a = check_results[1:9]
a[[9]]$missing
a[[6]]$all_vars
a[[6]]$year

# 2014: sbi.2008 PG.37 PG.12 + 2015, 2016 & 2017 2018
# 2019

manual_mapping = data.table(
  year = c(2014, 2016 #,2020
           , 2014, 2015, 2016, 2017, 2018),
  inconsistent_name = c("beid", "beid"#, "artificial.intelligence.robots.procesautomatisiering"
                        , "sbi.2008", "sbi.2008", "sbi.2008", "sbi.2008", "sbi.2008"),
  consistent_name = c("be.id", "be.id" #, "ai.productieproces"
                      , "sbi", "sbi", "sbi", "sbi", "sbi")
)

standardiz = function(file, mapping) {
  #extract year from name
  year = as.numeric(gsub(".*?(\\d{4}).*", "\\1", basename(file)))
  #import dataset
  df = spss.get(file, use.value.labels = F)
  #convert data
  dt = as.data.table(df)
  #apply mapping
  year_mapping = mapping[year == year]
  
  for (i in seq_len(nrow(year_mapping))) {
    if (year_mapping$inconsistent_name[i] %in% names(dt)) {
      setnames(dt, old = year_mapping$inconsistent_name[i], new = year_mapping$consistent_name[i])
    }
  }
  #create NA variables
  missing_var = setdiff(variables_interest, names(dt))
  if (length(missing_var) > 0) {
    for (var in missing_var) {
      dt[, (var) := NA]
    }
  }
  #only include relevant variables
  dt = dt[, ..variables_interest, with = F]
  #add year
  dt[, year := year]
  return(dt)
}
  
panel_ict = rbindlist(lapply(files, standardiz, mapping = manual_mapping), fill = T, ignore.attr = T)

fwrite(panel_ict, "Data/cleaned/long_ict_panel.csv")

sink(file = "statistics/describe_panel.txt")
describe(panel_ict)
sink(file = NULL)

tapply(panel_ict$n.ai, panel_ict$year, describe)
tapply(panel_ict$artificial.intelligence.robots.procesautomatisering, panel_ict$year, describe)

panel_ict = as.data.table(panel_ict)
panel_ict = panel_ict[!duplicated(panel_ict[, c("be.id","year")]), ]

rm(list = ls(all = T))
