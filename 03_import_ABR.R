library(Hmisc)
library(data.table)
library(panelView)

setwd("H:/project_1")

data_dir = "H:/project_1/Data/raw1/ABR"
files    = list.files(data_dir, full.names = T)

df = spss.get(files[9], use.value.labels = F)

variable_names = names(df)

print(variable_names)

variables_interest = c("be.id","gemcode")

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

check_results[2:10] #manually check for artificial intelligence, ai, production, robots

a = check_results[1:6]
a[[6]]$missing

manual_mapping = data.table(
  year = c( 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  inconsistent_name = c("BE.ID", "BE.ID","BE.ID","BE.ID","BE.ID","BE.ID","BE.ID","BE.ID", "BE.ID","BE.ID"),
  consistent_name = c("be.id", "be.id", "be.id","be.id","be.id","be.id","be.id","be.id", "be.id", "be.id")
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

panel_abr = rbindlist(lapply(files, standardiz, mapping = manual_mapping), fill = T, ignore.attr = T)

fwrite(panel_abr, "Data/cleaned/long_abr_panel.csv")
