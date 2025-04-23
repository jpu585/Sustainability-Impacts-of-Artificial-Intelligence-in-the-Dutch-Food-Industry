library(Hmisc)
library(data.table)
library(panelView)

setwd("H:/project_1")

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

data_dir = "H:/project_1/Data/raw1/PS_I"
files    = list.files(data_dir, full.names = T)

df = spss.get(files[9], use.value.labels = F)


variable_names = names(df)


variables_interest = c("BE.Id", "STATJAAR", "sbi", "kerncelcode", "RechtsvormID", "gk.SBS", "EindGewicht", "CorrectieGewicht", "Correctiefactor.DRT", "ImputatieGebruiken", "IsCongo", "IsDummy",  "IsGeblokkeerd", "IsNullTeller", "IsUitbijter", "AFSCHRG110000", "BEDRLST310000", "BEDRLST341000", "FINREST100000", "INKWRDE100000", "INVESTN130000", "LOONSOM110000", "LOONSOM110012", "OMZETPS210000","PERSLST100000", "PERSONS110000", "PERSONS100000")


check_results = lapply(files, check_variables, variables_interest)

check_results[1] #manually check for missing variables

a = check_results[10]
# a[1] 
# a[2] 
# a[3] 
# a[4] 
# a[5] 
# a[6] 
# a[7] 


a$missing
manual_mapping = data.table(
  year = c(2014, 2014, 2015 , 2016, 2017, 2018, 2019, 2020, 2023),
  #, 2014, 2014, 2015, 2015 , 2016, 2016, 2017, 2017, 2018, 2018),
  inconsistent_name = c("rechtsvormid",  "LOONSOM110002", "LOONSOM110002", "LOONSOM110002", "LOONSOM110002", "LOONSOM110002", "LOONSOM110002", "LOONSOM110002","BE.ID"),
  #, "BEDRLST342000", "BEDRLST343000", "BEDRLST342000", "BEDRLST343000", "BEDRLST342000", "BEDRLST343000", "BEDRLST342000", "BEDRLST343000", "BEDRLST342000", "BEDRLST343000"),
  consistent_name = c("RechtsvormID","LOONSOM110000", "LOONSOM110000", "LOONSOM110000", "LOONSOM110000", "LOONSOM110000", "LOONSOM110000", "LOONSOM110000", "BE.Id"))
  #, "BEDRLST342001", "BEDRLST343001","BEDRLST342001", "BEDRLST343001","BEDRLST342001", "BEDRLST343001","BEDRLST342001", "BEDRLST343001","BEDRLST342001", "BEDRLST343001"))
  
panel_ps_I = rbindlist(lapply(files, standardiz, mapping = manual_mapping), fill = T, ignore.attr = T)


sink(file = "statistics/describe_PS_I.txt")
describe(panel_ps_I)
sink(file = NULL)

fwrite(panel_ps_I, "Data/cleaned/long_ps_I.csv")


data_dir = "H:/project_1/Data/raw1/PS_G"
files    = list.files(data_dir, full.names = T)



sink(file = "code/ps_22.txt")
variable_names = names(df)
print(variable_names)
sink(file = NULL)


check_results = lapply(files, check_variables, variables_interest)

check_results[5] #manually check for missing variables

a = check_results[1:9]
a[[6]]$missing

panel_ps_G = rbindlist(lapply(files, standardiz, mapping = manual_mapping), fill = T, ignore.attr = T)


sink(file = "statistics/describe_PS_G.txt")
describe(panel_ps_G)
sink(file = NULL)



fwrite(panel_ps_G, "Data/cleaned/long_ps_G.csv")

panel_c = rbind(panel_ps_I, panel_ps_G)

sink(file = "statistics/describe_PS_c.txt")
describe(panel_c)
sink(file = NULL)



fwrite(panel_c, "Data/cleaned/long_ps_C.csv")

panel_c = as.data.table(panel_c)
panel_c= panel_c[!duplicated(panel_c[, c("BE.Id","year")]), ]

rm(list = ls(all = T))
