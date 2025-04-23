#Import data from Microdata to local directory Data/raw1/
#namely ~/PS_I  ~/PS_G, ~/ICT and ~/ABR from 2014 until 2023
#Data/manual/price indicees is manually downloaded see script for more information

#set working directory
setwd("H:/project_1")

#Run all data import files
source("code/01_import_PS.R")
source("code/02_import_ICT.R")
source("code/03_import_ABR.R")
source("code/04_import_ICT.R")

#match observation from different data sources
#create composite variables and treatment indicators
source("code/05_matching.R")

#creates did functions and datasets that can be directly used by the functions
source("code/06_functions_and_data.R")
#creates output
source("code/07_create_export_files.R")