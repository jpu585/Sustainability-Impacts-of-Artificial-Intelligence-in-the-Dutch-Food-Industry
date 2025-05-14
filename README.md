# Sustainability-Impacts-of-Artificial-Intelligence-in-the-Dutch-Food-Industry

This repository DOI: (XXX) contains files that contribute to our supplementary materials of the Manuscript: 'Sustainability Impacts of Artificial Intelligence in the Dutch Food Industry' by Jan-Philip R. Uhlemann, Alfons Oude Lansink, and Tobias Dalhaus (The DOI of the Manuscript can be found here when published). Within this repository we provide all necessary files to comprehend the procedure of code calculations. To automatically replicate the results run 00_master.R in a working directory with the other files. The study relies on confidential firm microdata, which can be requested at Statistics Netherlands (https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research). Statistics Netherlands also offers a replication service.

## Master file
00_master.R
This file runs the code scripts. To run the analysis it is necessary to import the Microdata to from the local directory Data/raw1/.

The data is saved in yearly files, the analysis uses data from 2014 until 2023. We need the production statistics files for the sectors industry and wholesale. The ICT survey and the company register. The files should be saved in the following relative directories: ~/PS_I  ~/PS_G, ~/ICT and ~/ABR. The price indicees are manually downloaded, more information is available in the corresponding script.

The analysis works in the following steps

# Runs all data import files

source("code/01_import_PS.R")

source("code/02_import_ICT.R")

source("code/03_import_ABR.R")

source("code/04_import_ICT.R")

# Matches observation from different data sources
source("code/05_matching.R")

Also creates composite variables and treatment indicators

# Creates and loads DiD functions and datasets
source("code/06_functions_and_data.R")

# Creates output
source("code/07_create_export_files.R")
