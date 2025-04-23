#library(cbsodataR)
library(readr)
library(data.table)

#cbs_get_data_from_link("https://opendata.cbs.nl/statline/#/CBS/nl/dataset/84142NED/table?ts=1736348912329")

capital_pi = read_delim("Data/manual/price indicees/Conjunctuurbeeld_per_bedrijfstak_08012025_161647.csv", 
              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
              trim_ws = TRUE)

#cbs_get_data_from_link("https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83131NED/table?dl=B3B56")

food_cpi = read_delim("Data/manual/price indicees/CPI__prijsindex_2015_100_08012025_162015.csv", 
            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
            trim_ws = TRUE)
#cbs_get_data_from_link("https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83936NED/table?dl=B3B61")

food_ppi = read_delim("Data/manual/price indicees/Producentenprijzen__SBI_2008__2015_100_08012025_163053.csv", 
             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
             trim_ws = TRUE)
#cbs_get_data_from_link("https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83935NED/table?dl=B3B66")

energy_ppi = read_delim("Data/manual/price indicees/PPI__naar_product__index_2015_100_08012025_164320.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                   trim_ws = TRUE)

t1 = cbind(capital_pi,
      food_cpi,
      food_ppi,
      energy_ppi)

t1 = t1[, c(2, 3, 7, 11, 15)]

t1 = setNames(t1, c("year", "capital_pi", "food_cpi", "food_ppi", "energy_ppi"))

t1$year = gsub("\\*", "", t1$year)

fwrite(t1, "Data/cleaned/price_indices.csv")

rm(list = ls(all = T))
