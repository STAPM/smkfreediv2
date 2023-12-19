library(curl)
library(magrittr)
library(dplyr)
library(tidyr)

###############################################################
### Population counts by local authority (all in 2022 UTLAs)

##################################
### 2000 - 2020 population

## spreadsheet saved in data-raw and obtained from :
## https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland


### read in lookup table from LTLA to UTLA

utla_2020_21 <- read.csv("data-raw/local authority lookups/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2020)_Lookup_in_England_and_Wales.csv") %>%
  filter(substr(UTLA20CD,1,1) == "E") %>%
  select(LTLA20CD,UTLA20CD,UTLA20NM)


### read in population data and get to LTLA level

data <- read.csv("data-raw/population estimates/MYEB1_detailed_population_estimates_series_UK_(2020_geog20).csv") %>%
  filter(age >= 18 & country == "E") %>%
  rename(LTLA20CD = ladcode20) %>%
  left_join(utla_2020_21, by = "LTLA20CD") %>%
  group_by(UTLA20CD,UTLA20NM) %>%
  summarise(population_2015 = sum(population_2015),
            population_2016 = sum(population_2016),
            population_2017 = sum(population_2017),
            population_2018 = sum(population_2018),
            population_2019 = sum(population_2019),
            population_2020 = sum(population_2020)) %>%
  ungroup()

### map to the 2022 UTLAs

pop_data_2015_20 <- merge(data, smkfreediv2::mapping_utla_2020_2022,
                          by = c("UTLA20CD","UTLA20NM")) %>%
  group_by(UTLA22CD,UTLA22NM) %>%
  summarise(population_2015 = sum(population_2015*wght_20_to_22),
            population_2016 = sum(population_2016*wght_20_to_22),
            population_2017 = sum(population_2017*wght_20_to_22),
            population_2018 = sum(population_2018*wght_20_to_22),
            population_2019 = sum(population_2019*wght_20_to_22),
            population_2020 = sum(population_2020*wght_20_to_22)) %>%
  ungroup()

rm(data,utla_2020_21)

##########################
### 2021 population

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2021/ukpopestimatesmid2021on2021geographyfinal.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

pop_data_2021 <- read_excel(temp, sheet = "MYE2 - Persons", range = "A8:CQ428") %>%
  rename(UTLA22CD = Code) %>%
  right_join(smkfreediv2::utla_2021_23, by = "UTLA22CD") %>%
  select(-c(Name,Geography,`All ages`,`0`,`1`,`2`,`3`,`4`,`5`,
            `6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`)) %>%
  pivot_longer(cols = -c("UTLA22CD","UTLA22NM"), names_to = "age", values_to = "population_2021") %>%
  group_by(UTLA22CD,UTLA22NM) %>%
  summarise(population_2021 = sum(population_2021)) %>%
  ungroup()


#######################################
#### combine datasets #################

population_counts <- full_join(pop_data_2015_20,pop_data_2021, by = c("UTLA22CD","UTLA22NM"))

## placehold - set 2022 population equal to 2021 until data is available

population_counts <- mutate(population_counts, population_2022 = population_2021)

usethis::use_data(population_counts, overwrite = TRUE)

