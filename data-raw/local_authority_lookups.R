library(curl)
library(magrittr)
library(dplyr)
library(data.table)
library(stringr)

#### All lookups obtained from the Open Geography portal

## April 2023 onwards

utla_2023 <- read.csv("data-raw/local authority lookups/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(April_2023)_Lookup_in_England_and_Wales.csv") %>% setDT()

utla_2023 <- utla_2023 %>%
  select(UTLA23CD, UTLA23NM) %>%
  distinct() %>%
  filter(substr(UTLA23CD,1,1) == "E")

usethis::use_data(utla_2023, overwrite = TRUE)

## April 2021 - March 2023

utla_2021_23 <- read.csv("data-raw/local authority lookups/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2022)_Lookup_in_England_and_Wales.csv") %>% setDT()

utla_2021_23 <- utla_2021_23 %>%
  select(UTLA22CD, UTLA22NM) %>%
  distinct() %>%
  filter(substr(UTLA22CD,1,1) == "E")

usethis::use_data(utla_2021_23, overwrite = TRUE)

## April 2020 - March 2021

utla_2020_21 <- read.csv("data-raw/local authority lookups/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2020)_Lookup_in_England_and_Wales.csv") %>% setDT()

utla_2020_21 <- utla_2020_21 %>%
  select(UTLA20CD, UTLA20NM) %>%
  distinct() %>%
  filter(substr(UTLA20CD,1,1) == "E")

usethis::use_data(utla_2020_21, overwrite = TRUE)

## April 2019 - March 2020

utla_2019_20 <- read.csv("data-raw/local authority lookups/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2019)_Lookup_in_England_and_Wales.csv") %>% setDT()

utla_2019_20 <- utla_2019_20 %>%
  select(UTLA19CD, UTLA19NM) %>%
  distinct() %>%
  filter(substr(UTLA19CD,1,1) == "E")

usethis::use_data(utla_2019_20, overwrite = TRUE)

## Pre 2019 (toolkit data is using 2012. create a 2012 - 2019 lookup)

ltla_2012_to_utla_2019 <- read.csv("data-raw/local authority lookups/LTLA to UTLA England and Wales 2012 2019.csv") %>% setDT()

ltla_2012_to_utla_2019 <- ltla_2012_to_utla_2019 %>%
  filter(substr(UTLA19CD,1,1) == "E")


usethis::use_data(ltla_2012_to_utla_2019, overwrite = TRUE)


############################
### Check UTLA comparison ##

LA2019 <- smkfreediv2::utla_2019_20
LA2020 <- smkfreediv2::utla_2020_21
LA2021 <- smkfreediv2::utla_2021_23
LA2023 <- smkfreediv2::utla_2023

### --------- 2019/20 to 2020/21 ----------- ##

merge_19_20 <- merge(LA2019, LA2020, by.x = "UTLA19CD", by.y = "UTLA20CD", all = TRUE)

### only difference between 2019 and 2020 UTLAs is Buckinghamshire code changes from
### "E10000002" in 2019 to "E06000060" in 2020

write.csv(LA2020,"data-raw/local authority lookups/LA2020.csv")

### --------- 2020/21 to 2021/22, 2022/23 ----------- ##

merge_20_21 <- merge(LA2020, LA2021, by.x = "UTLA20CD", by.y = "UTLA22CD", all = TRUE)

### difference is that E10000021 Northamptonshire is split into:
### E06000061 North Northamptonshire
### E06000062 West Northamptonshire

### to map from 2020 to 2022 use mid-year pop estimates for 2021.
### North = 182968 (45.92443%), West = 215443 (54.07557%)

write.csv(LA2021,"data-raw/local authority lookups/LA2021.csv")

### --------- 2022/23 to 2023 onwards ----------- ##

merge_21_23 <- merge(LA2021, LA2023, by.x = "UTLA22CD", by.y = "UTLA23CD", all = TRUE)

### Added local authorities:
### E06000063 Cumberland
### E06000064 Westmorland and Furness
### E06000065 North Yorkshire
### E06000066 Somerset

### Removed local authorities
### E10000006 Cumbria
### E10000023 North Yorkshire
### E10000027 Somerset



