### read in and save files which can be used to map UTLAs from different years

library(curl)
library(magrittr)
library(dplyr)
library(data.table)
library(readxl)

### 2019 to 2020

mapping_utla_2019_2020 <- read_excel("data-raw/local authority lookups/LA2019 to LA2020 mapping.xlsx")

### 2020 to 2021/2022

mapping_utla_2020_2022 <- read_excel("data-raw/local authority lookups/LA2020 to LA2022 mapping.xlsx")



########### write out mapping files

usethis::use_data(mapping_utla_2019_2020, overwrite = TRUE)
usethis::use_data(mapping_utla_2020_2022, overwrite = TRUE)
