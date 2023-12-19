
#### get GOR to 2022 local authority lookup from fingertips

library(data.table)
library(dplyr)
library(magrittr)

region_to_utla <- fingertipsR::fingertips_data(IndicatorID = 92443,
                                     AreaTypeID = 402,
                                     ParentAreaTypeID = 6) %>%
  filter(Age == "18+ yrs" &
           Sex == "Persons" &
           AreaType == "Counties & UAs (2021/22-2022/23)" &
           Timeperiod == 2021) %>%
  select(ParentName,AreaCode,AreaName) %>%
  rename(region = ParentName,
         UTLA22CD = AreaCode,
         UTLA22NM = AreaName)

usethis::use_data(region_to_utla, overwrite = TRUE)

