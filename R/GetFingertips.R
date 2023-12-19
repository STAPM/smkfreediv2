#' Get Smoking Prevalence Data
#'
#' A function which uses the fingertipsR package to obtain and clean data on
#' smoking prevalence by local authority from the OHID local tobacco profiles. By default
#' data are returned for the upper tier local authority boundaries as at December 2019.
#'
#' @param year Numeric. year of smoking prevalence data to obtain.
#'
#' @return A data frame of smoking prevalence by UTLA for a single year
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
#'
GetFingertips <- function(year = 2022){

  ############################################################################
  ## extract the data from fingertips. Identifier 92443 for the indicator:
  ## "Smoking Prevalence in adults (18+) - current smokers (APS)"

  ######## 2019 local authority boundaries

  if (year %in% 2019){

    data <- fingertipsR::fingertips_data(IndicatorID = 92443,
                                         AreaTypeID = 202,
                                         ParentAreaTypeID = 6) %>%
      filter(Age == "18+ yrs" &
               Sex == "Persons" &
               AreaType == "Counties & UAs (2019/20)" &
               Timeperiod == year) %>%
      select(AreaCode,Value,UpperCI95.0limit,LowerCI95.0limit) %>%
      rename(UTLA19CD = AreaCode,
             smk_prev = Value,
             smk_prev_uci = `UpperCI95.0limit`,
             smk_prev_lci = `LowerCI95.0limit`)

    ### convert to 2020 local authorities

    data <- left_join(data, smkfreediv2::mapping_utla_2019_2020, by = c("UTLA19CD")) %>%
      group_by(UTLA20CD) %>%
      summarise(smk_prev = weighted.mean(smk_prev, w = wght_19_to_20, na.rm = TRUE),
                smk_prev_uci = weighted.mean(smk_prev_uci, w = wght_19_to_20, na.rm = TRUE),
                smk_prev_lci = weighted.mean(smk_prev_lci, w = wght_19_to_20, na.rm = TRUE)) %>%
      ungroup()

    ### convert to 2022 local authorities

    data <- left_join(data, smkfreediv2::mapping_utla_2020_2022, by = c("UTLA20CD")) %>%
      group_by(UTLA22CD) %>%
      summarise(smk_prev = weighted.mean(smk_prev, w = wght_20_to_22),
                smk_prev_uci = weighted.mean(smk_prev_uci, w = wght_20_to_22),
                smk_prev_lci = weighted.mean(smk_prev_lci, w = wght_20_to_22)) %>%
      ungroup()

    pop_size <- select(smkfreediv2::population_counts, c("UTLA22CD","UTLA22NM","population_2019")) %>%
      rename(pop = population_2019)
  }

  ######## 2020 local authority boundaries

  if (year %in% 2020){

    data <- fingertipsR::fingertips_data(IndicatorID = 92443,
                                         AreaTypeID = 302,
                                         ParentAreaTypeID = 6) %>%
      filter(Age == "18+ yrs" &
               Sex == "Persons" &
               AreaType == "Counties & UAs (2020/21)" &
               Timeperiod == year) %>%
      select(AreaCode,Value,UpperCI95.0limit,LowerCI95.0limit) %>%
      rename(UTLA20CD = AreaCode,
             smk_prev = Value,
             smk_prev_uci = `UpperCI95.0limit`,
             smk_prev_lci = `LowerCI95.0limit`)

    ### convert to 2022 local authorities

    data <- left_join(data, smkfreediv2::mapping_utla_2020_2022, by = c("UTLA20CD")) %>%
      group_by(UTLA22CD) %>%
      summarise(smk_prev = weighted.mean(smk_prev, w = wght_20_to_22),
                smk_prev_uci = weighted.mean(smk_prev_uci, w = wght_20_to_22),
                smk_prev_lci = weighted.mean(smk_prev_lci, w = wght_20_to_22)) %>%
      ungroup()

    pop_size <- select(smkfreediv2::population_counts, c("UTLA22CD","UTLA22NM","population_2020")) %>%
      rename(pop = population_2020)
  }

  ######## 2021/2022 local authority boundaries

  if (year %in% c(2021,2022)){

  data <- fingertipsR::fingertips_data(IndicatorID = 92443,
                                       AreaTypeID = 402,
                                       ParentAreaTypeID = 6) %>%
    filter(Age == "18+ yrs" &
             Sex == "Persons" &
             AreaType == "Counties & UAs (2021/22-2022/23)" &
             Timeperiod == year)%>%
    select(AreaCode,Value,UpperCI95.0limit,LowerCI95.0limit) %>%
    rename(UTLA22CD = AreaCode,
           smk_prev = Value,
           smk_prev_uci = `UpperCI95.0limit`,
           smk_prev_lci = `LowerCI95.0limit`)

  if (year == 2021){
  pop_size <- select(smkfreediv2::population_counts, c("UTLA22CD","UTLA22NM","population_2021")) %>%
    rename(pop = population_2021)
  } else if (year == 2022){
  pop_size <- select(smkfreediv2::population_counts, c("UTLA22CD","UTLA22NM","population_2022")) %>%
    rename(pop = population_2022)
  }


   }

  ## Match in population size by local authority and calculate the number of smokers.
  ## merge in the lookup table to make sure all local authorities are represented including
  ## those with missing values in the tobacco profiles data


  ## Do a full join as fingertips may be missing some local authorities

  data_merge <- full_join(data, pop_size, by = "UTLA22CD") %>%
    mutate(n_smokers = (smk_prev/100)*pop,
           n_smokers_lci = (smk_prev_lci/100)*pop,
           n_smokers_uci = (smk_prev_uci/100)*pop) %>%
    select(c("UTLA22CD","UTLA22NM","pop",
             "smk_prev","smk_prev_lci","smk_prev_uci",
             "n_smokers","n_smokers_lci","n_smokers_uci"))



  return(data_merge)

}
