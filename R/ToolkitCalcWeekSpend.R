#' Weekly Tobacco Spend
#'
#' Generates weekly spending on tobacco as a weighted mean across individuals in the sample.
#'
#' @param data Data table. The cleaned toolkit data.
#' @param upshift Numeric. Parameter to upshift expenditures in the toolkit data to account for under-reporting.
#'
#' @importFrom data.table := copy rbindlist setDT
#'
#' @return mean and median weekly spending and standard errors by subgroup
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
ToolkitCalcWeekSpend <- function(data,
                                 upshift = 1.417) {

  mean_calcs <- copy(data)

  ### missing expenditure as 0

  #mean_calcs[is.na(weekspend) & Smoker == 1, weekspend := 0]

  ### apply upshift factor to mean weekly spending
  mean_calcs[, weekspend := weekspend * upshift]
  mean_calcs[, weekspend_real := weekspend_real * upshift]

  ### only keep observations which are not missing for strat_vars

  for(cv in "UTLA19CD") {

    if(cv %in% colnames(mean_calcs)) {
      mean_calcs <- mean_calcs[!is.na(get(cv))]
    } else {
      warning(cv, " not a column in the data")
    }

  }
  ### calculate the mean

  mean_calcs[, sample := ifelse(is.na(weekspend), 0, 1)]

  ### merge in the 2019-2020, and 2020-2022 local authority mapping weights

  mean_calcs <- merge(mean_calcs, smkfreediv2::mapping_utla_2019_2020, by = c("UTLA19CD"))
  mean_calcs <- merge(mean_calcs, smkfreediv2::mapping_utla_2020_2022, by = c("UTLA20CD"), allow.cartesian = TRUE)


  suppressWarnings({
    data_out <- mean_calcs[, .(mean_week_spend      = Hmisc::wtd.mean(weekspend, w = Aweight0*wght_19_to_20*wght_20_to_22 ,na.rm = T),
                               se_week_spend        = sqrt(Hmisc::wtd.var(weekspend, w = Aweight0*wght_19_to_20*wght_20_to_22 ,na.rm = T)) / sqrt(.N),
                               median_week_spend    = median(weekspend, na.rm = T),
                               se_median_week_spend = 1.253*sqrt(Hmisc::wtd.var(weekspend, w = Aweight0*wght_19_to_20*wght_20_to_22 ,na.rm = T)) / sqrt(.N),
                               ### real terms spending
                               mean_week_spend_real      = Hmisc::wtd.mean(weekspend_real, w = Aweight0*wght_19_to_20*wght_20_to_22 ,na.rm = T),
                               se_week_spend_real        = sqrt(Hmisc::wtd.var(weekspend_real, w = Aweight0*wght_19_to_20*wght_20_to_22 ,na.rm = T)) / sqrt(.N),
                               median_week_spend_real    = median(weekspend_real, na.rm = T),
                               se_median_week_spend_real = 1.253*sqrt(Hmisc::wtd.var(weekspend_real, w = Aweight0*wght_19_to_20*wght_20_to_22 ,na.rm = T)) / sqrt(.N),
                               ### sample size
                               sample_tkit          = sum(sample*wght_19_to_20*wght_20_to_22) ),by = c("UTLA22CD")]

  })

  data_out <- data_out[!is.nan(mean_week_spend),]

  return(data_out)
}
