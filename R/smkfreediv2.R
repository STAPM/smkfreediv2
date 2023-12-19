#' Smoke-free Dividend
#'
#' Wrapper function for obtaining data, calculating the smoke-free dividend for England local areas, defined
#' as the 152 upper-tier local authorities in 2022, and simulating the uncertainty around the estimates.
#'
#' @param year Numeric. year of smoking prevalence data to use in the calculations.
#' @param read_toolkit Logical. If TRUE, read in raw toolkit SPSS data, if FALSE use toolkit data already created using `ToolkitRead()`.
#'                     If option is set to FALSE, the toolkit data will be read from the directory set by `out_path` and use the file
#'                     name specified by `name`.
#' @param in_path Character. path to the directory where the raw SPSS data is saved.
#' @param out_path Character. Path to the directory where the working version of the data in RDS format will be exported.
#' @param data name of the raw toolkit data file (defaults to the April 2021 version).
#' @param name Character. Name of the file to export.
#' @param start_month Numeric. Toolkit tart month for the analysis period (default = 90, April 2014).
#' @param end_month Numeric. Toolkit end month for the analysis period (default = 160, February 2020).
#' @param index Data table. Tobacco price data to use in the deflation. Default is the ONS tobacco CPI series.
#' @param base_month Numeric. Month of the year to deflate currency variables to. (Default = 12).
#' @param base_year Numeric. Year to deflate currency variables to. (Default = 2018).
#' @param upshift Numeric. Parameter to upshift expenditures calculated from the toolkit data.
#' @param div Numeric. Proportion of licit tobacco expenditure which is smoke-free dividend.
#' @param n_sim Numeric. Number of simulations.
#' @param seed Numeric. Random number seed.
#' @param illicit_prop Numeric. The proportion of total tobacco expenditure which is illicit.
#'
#' @return Smoke-free dividend calculations with simulated uncertainty
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
smkfreediv2 <- function(year = 2022,
                        read_toolkit = TRUE,
                        in_path,
                        out_path,
                        data,
                        name,
                        start_month = 90,
                        end_month = 160,
                        index = smkfreediv2::cpi_tobacco,
                        base_month = 12,
                        base_year = 2018,
                        upshift = 1.417,
                        div = 0.93,
                        illicit_prop = 1298 / (14307 + 1298),
                        n_sim = 100,
                        seed = 42){

  cat(crayon::bold(crayon::underline(crayon::green("Calculating the smoke-free dividend for England\n\n"))))

  start_time <- Sys.time()

  ### Get toolkit data and calculate local authority mean spending

  cat(crayon::blue(crayon::underline("\tReading Toolkit data and calculating spending")))


  if (read_toolkit == TRUE){

    toolkit_raw <- ToolkitRead(in_path = in_path,
                               out_path = out_path,
                               data = data,
                               save = FALSE,
                               name = name)

  } else {

    toolkit_raw <- readRDS(paste0(out_path,"/", name, ".rds"))


  }

  toolkit_clean <- ToolkitClean(data = toolkit_raw,
                                start_month = start_month,
                                end_month = end_month)

  week_spend <- ToolkitCalcWeekSpend(data = toolkit_clean,
                                     upshift = upshift)


  cat(crayon::bold("\tdone\n"))

  ### Get prevalence data for the selected year

  cat(crayon::blue(crayon::underline("\tDownloading smoking prevalence data")))

  prevalence_data <- GetFingertips(year = year)

  cat(crayon::bold("\tdone\n"))

  ### Calculate the dividend and simulate the uncertainty

  ### merge spending with tobacco profiles to calculate the dividend

  cat(crayon::blue(crayon::underline("\tCalculating smoke-free dividend and simulating uncertainty\n")))


  dividend <- DividendCalc_sim(data_spend = week_spend,
                               data_prevalence = prevalence_data,
                               upshift = upshift,
                               div = div,
                               illicit_prop = illicit_prop,
                               n_sim = n_sim,
                               seed = seed)

  cat(crayon::bold("\tdone\n"))

  #######################
  ## Record time taken

  end_time <- Sys.time()

  tdiff <- difftime(end_time, start_time, units = "mins")

  time <- paste0("\nComplete.\n\nTotal processing time: ", round(tdiff,2), " minutes\n")

  cat(crayon::bold(crayon::underline(crayon::green(time))))


  ### return local authority level smoke free dividend estimates

  return(dividend)

}
