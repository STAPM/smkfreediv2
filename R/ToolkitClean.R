#' Clean Toolkit Data
#'
#' Clean the raw toolkit data to produce a dataset that can be used for the
#' smoke free dividend calculations
#'
#' @param data Data table. Toolkit data.
#' @param start_month Numeric. Toolkit tart month for the analysis period (default = 90, April 2014).
#' @param end_month Numeric. Toolkit end month for the analysis period (default = 160, February 2020).
#' @param index Data table. Tobacco price data to use in the deflation. Default is the ONS tobacco CPI series.
#' @param base_month Numeric. Month of the year to deflate currency variables to. (Default = 12).
#' @param base_year Numeric. Year to deflate currency variables to. (Default = 2018).
#'
#' @importFrom data.table := copy rbindlist setDT
#'
#' @return Cleaned toolkit data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
ToolkitClean <- function(data,
                         start_month = 90,
                         end_month = 160,
                         index = smkfreediv2::cpi_tobacco,
                         base_month = 12,
                         base_year = 2018) {

  ### retain only the needed variables

  data <- data[, .(xwave,    # survey wave
                   month,    # survey month
                   Sex = sexz,     # sex
                   Aweight0,       # weight
                   Age = actage,   # age
                   qual,           # highest qualification
                   tenure,        # housing tenure
                   gor = gore, # government office region
                   grade = sgz ,      # social grade
                   work,       # working status
                   ethnic,     # ethnicity
                   LAD12CD = LAcode,     # local authority code
                   daily,      # current daily smoker
                   Smoker = smoker,   # smoking status
                   ExSmoker = exsmoker,

                   ### expenditures
                   weekspend,
                   buysmug,
                   buysmuc,

                   ### consumption/dependence
                   # q632x1, q632a9_1, q632a0_1, q632b1_1
                   ryoperc,
                   cigs_type = typcig,
                   cigs_perday_ryo = basehand,
                   cigs_perday_fm = mancig
  )]

  ###########################
  ### CONSUMPTION VARIABLES


  data[, cigs_type_fm   := ifelse(cigs_type == "Manufactured only", 1, 0)]
  data[, cigs_type_ryo  := ifelse(cigs_type == "RYO only", 1, 0)]
  data[, cigs_type_both := ifelse(cigs_type == "Manufactured and RYO", 1, 0)]

  data[is.na(cigs_type), cigs_type_fm := NA]
  data[is.na(cigs_type), cigs_type_ryo := NA]
  data[is.na(cigs_type), cigs_type_both := NA]


  #######################
  #### TIME VARIABLES

  data[month %in% c(   3,  15, 27, 39, 51, 63, 75, 87, 99,  111, 123, 135, 147, 159, 171, 183, 195, 207 ), Month :=  "January"]
  data[month %in% c(   4,  16, 28, 40, 52, 64, 76, 88, 100, 112, 124, 136, 148, 160, 172, 184, 196, 208 ), Month :=  "February"]
  data[month %in% c(   5,  17, 29, 41, 53, 65, 77, 89, 101, 113, 125, 137, 149,      173, 185, 197, 209 ), Month :=  "March"]
  data[month %in% c(   6,  18, 30, 42, 54, 66, 78, 90, 102, 114, 126, 138, 150, 162, 174, 186, 198, 210 ), Month :=  "April"]
  data[month %in% c(   7,  19, 31, 43, 55, 67, 79, 91, 103, 115, 127, 139, 151, 163, 175, 187, 199, 211 ), Month :=  "May"]
  data[month %in% c(   8,  20, 32, 44, 56, 68, 80, 92, 104, 116, 128, 140, 152, 164, 176, 188, 200, 212 ), Month :=  "June"]
  data[month %in% c(   9,  21, 33, 45, 57, 69, 81, 93, 105, 117, 129, 141, 153, 165, 177, 189, 201, 213 ), Month :=  "July"]
  data[month %in% c(   10, 22, 34, 46, 58, 70, 82, 94, 106, 118, 130, 142, 154, 166, 178, 190, 202, 214 ), Month :=  "August"]
  data[month %in% c(   11, 23, 35, 47, 59, 71, 83, 95, 107, 119, 131, 143, 155, 167, 179, 191, 203, 215 ), Month :=  "September"]
  data[month %in% c(   12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216 ), Month :=  "October"]
  data[month %in% c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 169, 191, 193, 205, 217 ), Month :=  "November"]
  data[month %in% c(2, 14,     38, 50, 62, 74, 86, 98, 110, 122, 134, 146, 158, 170, 182, 194, 206, 218 ), Month :=  "December"]

  data[month %in% 1:2, Year := 2006]
  data[month %in% 3:14, Year := 2007]
  data[month %in% 15:25, Year := 2008]
  data[month %in% 27:38, Year := 2009]
  data[month %in% 39:50, Year := 2010]
  data[month %in% 51:62, Year := 2011]
  data[month %in% 63:74, Year := 2012]
  data[month %in% 75:86, Year := 2013]
  data[month %in% 87:98, Year := 2014]
  data[month %in% 99:110, Year := 2015]
  data[month %in% 111:122, Year := 2016]
  data[month %in% 123:134, Year := 2017]
  data[month %in% 135:146, Year := 2018]
  data[month %in% 147:158, Year := 2019]
  data[month %in% 159:170, Year := 2020]
  data[month %in% 171:182, Year := 2021]
  data[month %in% 183:194, Year := 2022]
  data[month %in% 195:206, Year := 2023]
  data[month %in% 207:218, Year := 2024]

  data <- data[month %in% start_month:end_month,]

  ################################
  ## SOCIO-ECONOMIC VARIABLES

  ## region

  data[, gor := factor(gor, levels = c("North East","North West","Yorkshire and The Humber",
                                       "East Midlands","West Midlands","East of England",
                                       "London","South East","South West"))]

  data[gor == "Yorkshire and The Humber", gor := "Yorkshire and the Humber"]
  data[, gor := as.character(gor)]


  ## age and sex

  data[, Sex := factor(Sex, levels = c("Men","Women"), labels = c("Male","Female"))]

  data[ , Ageband := c("16-24", "25-34", "35-44", "45-54", "55-64", "65+")[findInterval(Age, c(16, 25, 35, 45, 55, 65, 1000))]]

  data[, Ageband := factor(Ageband, levels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65+"))]

  ## social grade

  data[grade %in% c("AB","C1"), grade_2cat := "ABC1"]
  data[grade %in% c("C2","D","E"), grade_2cat := "C2DE"]

  data[, grade := factor(grade, levels = c("AB", "C1", "C2", "D", "E"))]
  data[, grade_2cat := factor(grade_2cat, levels = c("ABC1", "C2DE"))]

  ## labour market status

  data[work %in% c("HAVE PAID JOB - FULL TIME (30+ HOURS PER WEEK)"),lmstatus := "Employed Full-Time"]
  data[work %in% c("HAVE PAID JOB - PART TIME (8-29 HOURS PER WEEK)",
                   "HAVE PAID JOB - PART TIME (UNDER 8 HOURS PER WEEK)"),lmstatus := "Employed Part-Time"]
  data[work %in% c("SELF-EMPLOYED"),lmstatus := "Self-Employed"]
  data[work %in% c("UNEMPLOYED AND SEEKING WORK"),lmstatus := "Unemployed"]
  data[work %in% c("FULL TIME STUDENT",
                   "STILL AT SCHOOL"),lmstatus := "Education"]
  data[work %in% c("RETIRED"),lmstatus := "Retired"]
  data[work %in% c("NOT WORKING - HOUSEWIFE/HUSBAND",
                   "NOT IN PAID WORK FOR OTHER REASON",
                   "NOT IN PAID WORK BECAUSE OF LONG TERM ILLNESS OR DISABILITY"),lmstatus := "Other Inactive"]

  data[lmstatus %in% c("Employed Full-Time",
                       "Employed Part-Time",
                       "Self-Employed"), lmstatus_3cat := "Employed"]

  data[lmstatus %in% c("Unemployed"), lmstatus_3cat := "Unemployed"]

  data[!(lmstatus %in% c("Employed Full-Time",
                         "Employed Part-Time",
                         "Self-Employed",
                         "Unemployed")), lmstatus_3cat := "Inactive"]

  data[, lmstatus := factor(lmstatus, levels = c("Employed Full-Time",
                                                 "Employed Part-Time",
                                                 "Self-Employed",
                                                 "Unemployed",
                                                 "Education","Retired","Other Inactive"))]

  data[, lmstatus_3cat := factor(lmstatus_3cat, levels = c("Employed",
                                                           "Unemployed",
                                                           "Inactive"))]

  ## ethnicity


  data[ethnic %in% c("WHITE BRITISH","WHITE OTHER","WHITE IRISH","WHITE GYPSY /TRAVELLER"), ethnicity := "White"]
  data[!(ethnic %in% c("WHITE BRITISH","WHITE OTHER","WHITE IRISH","WHITE GYPSY /TRAVELLER")), ethnicity := "Non-White"]

  data[, ethnicity := factor(ethnicity, levels = c("White","Non-White"))]

  data[, c("work","ethnic") := NULL]

  ############################################################################
  ### GENERATE LOCAL AUTHORITY NAMES AND CLEAN TO MATCH OTHER DATA SOURCES ###

  # read in the mapping from LTLA to UTLA

  data[, LAD12CD := stringr::str_trim(LAD12CD)]
  data_out <- merge(data, smkfreediv2::ltla_2012_to_utla_2019, by = c("LAD12CD"), all.x = TRUE, sort = FALSE)

  ## drop non-English local authorities
  data_out <- data_out[substring(LAD12CD,1,1) == "E",]

  ############################################################################
  ### CREATE A DEFLATED WEEKLY SPENDING VARIABLE #############################

  ## rebase to chosen base month and year

  inflation <- copy(smkfreediv2::cpi_tobacco)

  cpi_base <- as.numeric(inflation[Year == base_year & month == base_month, "cpi"])
  index[, cpi := 100*cpi/cpi_base]

  ## merge to the toolkit data

  data_out <- merge(data_out, index, by = c("Year","Month"), all.x = TRUE, sort = FALSE)

  ## deflate

  data_out[, weekspend_real := weekspend*(100/cpi)]

  ## drop cpi

  data_out[, cpi := NULL]

  ###########

  return(data_out)
}
