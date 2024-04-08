library(readxl)
library(data.table)
library(curl)

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/d7cb/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")


cpi <- readxl::read_excel(temp,
                          sheet = "data",
                          range = "A20:B700",
                          col_names = FALSE)

setDT(cpi)
setnames(cpi, names(cpi), c("t","cpi"))

cpi[, Year := as.numeric(substr(t,1,4))]
cpi[, month := substr(t,6,8)]

### keep only monthly data and recode to numeric

cpi <- cpi[!(month %in% c(NA,"","Q1","Q2","Q3","Q4")),]

cpi[.(month = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
       to = c(1:12)), on = "month", month := i.to]

cpi[, month := as.numeric(month)]


cpi_tobacco <- cpi[, c("Year","month","cpi")]

cpi_tobacco[, Month := factor(month,
                              levels = 1:12,
                              labels = c("January","February","March","April","May","June",
                                         "July","August","September","October","November","December"))]

cpi_tobacco <- cpi_tobacco[, c("Year","month","Month","cpi")]

### re-base to december 2022

cpi_base <- as.numeric(cpi_tobacco[Year == 2023 & month == 12, "cpi"])

cpi_tobacco[, cpi := 100*cpi/cpi_base]

usethis::use_data(cpi_tobacco, overwrite = TRUE)
