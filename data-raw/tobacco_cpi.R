library(readxl)
library(data.table)
library(curl)


Year <- c(rep(1988:2022, each = 12) , rep(2023,10))
month <- c(rep(1:12, times = 2022 - 1988 + 1), 1:10)

data <- data.table(Year,month)

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/d7cb/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")


cpi <- readxl::read_excel(temp,
                          sheet = "data",
                          range = "A187:B616",
                          col_names = FALSE)

setDT(cpi)
setnames(cpi, names(cpi), c("t","cpi"))

cpi_tobacco <- cbind(data, cpi)

cpi_tobacco[, Month := factor(month,
                              levels = 1:12,
                              labels = c("January","February","March","April","May","June",
                                         "July","August","September","October","November","December"))]

cpi_tobacco <- cpi_tobacco[, c("Year","month","Month","cpi")]

### re-base to december 2022

cpi_base <- as.numeric(cpi_tobacco[Year == 2022 & month == 12, "cpi"])

cpi_tobacco[, cpi := 100*cpi/cpi_base]

usethis::use_data(cpi_tobacco, overwrite = TRUE)
