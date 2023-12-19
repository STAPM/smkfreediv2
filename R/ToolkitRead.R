#' Read Toolkit Data
#'
#' Reads in the raw Smoking Toolkit Study (STS) data in SPSS format
#' and saves to a chosen directory in RDS format as well as returning it as
#' an R object
#'
#' @param in_path path to the directory where the raw SPSS data is saved.
#' @param out_path path to the directory where the working version of the data will be exported.
#' @param data name of the data file (defaults to the April 2021 version)
#' @param save Logical. If TRUE, exports out a copy of the data in RDS format to the directory specified by `out_path`
#' @param name Character. Name of the file to export
#'
#' @importFrom data.table := copy rbindlist setDT
#'
#' @return Data frame of STS data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
ToolkitRead <- function(
    in_path,
    out_path,
    data ,
    save = TRUE,
    name
) {

  suppressWarnings(
  STS_data <- foreign::read.spss(paste0(here::here(in_path), "/", data), to.data.frame = TRUE)
  )

  setDT(STS_data)

  setnames(STS_data, stringr::str_replace(colnames(STS_data), "X.", "A"))

  if (save) {
    saveRDS(STS_data,paste0(here::here(out_path), "/", name, ".rds"))
  }

  return(STS_data)
}
