##' Read file
##'
##' A super thin wrapper around readr::read_csv
##'
##' @title read_file
##' @param filename the filename of the CSV file to read
##' @param delim the delimiter to use
##' @param dt_format the date time format to use
##' @return a data frame
##' @author James E. Marca
##'
read_file <- function(filename,delim=",",dt_format='%Y%.%m%.%d %H%.%M%.%S'){
    coltypes <- list(
        readr::col_double(),
        readr::col_double(),
        readr::col_character(),
        readr::col_double(),
        readr::col_double(),
        readr::col_character(),
        readr::col_double(),
        readr::col_double(),
        readr::col_character(),
        readr::col_datetime(dt_format),
        readr::col_character()
    )
    readr::read_delim(filename,delim,col_types=coltypes)
}
