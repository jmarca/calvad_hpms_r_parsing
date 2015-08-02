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
       'Year_Record'    = readr::col_double(),
       'State_Code'     = readr::col_double(),
       'Route_ID'       = readr::col_character(),
       'Begin_Point'    = readr::col_double(),
       'End_Point'      = readr::col_double(),
       'Data_Item'      = readr::col_character(),
       'Section_Length' = readr::col_double(),
       'Value_Numeric'  = readr::col_double(),
       'Value_Text'     = readr::col_character(),
       'Value_Date'     = readr::col_datetime(dt_format),
       'Comments'       = readr::col_character()
    )
    readr::read_delim(filename,delim,col_types=coltypes)
}
