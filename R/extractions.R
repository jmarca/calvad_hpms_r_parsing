##' Extract Value_Numeric from HPMS CSV data
##'
##' @title extract_numeric
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Value_Numeric",
##'     and the identifying rows in the data
##' @author James E. Marca
##'
extract_numeric <- function(the_data){
    data_numeric <- the_data %>%
        dplyr::select(-Comments,-Value_Date,-Value_Text) %>%
        dplyr::filter( !is.na(Value_Numeric) & Value_Numeric != '') %>%
        dplyr::distinct %>%
        tidyr::spread(Data_Item,Value_Numeric)
}


##' Extract date from HPMS CSV data
##'
##' @title extract_date
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Value_Date",
##'     and the identifying rows in the data
##' @author James E. Marca
##'
extract_date <- function(the_data){
    data_date <-
        the_data %>%
        dplyr::select(-Section_Length,-Comments,-Value_Numeric,-Value_Text) %>%
        dplyr::filter( !is.na(Value_Date) & Value_Date != '') %>%
        tidyr::spread(Data_Item,Value_Date)
}

##' Extract text from HPMS CSV data
##'
##' @title extract_text
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Value_Text",
##'     and the identifying rows in the data
##' @author James E. Marca
##'
extract_text <- function(the_data){
    data_text <-
        the_data %>%
        dplyr::select(-Section_Length,-Comments,-Value_Numeric,-Value_Date) %>%
        dplyr::filter(!is.null(Value_Text) &
                           !is.na(Value_Text) &
                           Value_Text != '') %>%
        tidyr::spread(Data_Item,Value_Text)
}

##' Extract comments from HPMS CSV data
##'
##' @title extract_comments
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Comments",
##'     and the identifying rows in the data
##' @author James E. Marca
##'
extract_comments <- function(the_data){
    data_comments <-
        the_data %>%
        dplyr::select(-Section_Length,-Value_Text,-Value_Numeric,-Value_Date) %>%
        dplyr::filter( !is.na(Comments) & Comments != '') %>%
        tidyr::spread(Data_Item,Comments)
}
