##' Extract Value_Numeric from HPMS CSV data
##'
##' @title extract_numeric
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Value_Numeric",
##'     and the identifying rows in the data
##' @importFrom magrittr "%>%"
##' @author James E. Marca
##'
extract_numeric <- function(the_data){
    filtereddf <- the_data %>%
                   dplyr::select(-Comments,-Value_Date,-Value_Text) %>%
                   dplyr::filter( !is.na(Value_Numeric) & Value_Numeric != '')
    dplyr::distinct(filtereddf)  %>%
        tidyr::spread(Data_Item,Value_Numeric)
}


##' Extract date from HPMS CSV data
##'
##' @title extract_date
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Value_Date",
##'     and the identifying rows in the data
##' @importFrom magrittr "%>%"
##' @author James E. Marca
##'
extract_date <- function(the_data){
    if(!methods::is(the_data$Value_Date,'POSIXt')){
        return (

                ## if is string or not POSIXt, then also test for blank

            the_data %>%
                dplyr::select(-Section_Length,-Comments,
                              -Value_Numeric,-Value_Text) %>%
                dplyr::filter( !is.na(Value_Date) & Value_Date != '' ) %>%
                tidyr::spread(Data_Item,Value_Date)
        )
    }else{
        return (

                ## if is POSIXt, then all blank become NA during loading

            the_data %>%
                dplyr::select(-Section_Length,-Comments,
                              -Value_Numeric,-Value_Text) %>%
                dplyr::filter( !is.na(Value_Date) ) %>%
                tidyr::spread(Data_Item,Value_Date)
        )

    }

}

##' Extract text from HPMS CSV data
##'
##' @title extract_text
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Value_Text",
##'     and the identifying rows in the data
##' @author James E. Marca
##' @importFrom magrittr "%>%"
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
##' @importFrom magrittr "%>%"
##' @author James E. Marca
##'
extract_comments <- function(the_data){
    data_comments <-
        the_data %>%
        dplyr::select(-Section_Length,-Value_Text,-Value_Numeric,-Value_Date) %>%
        dplyr::filter( !is.na(Comments) & Comments != '') %>%
        tidyr::spread(Data_Item,Comments)
}
