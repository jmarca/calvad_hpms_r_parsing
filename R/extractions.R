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


is_keeper <-  c(
"AADT", "AADT_COMBINATION", "AADT_SINGLE_UNIT", "ACCESS_CONTROL",
"ALTERNATIVE_ROUTE_NAME", "COUNTER_PEAK_LANES", "COUNTY_CODE", "DIR_FACTOR",
"FACILITY_TYPE", "F_SYSTEM", "HOV_LANES", "HOV_TYPE", "IRI", "K_FACTOR",
"LANE_WIDTH", "NHS", "OWNERSHIP", "PCT_PEAK_COMBINATION", "PCT_PEAK_SINGLE",
"PEAK_LANES", "ROUTE_NUMBER", "ROUTE_QUALIFIER", "ROUTE_SIGNING", "SPEED_LIMIT",
"THROUGH_LANES", "TRUCK", "URBAN_CODE", "YEAR_LAST_IMPROV"
               )
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title text_clean
##' @param textvals
##' @return clean text
##' @importFrom magrittr "%>%"
##' @author James E. Marca
##'
text_clean <- function(textvals){

    ## first get rid of all NA values
    na_idx <- is.na(textvals)
    empty_idx <- textvals == ''
    idx_notna <- !(na_idx | empty_idx)

    if(any(idx_notna)){
        if(length(idx_notna[idx_notna]) > 1){

            print(table(!na_idx))
            print(table(!empty_idx))

            print(paste('combining',length(idx_notna[idx_notna]),'values for text field',
                        ))
            print(paste(textvals[idx_notna]))
        }
        return (paste(textvals[idx_notna],collapse='; '))
    }else{
        return ('')
    }

}

common_vars <- c('Year_Record',
                 'State_Code',
                 'Route_ID',
                 'Begin_Point',
                 'End_Point')

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title state_machine
##' @param records
##' @return a df sliced to minimal begin_pt, end_pts
##' @importFrom magrittr "%>%"
##' @author James E. Marca
##'
state_machine <- function(records){

    var_cols <- setdiff(names(records),
                        c(common_vars,'Section_Length'))

    newset <- NULL
    sorted <- dplyr::arrange(records,Begin_Point)
    states <- sorted$Begin_Point
    for (i in 1:length(states)){
        ## what is active now
        active <- sorted$Begin_Point <= states[i] &
            sorted$End_Point > states[i]
        if(any(active) && length(active[active]>1)){
            ## if smaller, nothing to do here
            endpoint <- min(sorted$End_Point[active])
            newrow <- sorted[i,common_vars]
            newrow$End_Point <- endpoint
            ## stop the overlap insanty!

            ## fix section length too
            newrow$Section_Length <- newrow$End_Point - newrow$Begin_Point

            for(z in var_cols){
                possibles <- sorted[active,][[z]]
                na_idx <- is.na(possibles)
                empty_idx <- possibles == ''

                idx_something <- !( na_idx | empty_idx)

                ## sql coalesce takes the first one.  check if unique?
                if(any(idx_something)){
                    possibleValue <- unique(possibles[idx_something])
                    if(length(possibleValue)>1){
                        print(paste('picking only the first of',
                                    length(possibleValue),
                                    'values for text field',
                                    z
                                    )
                              )
                        print(possibleValue)
                    }
                    newrow[z]=possibleValue[1]
                }else{
                    ## print(paste('nada',states[i],z))
                }
            }
            if(is.null(newset)){
                newset <- data.frame(newrow)
            }else{
                newset <- cbind(newset,newrow)
            }
        }
    }
    return(newset)
}

##' Extract Values from HPMS CSV data
##'
##' @title extract_numeric
##' @param the_data the data frame of data
##' @return a data frame containing the "Data_Item", its "Value_Numeric",
##'     and the identifying rows in the data
##' @importFrom magrittr "%>%"
##' @author James E. Marca
##' @export
##'
grouped_extract <- function(the_data){

    ## throwing in the towel for now.  Keep only the variables I care about
    r3 <- the_data %>%
        dplyr::filter(Data_Item %in% is_keeper ) %>%
        dplyr::group_by(Year_Record,
                        State_Code,
                        Route_ID,
                        Begin_Point,
                        End_Point,
                        Data_Item) %>%
        dplyr::summarise(Section_Length=max(Section_Length,na.rm=TRUE)
                        ,num=max(Value_Numeric,na.rm=TRUE)
                        ,txt=text_clean(Value_Text)
                        ,cmt=text_clean(Comments)
                         )
    short_idx <- is.na(r3$Section_Length)
    if(any(short_idx)){
        r3$Section_Length[short_idx] <-
            r3$End_Point[short_idx] - r3$Begin_Point[short_idx]
    }

    r4_n <- r3 %>%
        dplyr::select(-txt,-cmt) %>%
        dplyr::filter(!is.na(num)) %>%
        tidyr::spread(Data_Item,num)

    r4_t <- r3 %>%
        dplyr::select(-Section_Length,-num,-cmt) %>%
        dplyr::filter(txt !=  '' & !is.na(txt)) %>%
        tidyr::spread(Data_Item,txt)
    varlen <- length(names(r4_t))
    if(varlen >5){
        names(r4_t)[6:varlen] <- paste(names(r4_t)[6:varlen],'txt',sep='_')
    }
    r4_c <- r3 %>%
        dplyr::select(-Section_Length,-num,-txt) %>%
        dplyr::filter(cmt !=  '' & !is.na(cmt)) %>%
        tidyr::spread(Data_Item,cmt)
    varlen <- length(names(r4_c))
    if(varlen >5){
        names(r4_c)[6:varlen] <- paste(names(r4_c)[6:varlen],'cmt',sep='_')
    }


    ## recombine
    r5 <- dplyr::full_join(r4_t,r4_c)
    r6 <- dplyr::full_join(r4_n,r5)

    ## do that hacky thing above to make each record consistent with
    ## the "containing" records.  So if a record has a value

    r7 <- r6 %>%
        dplyr::group_by(Year_Record,
                        State_Code,
                        Route_ID) %>%
        dplyr::do(state_machine(.))
    r7
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
