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

##' A state machine to merge and rationalize the passed in records.
##'
##' The problem with the HPMS data as recorded is that it contains
##' overlapping records, and many times the overlapping data is not
##' repeated in other records.  For example
##'
##' ```
##' Rec   Route_ID Begin_Point End_Point  AADT OWNERSHIP F_SYSTEM FACILITY_TYPE THROUGH_LANES
##'  1     18001       62.08     62.94    NA         4       NA            NA            NA
##'  2     18001       62.08     62.94    NA        NA       NA            NA            NA
##'  3     18001       62.08     63.59 24637        NA        3             2            NA
##'  4     18001       62.94     63.44    NA         2       NA            NA             2
##'  5     18001       62.94     63.44    NA        NA       NA            NA            NA
##'  6     18001       63.44     63.59    NA         4       NA            NA            NA
##'  7     18001       63.44     63.59    NA         4       NA            NA             2
##'  8     18001       63.59     64.10    NA         4       NA            NA             4
##'  9     18001       63.59     64.10    NA         4       NA            NA            NA
##' 10     18001       63.59     64.10 35250        NA        3             2             2
##'
##' ```
##'
##' If you notice, the entire section, from 62.08 through 64.10 has an
##' AADT value.  However, several sections have NA assigned because they
##' are "under" the wider spans 62.08 to 63.59, and then 63.59 to
##' 64.10. Within these spans, characteristics like "Ownership", "Lanes"
##' and other values (not shown) change, while "F-System" and
##' "Facility-Type" do not.
##'
##' Looking at it another way:
##'
##' Rec         Section Covered
##'     +---------+---------+-----+--------+
##'  1  |---------|
##'  2  |---------|
##'  3  |-------------------------|
##'  4            |---------|
##'  5            |---------|
##'  6                      |-----|
##'  7                      |-----|
##'  8                            |--------|
##'  8                            |--------|
##' 10                            |--------|
##'     +---------+---------+-----+--------+
##'     A         B         C     D        E
##'   62.08     62.94     63.44 63.59    64.10
##'
##' This routine will step through the passed in records and create
##' the edge on view, looking *up* at the above diagram.  So for the
##' section from 62.08, it will merge records 1, 2, and 3.  From B to
##' C will merge records 3, 4, and 5.  From C to D will merge 3, 6,
##' and 7.  And finally section D to E will merge records 8, 9, and
##' 10.
##'
##'
##' Note that I *hate* this code.  It feels very much *not* in a
##' proper R idiom, rather in a more loopy language.  I feel like
##' there should be some way to organize this in terms of lists etc,
##' but I can't see it, and this does the job for now (albeit super
##' slowly)
##'
##' One thing to keep in mind is due to how dplyr::do works, every
##' group of records must return at least one row.  So that means a
##' record with start point equal to end point will produce a null
##' output here, and so you have to get rid of those *before* calling
##' this routine.
##'
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
    states <- unique(sort(c(sorted$Begin_Point,sorted$End_Point)))

    for (i in 1:length(states)){
        ## what is active now
        active <- sorted$Begin_Point <= states[i] &
            sorted$End_Point > states[i]
        if(any(active) && length(active[active]>1)){
            ## if smaller, nothing to do here


            newrow <- sorted[1,common_vars]

            newrow$Begin_Point <- states[i]

            endpoint <- min(sorted$End_Point[active])
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
                    newrow[z]=NA
                }
            }
            newset <- rbind(newset,newrow)
        }
    }
    if(is.data.frame(newset)){
        return(newset)
    }else{
        print('not a data frame')
        print(records)
        return(NULL)
    }
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

    ## also note that the Begin_Point != End_Point filter is required
    ## for the state_machine code
    r3 <- the_data %>%
        dplyr::filter(Data_Item %in% is_keeper & Begin_Point != End_Point) %>%
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

    ## missing_columns <- setdiff(c(common_vars,
    ##                              paste(is_keeper,'txt',sep='_')
    ##                              paste(is_keeper,'cmt',sep='_')
    ##                             ,names(r7))
    ## r7[,missing_columns] <- NA

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
