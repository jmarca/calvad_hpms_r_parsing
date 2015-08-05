##' canonical routeid generator
##'
##' the route id in HPMS is sometimes prepended with zeros
##'
##' @title canonical_routeid
##' @param the_data a data frame
##' @return the new column of ids
##' @author James E. Marca
##' @export
##'
canonical_routeid <- function(the_data){
    leftpad <- max(stringr::str_length(the_data$Route_ID))
    stringr::str_pad(the_data$Route_ID,side='left',pad='0',width=leftpad)
}
