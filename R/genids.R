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
    route_var_idx <- grepl(pattern="^route_id$",
                           x=names(the_data),
                           perl=TRUE,ignore.case=TRUE)
    route_var <- names(the_data)[route_var_idx]
    leftpad <- max(stringr::str_length(the_data[[route_var]]))
    stringr::str_pad(the_data[[route_var]],side='left',pad='0',width=leftpad)
}
