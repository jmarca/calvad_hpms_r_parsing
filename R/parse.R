## borrowed the following hack from tidyr to get %>% imported

##' Pipe operator
##'
##' See \code{\link[magrittr]{\%>\%}} for more details.
##'
##' @name %>%
##' @rdname pipe
##' @keywords internal
##' @importFrom magrittr %>%
##' @usage lhs \%>\% rhs
NULL


##' Parse a CSV HPMS data file
##'
##' so far only works on 2012 data
##'
##' @title parse_file
##' @param filename the path to the filename to parse
##' @return a list of data frames
##' @author James E. Marca
##' @export
##'
parse_file <- function(filename,...){
    df <- read_file(filename,...)
    df <- whitespace_fix(df)
    dfn <- extract_numeric(df)
    dfdt <- extract_date(df)
    dftx <- extract_text(df)
    dfc <- extract_comments(df)

    ## don't recombine here.  Will produce loads of NA values if I do
    list('data_numeric'=dfn,
         'data_date'=dfdt,
         'data_text'=dftx,
         'data_comments'=dfc
         )
}
