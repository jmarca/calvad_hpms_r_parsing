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


##' Parse a CSV HPMS data file, group by vars, extract types,
##' recombine, and then spread the data wide
##'
##' @title parse_file_grouped_extract
##' @param filename the path to the filename to parse
##' @param ... additional parameters to pass on to {read_file}
##' @return a list of data frames
##' @author James E. Marca
##' @export
##'
parse_file_grouped_extract <- function(filename,...){
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

##' Parse a CSV HPMS data file, return lists of numeric,text,date,comments
##'
##' @title parse_file_into_types
##' @param filename the path to the filename to parse
##' @param ... will be passed to {read_file}
##' @return a list of data frames
##' @author James E. Marca
##' @export
##'
parse_file_into_types <- function(filename,...){
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
