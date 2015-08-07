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
    grouped_extract(df)
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


require('RPostgreSQL')

##' Tweak the HPMS table
##'
##' Will save a bit, truncate it, add an index, then save all
##'
##' @title save_and_tweak_hpms_data
##' @param df  the dataframe to save
##' @param config the config file, loaded via configr
##' @param con a database connection.  Optional, will be created if not set
##' @param tablename the table name to write the data into
##' @author James E. Marca
##' @export
##'
save_and_tweak_hpms_data <- function(df,config,con,tablename='hpms_subset'){

    if(missing(con)){
        m <- dbDriver("PostgreSQL")
        con <-  dbConnect(m
                         ,user=config$postgresql$auth$username
                         ,host=config$postgresql$host
                         ,dbname=config$postgresql$hpmsdb)

    }

    ## save it using dplyr
    sqlsrc <-  dplyr::src_postgres(dbname=config$postgresql$hpmsdb,
                                   host=config$postgresql$host,
                                   port=config$postgresql$port,
                                   user=config$postgresql$user
                                   )
    names(df) <- stringr::str_to_lower(names(df))

    ## nah, not useful really
    ## df$route_id <- canonical_routeid(df)

    ## if the table does not exist, then write one row to initialize it

    if( !dplyr::db_has_table(con=con,table=tablename) ){

        sqlhpms <- dplyr::copy_to(
            sqlsrc,df=df[1:2,],
            tablename,
            temporary=FALSE
        )

        ## truncate that data, add serial column for PK
        trunc_query <- paste( "truncate",tablename);
        rs <- RPostgreSQL::dbSendQuery(con,trunc_query)
        ## create a primary key index

        pk_query <- paste("ALTER TABLE ONLY",tablename,
                          "add column id serial primary key")
        rs <- RPostgreSQL::dbSendQuery(con,pk_query)
    }

    ## if the table and the data do not match up, then writing data will choke

    my_tbl <- dplyr::tbl(src=sqlsrc,tablename)
    reappend <- dplyr::db_insert_into(con=con,table=tablename,values=df)
    my_tbl <- dplyr::tbl(src=sqlsrc,tablename)
    if(missing(con)){
        dbDisconnect(con)
    }
    my_tbl
}
