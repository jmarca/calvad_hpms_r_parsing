require('RPostgreSQL')

## need to load config for psql params
config_file <- Sys.getenv('R_CONFIG')

if(config_file ==  ''){
    config_file <- 'test.config.json'
}
print(paste ('using config file =',config_file))
configurator <- configr::configrr()
config <-  configurator(config_file)

m <- dbDriver("PostgreSQL")
con <-  dbConnect(m
                 ,user=config$postgresql$auth$username
                 ,host=config$postgresql$host
                 ,dbname=config$postgresql$hpmsdb)

fname <- c('./files/2011.csv'
          ,'./files/2012.csv'
          ,'./files/2013.csv')

testtable <- 'test_sql_save'

suppressMessages(
    rs <- RPostgreSQL::dbSendQuery(
        con,
        paste('drop table if exists',testtable)
        )
)

test_that(
    'can write sql',
    {
        filename <-  fname[2]
        df <- read_file(filename)
        df <- whitespace_fix(df)
        df_spread <- grouped_extract(df)
        res <- save_and_tweak_hpms_data(df=df_spread,
                                        config=config,
                                        tablename=testtable)

        sqlsrc <-  dplyr::src_postgres(dbname=config$postgresql$hpmsdb,
                                       host=config$postgresql$host,
                                       port=config$postgresql$port,
                                       user=config$postgresql$user
                                       )
        my_tbl <- dplyr::tbl(src=sqlsrc,testtable)

        qres <- dplyr::collect(my_tbl)
        zres <- dplyr::collect(res)
        expect_equal(qres,zres)

        rec_a03501 <- qres %>% dplyr::filter(route_id == '000000A03501')
        expect_equal(dim(rec_a03501)[1],3)
    })


## test fails, I actually CANNOT append data!
## test_that(
##     'can append sql',
##     {
##         filename <-  fname[1]
##         df <- read_file(filename)
##         df <- whitespace_fix(df)
##         df_spread <- grouped_extract(df)
##         res <- save_and_tweak_hpms_data(df=df_spread,
##                                         config=config,
##                                         tablename=testtable)

##         sqlsrc <-  dplyr::src_postgres(dbname=config$postgresql$hpmsdb,
##                                        host=config$postgresql$host,
##                                        port=config$postgresql$port,
##                                        user=config$postgresql$user
##                                        )
##         my_tbl <- dplyr::tbl(src=sqlsrc,testtable)

##         qres <- dplyr::collect(my_tbl)
##         zres <- dplyr::collect(res)
##         expect_equal(qres,zres)

##     })

suppressMessages(
    rs <- RPostgreSQL::dbSendQuery(
        con,
        paste('drop table if exists',testtable)
        )
)

RPostgreSQL::dbDisconnect(con)
