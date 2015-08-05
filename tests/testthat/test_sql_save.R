## need to lod config for psql params
config_file <- Sys.getenv('R_CONFIG')

if(config_file ==  ''){
    config_file <- 'test.config.json'
}
print(paste ('using config file =',config_file))
configurator <- configr::configrr()
config <-  configurator(config_file)

fname <- c('./files/2011.csv'
          ,'./files/2012.csv'
          ,'./files/2013.csv')



test_that(
    'can write sql',
    {
        filename <-  fname[2]
        df <- read_file(filename)
        df <- whitespace_fix(df)
        df_spread <- grouped_extract(df)
        res <- save_and_tweak_hpms_data(df=df_spread,
                                        config=config,
                                        tablename='deleteme')

        sqlsrc <-  dplyr::src_postgres(dbname=config$postgresql$hpmsdb,
                                       host=config$postgresql$host,
                                       port=config$postgresql$port,
                                       user=config$postgresql$user
                                       )
        my_tbl <- dplyr::tbl(src=sqlsrc,'deleteme')

        qres <- dplyr::collect(my_tbl)
        zres <- dplyr::collect(res)
        expect_equal(qres,zres)

    })
