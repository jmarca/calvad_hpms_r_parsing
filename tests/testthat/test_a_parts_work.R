fname <- c('./files/2011.csv'
          ,'./files/2012.csv'
          ,'./files/2013.csv')


test_that(
    'can load (some) of csv file for 2012',
    {
        filename <-  fname[2]
        df <- read_file(filename)
        df <- whitespace_fix(df)
        dfn <- extract_numeric(df)
        expect_equal(dim(dfn),c(119,50))

        dfdt <- extract_date(df)
        expect_equal(dim(dfdt),c(34,7))
        expect_equal(names(dfdt),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "FUTURE_AADT","IRI"
                       )
                     )
        dftx <- extract_text(df)
        expect_equal(dim(dftx),c(26,6))
        expect_equal(names(dftx),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "ALTERNATIVE_ROUTE_NAME"
                       )
                     )
        dfc <- extract_comments(df)
        expect_equal(dim(dfc),c(90,13))
        expect_equal(names(dfc),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "AADT",
                       "COUNTY_CODE",
                       "F_SYSTEM",
                       "K_FACTOR",
                       "NHS",
                       "OWNERSHIP",
                       "THROUGH_LANES",
                       "URBAN_CODE"
                       )
                     )

    })

test_that(
    'can load (some) of csv file for 2011',
    {
        filename <-  fname[1]
        df <- read_file(filename,dt_format="%m%.%d%.%Y %H%.%M%.%S %p")
        df <- whitespace_fix(df)
        dfn <- extract_numeric(df)
        expect_equal(dim(dfn),c(55,55))

        dfdt <- extract_date(df)
        expect_equal(dim(dfdt),c(34,7))
        expect_equal(names(dfdt),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "FUTURE_AADT","IRI"
                       )
                     )
        dftx <- extract_text(df)
        expect_equal(dim(dftx),c(37,6))
        expect_equal(names(dftx),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "ALTERNATIVE_ROUTE_NAME"
                       )
                     )
        dfc <- extract_comments(df)
        expect_equal(dim(dfc),c(0,5))
        expect_equal(names(dfc),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point"
                       )
                     )

    })

test_that(
    'can load (some) of csv file for 2013',
    {
        filename <-  fname[3]
        df <- read_file(filename,delim='|')
        df <- whitespace_fix(df)
        dfn <- extract_numeric(df)
        expect_equal(dim(dfn),c(101,55))

        dfdt <- extract_date(df)
        expect_equal(dim(dfdt),c(46,8))
        expect_equal(names(dfdt),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "FUTURE_AADT","IRI","NHS"
                       )
                     )
        dftx <- extract_text(df)
        expect_equal(dim(dftx),c(35,6))
        expect_equal(names(dftx),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "ALTERNATIVE_ROUTE_NAME"
                       )
                     )
        dfc <- extract_comments(df)
        expect_equal(dim(dfc),c(76,19))
        expect_equal(names(dfc),
                     c( "Year_Record","State_Code","Route_ID",
                       "Begin_Point","End_Point",
                       "AADT",
                       "COUNTY_CODE",
                       "CRACKING_LENGTH",
                       "F_SYSTEM",
                       "IRI",
                       "K_FACTOR",
                       "NHS",
                       "OWNERSHIP",
                       "PSR",
                       "RUTTING",
                       "SURFACE_TYPE",
                       "TERRAIN_TYPE",
                       "THROUGH_LANES",
                       "URBAN_CODE"
                       )
                     )

    })
