fname <- c('./files/2011.csv'
          ,'./files/2012.csv'
          ,'./files/2013.csv')


test_that(
    'can load (some) of csv file for 2011',
    {
        result <- parse_file(fname[1],dt_format="%m%.%d%.%Y %H%.%M%.%S %p")
        expect_is(result,'list')
        expect_that(length(result),equals(4))
        expect_is(result[[1]],'data.frame')
        expect_is(result[[2]],'data.frame')
        expect_is(result[[3]],'data.frame')
        expect_is(result[[4]],'data.frame')
        expect_that(names(result),
                    equals(
                        c('data_numeric',
                          'data_date',
                          'data_text',
                          'data_comments')))

    })
test_that(
    'can load (some) of csv file for 2012',
    {
        result <- parse_file(fname[2])
        expect_is(result,'list')
        expect_that(length(result),equals(4))
        expect_is(result[[1]],'data.frame')
        expect_is(result[[2]],'data.frame')
        expect_is(result[[3]],'data.frame')
        expect_is(result[[4]],'data.frame')
        expect_that(names(result),
                    equals(
                        c('data_numeric',
                          'data_date',
                          'data_text',
                          'data_comments')))
    })
test_that(
    'can load (some) of csv file for 2013',
    {
        result <- parse_file(fname[3],delim="|")
        expect_is(result,'list')
        expect_that(length(result),equals(4))
        expect_is(result[[1]],'data.frame')
        expect_is(result[[2]],'data.frame')
        expect_is(result[[3]],'data.frame')
        expect_is(result[[4]],'data.frame')
        expect_that(names(result),
                    equals(
                        c('data_numeric',
                          'data_date',
                          'data_text',
                          'data_comments')))
        expect_equal(names(result$data_comments),
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
