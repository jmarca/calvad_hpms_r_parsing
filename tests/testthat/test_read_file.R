config <- rcouchutils::get.config(Sys.getenv('RCOUCHUTILS_TEST_CONFIG'))
year <- 2012

fname <- c('./files/2012.csv','./files/2013.csv','./files/2011.csv')


test_that(
    'can read 2012 file',
    {
        result <- read_file(fname[1])
        expect_is(result,'data.frame')
        expect_that(dim(result),equals(c(999,11)))
        expect_that(names(result),
                    equals(
                        c("Year_Record"   ,"State_Code"
                         ,"Route_ID"      ,"Begin_Point"
                         ,"End_Point"     ,"Data_Item"
                         ,"Section_Length","Value_Numeric"
                         ,"Value_Text"    ,"Value_Date"
                         ,"Comments"  )))
    })

test_that(
    'can read 2013 file',
    {
        result <- read_file(fname[2],delim='|')
        expect_is(result,'data.frame')
        expect_that(dim(result),equals(c(999,11)))
        expect_that(names(result),
                    equals(
                        c("Year_Record"   ,"State_Code"
                         ,"Route_ID"      ,"Begin_Point"
                         ,"End_Point"     ,"Data_Item"
                         ,"Section_Length","Value_Numeric"
                         ,"Value_Text"    ,"Value_Date"
                         ,"Comments"  )))
    })

test_that(
    'can read 2011 file',
    {
     ## need github version of readr for the %p format
        result <- read_file(fname[3],dt_format="%m%.%d%.%Y %H%.%M%.%S %p")
        expect_is(result,'data.frame')
        expect_that(dim(result),equals(c(999,11)))
        expect_that(names(result),
                    equals(
                        c("Year_Record"   ,"State_Code"
                         ,"Route_ID"      ,"Begin_Point"
                         ,"End_Point"     ,"Data_Item"
                         ,"Section_Length","Value_Numeric"
                         ,"Value_Text"    ,"Value_Date"
                         ,"Comments"  )))
    })
