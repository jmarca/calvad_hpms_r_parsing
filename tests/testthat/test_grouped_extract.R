fname <- c('./files/2011.csv'
          ,'./files/2012.csv'
          ,'./files/2013.csv')


test_that(
    'can load (some) of csv file for 2012',
    {
        filename <-  fname[2]
        df <- read_file(filename)
        df <- whitespace_fix(df)
        df_spread <- grouped_extract(df)

        dim_df_spread <- dim(df_spread)
        ## well what do I expect?
        numrows <- dim(unique(df[,c('Route_ID','Begin_Point','End_Point')]))[1]
        expect_equal(dim_df_spread,c(numrows,36))

    })

test_that(
    'can load (some) of csv file for 2011',
    {
        filename <-  fname[1]
        df <- read_file(filename,dt_format="%m%.%d%.%Y %H%.%M%.%S %p")
        df <- whitespace_fix(df)
        df_spread <- grouped_extract(df)

        dim_df_spread <- dim(df_spread)
        ## well what do I expect?
        numrows <- dim(unique(df[,c('Route_ID','Begin_Point','End_Point')]))[1]
        expect_equal(dim_df_spread,c(numrows,27))
    })

test_that(
    'can load (some) of csv file for 2013',
    {
        filename <-  fname[3]
        df <- read_file(filename,delim='|')
        df <- whitespace_fix(df)
        df_spread <- grouped_extract(df)

        dim_df_spread <- dim(df_spread)

        ## well what do I expect?
        numrows <- dim(unique(df[,c('Route_ID','Begin_Point','End_Point')]))[1]
        numcols <- length(intersect(unique(df$Data_Item),is_keeper))
        expect_equal(dim_df_spread,c(numrows,37))

    })
