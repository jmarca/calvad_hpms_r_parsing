fname <- c('./files/small.csv')


test_that(
    'can process out dupes',
    {
        filename <-  fname[1]
        df <- read_file(filename)
        df <- whitespace_fix(df)
        df_spread <- grouped_extract(df)
        expect_equal(dim(df_spread),c(13,34))

    })
