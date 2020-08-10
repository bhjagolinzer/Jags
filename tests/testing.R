require(testthat)
filename <- 'fakefile.csv'
expect_that(object = filename,condition = is_a("character"))
