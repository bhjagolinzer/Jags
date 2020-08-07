library(readr)
library(dplyr)
library(testthat)
testthat::expect_that(object = filename,condition = isa("character"))
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
