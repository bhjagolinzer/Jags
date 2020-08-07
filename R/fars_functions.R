#' Read in a CSV file and convert it into a data frame
#'
#' @param filename a string - The name of a csv file to import
#' @return A data frame containing the information from the imported csv
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @note
#' This function will error out if the \code{filename} does not exist
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create file name for specific accident year
#'
#' @param year an integer - the year of accidents you are interested in
#' @return the name of a specific file to import
#' @note
#' This function will not work as intended if an \code{year} is not an integer
#' @examples
#' make_filename(2013)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'Read in files and create tibble of month and year combinations in each file
#'
#'@param years a vector of four digit integers - all the years of files you are interested in
#'@return a list of tibbles containing month/year combinations in the files specified by the \code{years} argument
#'@importFrom dplyr mutate select
#'@note
#' This function will not work as intended if an \code{years} is not an integer or a vector of integers
#' @examples
#' fars_read_years(c(2013,2014))
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#'Read in files and create tibble of counts of year/month combinations in all imported files
#'
#'@param years a vector of four digit integers - all the years of files you are interested in
#'@return a tibble containing counts of month/year combinations in the files specified by the \code{years} argument
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#'@note
#' This function will not work as intended if an \code{years} is not an integer or a vector of integers
#' @examples
#' fars_summarize_years(c(2013,2014))
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'Read in files and create visual of all accidents in state for a given year
#'
#'@param year a four digit integer - the year of accidents you are interested in
#'@param state.num an integer - the FIPS code for a US state
#'@return a visual of all the accidents in state for a given year by geographic location
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'@note
#' This function will not work as intended if an \code{year} is not an integer or \code{state.num} is not an integer from 1-56
#' @examples
#' fars_map_state(1,2013)
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
