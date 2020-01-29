#' Load packages
#'
#' This function is been used to load the required dataset(s) that is used
#' in other functions of the package
#' @param A CSV file that contain all the data
#' @return A tibble (data.frame) dataset or an error message if the dataset
#'     does'nt exist!
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @example \dontrun{
#' fars_read("/data/accident_2015.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Name datasets
#'
#' A function that create the name(s) of the file(s)/datasets that will
#' be used.
#'
#' @param year a vector of one or multiple years
#' @return a vector containing one or more fileName values
#' @examples \dontrun{
#' make_filename(2019)
#' make_filename(c(2011, 2012, 2013))
#' }
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' Read years
#'
#' This function read multiple year(s)/dataset(s) and check if they exist(s)
#'
#' @param years A integer vector containing 1 or more years datasets. If
#'     a year or more do not exist it returns a error message!
#' @return A list containing the values of month and year for each year(s) entered
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @examples \dontrun{
#' m_years <- c( 2011, 2012, 2013, 2014)
#' fars_read_years(m_years)
#'
#' fars_read_years(2015)
#' }
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


#' Summarize datasets
#'
#' This functions summarize the dataset(s) by years and month.
#'
#' @param years A integer representing a specific year
#' @return A tibble that contains the count of measures per month and year
#' @importFrom dplyr bind_rows group_by summarize spread
#' @importFrom magrittr %>%
#' @examples \dontrun{
#' x <- c(2014, 2015)
#' fars_summarize_years(x)
#'
#' fars_summarize_years(2013)
#' }
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}



#' Draw a map
#'
#' Function that return the map of a state accidents in given year(s)
#'
#' @param state.num An integer representing a US State
#' @param year A integer that represent a year we are looking the data for
#' @return map a map object. If the state id is not correct the function stops.
#'     No measures return as message.
#' @importFrom dplyr select
#' @importFrom maps map
#' @importFrom graphics point
#' @example \dontrun{fars_map_state(12, 2013)}
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
