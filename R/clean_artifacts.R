#' Function to clean artifacts and NAs in a file loaded by \link{load_labchart}
#'
#' Specify a threshold for artifacts and check for NA/NaNs in the data
#'
#' @param data a data frame to be cleaned.
#' @param n_sd the threshold to define an artifact. Default is 5.
#'   to
#' @return a cleaned data frame.
#'
#' @importFrom zoo na.locf
#' @importFrom magrittr %<>%
#' @importFrom dplyr select mutate_all mutate %>%
#' @importFrom stats sd
#'

clean_artifacts = function(data, n_sd = 5) {

  # setup an internal function to clean up each column
  clean_col = function(x, n_sd) {
    if (is.numeric(x)) {
      dvec = c(0,diff(x, 1))
      outliers = which(abs(dvec) > n_sd*(sd(dvec)))
      x[outliers] = NA
      return(x)
    }
  }

  # grab the comments and time to avoid interpolation issues
  comments = data$comments
  time = data$time
  data %<>% dplyr::select(-comments, -time)

  # clean up all the data
  data %<>%
    # make sure we don't have any NaNs
    mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
    # clean all the cols
    mutate_all( clean_col, n_sd) %>%
    # do a two part NA removal
    na.locf(na.rm = F) %>%
    na.locf(na.rm = F, fromLast = T) %>%
    mutate(comments = comments, time = time)
}
