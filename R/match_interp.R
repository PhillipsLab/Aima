#' Function for interpolation of physiological data
#'
#' Interpolate all data columns to a specified length using linear interpolation.
#'
#' @param data a data frame containing only numeric columns to be interpolated.
#' @param data2 a data frame of length \code{x}, the length to be interpolated.
#'   to
#' @return a data frame of length nrow(data2).
#'
#' @importFrom stats approx
#' @importFrom dplyr %>%
#' @importFrom purrr map
#'

match_interp = function(data, data2) {
  spacing = nrow(data) / nrow(data2)
  newdata = data %>%
    map(function(x) approx(x = seq(1,nrow(data)),
                    xout = round(seq(1, nrow(data), spacing)),
                    y = x,
                    method = "linear")$y) %>%
    as.data.frame()
  # Check to make sure lengths are the same
  if (nrow(newdata) != nrow(data2)) {
    newdata = newdata[1:nrow(data2),]
  }
  return(newdata)
}
