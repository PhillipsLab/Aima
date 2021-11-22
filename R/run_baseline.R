#' Analyze data from a baseline assessment of hemodynamics and/or sympathetic
#' nerve activity.
#' 
#' The required object for this function is the output of \link{load_labchart}
#'
#' @param data a data frame output from \link{load_labchart} to be analyzed.
#' @param rate a numeric, the sampling rate of the data. Note if the data was
#'   downsampled out of \code{LabChart} the corrected sampling rate should be
#'   specified in frames per second. Default is 1000.
#' @param normalize_time a logical, if \code{TRUE} data will be interpolated
#'   to one datapoint per second using an internal function \link{match_interp}.
#'   If \code{FALSE} the existing sampling rate will be retained. Default is
#'   \code{TRUE}.
#' @param trial_length the length of the baseline trial in minutes. This will
#'   be used if \code{normalize_time} is \code{TRUE} to convert the data to a
#'   one datapoint per second structure. Default is 5 minutes. 
#' @return a data frame containing the baseline assessment data with or without
#'   interpolation
#'  
#' @importFrom purrr map_lgl
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate select %>%
#' 
#' @export
#' 

run_baseline = function(
    data,
    rate = 1000,
    normalize_time = T,
    trial_length = 5
  ) {

  # check the inputs
  ## first, check that all the columns are numeric
  x = data %>% dplyr::select(-comments)
  if (!all(map_lgl(x, is.numeric))) {
    stop("Make sure your input structure is correct. You should not have
      character strings except for the comments column")
  }
  ### second, check to make sure we don't have any NAs
  if (any(is.na(x))) {
    stop("Double check your data, there appears to be some NA values.
      These can be fixed by running the function `clean_artifacts()`")
  }

  # first, preprocess the data
  data %<>%
    mutate(time = seq_len(nrow(.))) %>%
    dplyr::select(-comments)

  # we have setup a function that quickly enables users to adjust their dataset
  # to one datapoint per second. For hemodynamic analyses this is often useful.
  # However, users can keep their data at the original sampling rate by leaving
  # normalize set to 'F'.
  if (normalize_time) {
    data %<>%
      match_interp(data.frame(x = seq(1, (trial_length * 60)))) %>%
      mutate(time = seq_len(nrow(.)))
  }

  return(data)

}
