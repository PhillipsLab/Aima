#' Analyze data from a lower body negative pressure trial.
#'
#' The required object for this function is the output of \link{load_labchart}.
#'
#' @param data a data frame output from \link{load_labchart} to be analyzed.
#' @param rate a numeric, the sampling rate of the data. Note if the data was
#'   downsampled out of \code{LabChart} the corrected sampling rate should be
#'   specified in frames per second. Default is 1000.
#' @param normalize_time a logical, if \code{TRUE} data will be interpolated
#'   to one datapoint per second using an internal function \link{match_interp}.
#'   If \code{FALSE} the existing sampling rate will be retained. Default is
#'   \code{TRUE}.
#' @param baseline_length a numeric, the length of time in seconds the user
#'   would like to use for the calculation of baseline statistics prior to
#'   the onset of the chamber. Default is 60 seconds.
#' @param trial_length a numeric, the length of time in seconds the user
#'   would like to specify that the trial proceeded for following the onset
#'   of the chamber. Default is 120 seconds.
#' @param n_steps a numeric, the number of chamber steps used in this
#'   lower-body negative pressure trial. Default is 2 steps.
#' @param step_length a numeric, the length of time in seconds that each
#'   step was sustained for prior to proceeding to the subsequent step or the
#'   trial finishing. Default is 60 seconds.
#' @param chamber_col a character, the name of the column containing the analog
#'   data of the chamber pressure. This will be used to identify the start
#'   and stop points of the chamber assessment. Default is 'Chamber'.
#' @param initial_chamber_pressure a numeric, the intial pressure specified
#'   for the lower-body negative pressure chamber. This value will be used
#'   to annotate the meta data. Default is 10 mmHg.
#' @param chamber_step a numeric, the size of pressure change between each
#'   step of the chamber during the trial. Default is 5 mmHg.
#' @return a data frame containing the lower-body negative pressure data with
#'   all the required meta data annotated.
#'
#' @importFrom purrr map_lgl
#' @importFrom magrittr %<>% extract
#' @importFrom dplyr mutate select left_join
#'
#' @export
#'

run_chamber = function(
    data,
    rate = 1000,
    normalize_time = T,
    # trial length parameters
    baseline_length = 60,
    trial_length = 120,
    n_steps = 2,
    step_length = 60,
    # chamber parameters
    chamber_col = 'Chamber',
    initial_chamber_pressure = 10,
    chamber_step = 5
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
  ### third, make sure we have the chamber in the data
  if (!chamber_col %in% colnames(data)) {
    stop("Please make sure that you have correctly identified the data column
      that contains the lower body negative pressure output. This can be
      defined in the argument 'chamber_col'.")
  }

  # first, preprocess the data
  data %<>%
    mutate(time = seq_len(nrow(.))) %>%
    dplyr::select(-comments)

  # since we are processing a chamber trial. Auto detect the first time
  # the chamber turns on
  detect_threshold = initial_chamber_pressure - 2
  chamber_start = which(data[[chamber_col]] > detect_threshold)[1]
  baseline_start = chamber_start - (rate * baseline_length)
  trial_end = chamber_start + (rate * trial_length)

  # subset the data
  data %<>% extract(seq(baseline_start, (trial_end-1)), )

  # we have setup a function that quickly enables users to adjust their dataset
  # to one datapoint per second. For hemodynamic analyses this is often useful.
  # However, users can keep their data at the original sampling rate by leaving
  # normalize set to 'F'.
  if (normalize_time) {
    data %<>%
      match_interp(data.frame(x = seq(1, (baseline_length + trial_length)))) %>%
      mutate(time = seq_len(nrow(.)))
  }

  # Here, we set the condition here based on our specific procedures.
  # However, users can specify the length of time and title of their
  # procedure. These are defined based on the arguments n_steps and step_length
  if (normalize_time) {
    data %<>%
      mutate(step = cut(time,
                             breaks = c(0, baseline_length,
                                        baseline_length + (step_length *
                                                             seq_len(n_steps))))
      ) %>%
      # re-code these
      mutate(step = as.character(factor(step, labels = c(
        'baseline',
        paste("Step", seq_len(n_steps))
      ))))
  } else {
    data %<>%
      mutate(step = cut(time,
                             breaks = c(0, (baseline_length * rate),
                                        (baseline_length * rate) +
                                          ((step_length * rate) *
                                          seq_len(n_steps))))
      ) %>%
      # re-code these
      mutate(step = as.character(factor(step, labels = c(
        'baseline',
        paste("Step", seq_len(n_steps))
      ))))
  }

  # Finally, we will add in the information about the chamber pressure,
  # according to the arguments: 'initial_chamber_pressure' and 'chamber_step'.
  chamber_pressure_map = data.frame(
    step = c('baseline', paste("Step", seq_len(n_steps))),
    chamber_pressure = c(0, initial_chamber_pressure,
                         (initial_chamber_pressure +
                            (chamber_step * seq_len(n_steps-1))))
                          )
  data %<>% left_join(chamber_pressure_map, by = 'step')

  return(data)

}
