#' Analyze data from a lower body negative pressure trial
#'
#' The required object for this function is the output of \link{run_chamber}.
#'
#' @param data a data frame output from \link{run_chamber} to be summarised.
#' @param drop_stimulus a logical, if \code{TRUE} the initial respond to the
#'   chamber pressure will be ignored in the summary statistics. If \code{FALSE}
#'   the entire trial will be used in the calculation of summary statistics.
#'   Default is \code{TRUE}.
#' @return a data frame containing summary statistics for all variables.
#'
#' @importFrom zoo na.locf
#' @importFrom magrittr %<>%
#' @importFrom dplyr select_ group_by summarise_all mutate
#'   filter select %>% ungroup summarise
#' @importFrom tidyr gather
#' @export
#'

analyze_chamber = function(
  data,
  drop_stimulus = T
) {

    # check for condition
    if (is.null(data$condition)) {
      data %<>% mutate(condition = '1')
    }

  # optionally, we will ignore the immediate drop after the stimulus
  # and only take the outcome measures once the pressure has stabilized
  if (!drop_stimulus) {
   sum = data %>%
      dplyr::select(-time) %>%
      gather(variable, value, -condition, -step) %>%
      group_by(variable, step, condition) %>%
      summarise(mean = mean(value), min = min(value), max = max(value)) %>%
      # add delta in
      group_by(condition, variable) %>%
      mutate(delta = mean - mean[step == 'baseline'],
             delta_max = max - mean[step == 'baseline'],
             delta_min = min - mean[step == 'baseline']) %>%
      ungroup()
  } else {
    sum = data %>%
      group_by(step) %>%
      mutate(drop = ifelse(step == 'baseline',
                           1, as.numeric(cut(time, 2))-1)) %>%
      filter(drop == 1) %>%
      dplyr::select(-time, -drop) %>%
      gather(variable, value, -condition, -step) %>%
      group_by(variable, step, condition) %>%
      summarise(mean = mean(value), min = min(value), max = max(value)) %>%
      # add delta in
      group_by(condition, variable) %>%
      mutate(delta = mean - mean[step == 'baseline'],
             delta_max = max - mean[step == 'baseline'],
             delta_min = min - mean[step == 'baseline']) %>%
      ungroup()
  }

  return(sum)
}
