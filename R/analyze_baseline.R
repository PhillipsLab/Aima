#' Analyze data from a baseline assessment of hemodynamics and/or sympathetic
#' nerve activity.
#'
#' The required object for this function is the output of \link{run_baseline}
#'
#' @param data a data frame output from \link{run_baseline} to be summarised.
#' @return a data frame containing summary statistics for all variables.
#'
#' @importFrom dplyr group_by mutate select %>% ungroup summarise
#' @importFrom tidyr gather
#' @importFrom magrittr %<>%
#'
#' @export
#'

analyze_baseline = function(
  data
) {

  # check for condition
  if (is.null(data$condition)) {
    data %<>% mutate(condition = '1')
  }
  # summarise the data
   sum = data %>%
      dplyr::select(-time) %>%
      gather(variable, value, -condition) %>%
      group_by(condition, variable) %>%
      summarise(mean = mean(value), min = min(value), max = max(value)) %>%
      ungroup()
  return(sum)
}
