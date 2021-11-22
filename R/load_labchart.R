#' Load txt files exported from LabChart
#'
#' Convert an exported LabChart text file to a user-friendly structure that is
#' compatible with downstream functions from Aima.
#'
#' @param file an exported file of type \code{.txt} from \code{LabChart}
#'   to be processed.
#' @param col_names a vector specifying the column names of the file. These
#'   correspond to the channels defined in \code{LabChart}.
#' @return a tibble with column names set to \code{col_names}.
#'
#' @importFrom readr read_delim
#' @importFrom magrittr %<>%
#' @importFrom readr cols
#'
#' @export
#'

load_labchart = function(file, col_names = NULL) {

  # Check for column names
  if (is.null(col_names)) {
    stop("Please specify the input columns for your data")
  }

  # Load labchart data saved as txt
  data = suppressWarnings(
      read_delim(file, "\t", skip=9, col_names=col_names,
    col_types = cols())
  )

  # clean the data
  data %<>% clean_artifacts()

  return(data)
}
