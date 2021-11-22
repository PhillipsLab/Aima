#' Plot a bar graph of summary statistics

#' Visualize the results of summary statistics for a baseline or lower-body
#' negative pressure assessment from the outputs \link{analyze_baseline} or
#' \link{analyze_chamber}.
#'
#' The following color maps can be specified using the \code{palette} argument:
#' \enumerate{
#'   \item viridis color scales: magma, inferno, plasma, viridis, or cividis
#'     (the default)
#'   \item sequential ColorBrewer scales: BuGn, BuPu, GnBu, Greens, Greys,
#'     Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu,
#'     YlOrBr, or YlOrRd
#'   \item diverging ColorBrewer scales: BrBG, PiYG, PRGn, PuOr, RdBu, RdGy,
#'     RdYlBu, RdYlGn, or Spectral
#'   \item a vector of colors that will be passed into
#'     \code{scale_color_manual}
#' }
#'
#' @param data a summarised data frame of a baseline or lower-body negative
#'   pressure assessment obtained from \link{analyze_baseline} or
#'   \link{analyze_chamber}.
#' @param features the features to plot. A separate plot will be provided for
#'   each feature. The feature must be present as a column name of \code{data}.
#' @param outcome the type of data to show. Please select from \code{delta}
#'   or \code{delta_min} or \code{delta_max}. Defaults to \code{delta}.
#' @param palette the color palette used to visualize the trace for each variable
#'   type. Can be provided either a string specifying a ColorBrewer (see
#'   \code{\link{scale_color_brewer}}) or viridis (see
#'   \code{\link{scale_color_viridis}}) color palette, or a vector of
#'   colors that will be provided to \code{\link{scale_color_manual}}
#' @param combine_plots optionally, combine the plots into a single object.
#'   Please note that this will prevent users from adding ggplot layers.
#'   Defaults to \code{TRUE}.
#'
#' @return a \code{ggplot} object.
#'
#' @importFrom magrittr %<>%
#' @importFrom dplyr %>% mutate mutate_ filter distinct
#' @importFrom purrr pmap
#' @importFrom tibble tibble
#' @importFrom patchwork wrap_plots
#' @importFrom viridis scale_color_viridis scale_fill_viridis
#' @import ggplot2
#'
#' @export

plot_summary_bars = function(
  data,
  features = NULL,
  outcome = 'delta',
  palette = 'Greys',
  combine_plots = T
) {

  # check the input arguments
  if (is.null(features)) {
    stop("Please select at least one feature to plot")
  }

  # adjust the column names
  data %<>% mutate_('outcome' = outcome)

  # filter to the appropriate features
  data %<>% filter(variable %in% features)

  # grab some data needed to make a nice plot
  size_sm = 6
  size_lg = 7

  # if we are dealing with a negative pressure trial, plot steps separately.
  if (!suppressWarnings(is.null(data$step))) {
    # if we are plotting delta, do not plot baseline
    if (grepl("delta", outcome)) {
      data %<>% filter(step != 'baseline')
    }
    data0 = distinct(data, variable, step)
  } else {
    data0 = distinct(data, variable) %>% mutate(step = 'baseline')
    data %<>% mutate(step = 'baseline')
  }

  p = pmap(data0, function(...) {
               current = tibble(...)
               var_label = unique(current$variable)
               p0 = filter(data,
                      variable == current$variable,
                      step == current$step) %>%
                 mutate(outcome = ifelse(outcome == 0, 0.1, outcome)) %>%
                 ggplot(aes(x = condition, y = outcome,
                            fill = condition)) +
                 facet_wrap(~ step) +
                 geom_col(width = 0.7, size = 0.3, color = NA) +
                 coord_fixed() +
                 labs(x = 'Condition', y = paste0(outcome, " ", var_label)) +
                 scale_y_continuous(expand = c(0.01,0)) +
                 guides(color = F, fill = F) +
                 theme_bw() +
                 theme(axis.title.x = element_text(size = size_lg),
                       axis.title.y = element_text(size = size_lg),
                       panel.grid = element_blank(),
                       panel.border = element_blank(),
                       strip.text = element_text(size = size_lg),
                       strip.background = element_blank(),
                       axis.line.y = element_line(colour = "grey50", size = .2),
                       axis.line.x = element_line(colour = "grey50", size = .2),
                       axis.ticks = element_line(colour = "grey50", size = .2),
                       legend.position = 'right',
                       legend.justification = 'bottom',
                       legend.text = element_text(size = size_sm),
                       legend.title = element_text(size = size_sm),
                       legend.key.size = unit(0.25, "lines"),
                       legend.margin = margin(rep(0, 4)),
                       legend.background = element_blank(),
                       plot.title = element_text(size = size_lg, hjust = 0.5),
                       aspect.ratio = 1)

     ## add color scheme
    if (length(palette) == 1 &&
        palette %in% c("viridis", "cividis", "plasma", "magma", "inferno")) {
      p0 = p0 +
        scale_fill_viridis(option = palette,
                            na.value = 'white',
                            discrete = T)
    } else if (length(palette) == 1 &&
               palette %in% c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                              "RdYlBu", "RdYlGn", "Spectral", "Blues", "BuGn",
                              "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                              "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu",
                              "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")) {
      p0 = p0 +
        scale_fill_brewer(palette = palette,
                           na.value = 'white')
    } else {
      p0 = p0 +
        scale_fill_manual('', values = palette)
    }
                     })

    if (combine_plots) {
      p %<>% wrap_plots()
    }

  return(p)
}
