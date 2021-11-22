#' Plot a line graph of a lower-body negative pressure trial.
#'
#' Visualize the change in a hemodynamic or sympathetic nervous system variable
#' that is specified in the output of \link{run_chamber}.
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
#' @param data a data frame of lower-body negative pressure data obtained from
#'   \link{run_chamber}.
#' @param features the features to plot. A separate plot will be provided for
#'   each feature. The feature must be present as a column name of \code{data}.
#' @param outcome the type of data to show. Please select from \code{raw}
#'   or \code{delta}. Defaults to \code{delta}.
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
#' @importFrom dplyr %>% group_by mutate mutate_ filter distinct select
#' @importFrom tidyr gather
#' @importFrom purrr pmap
#' @importFrom tibble tibble
#' @importFrom patchwork wrap_plots
#' @importFrom viridis scale_color_viridis scale_fill_viridis
#' @import ggplot2
#'
#' @export

plot_chamber_trace = function(
  data,
  features = NULL,
  outcome = 'delta',
  palette = "Greys",
  combine_plots = T
) {

  # check the input arguments
  if (is.null(features)) {
    stop("Please select at least one feature to plot")
  }

  # check for condition
  if (is.null(data$condition)) {
    data %<>% mutate(condition = '1')
  }

  # preprocess the data briefly
  data %<>%
    gather(variable, raw, -time, -condition, -step) %>%
    group_by(condition, variable) %>%
    mutate(delta = raw - raw[step == 'baseline']) %>%
    # adjust the column names
    mutate_('outcome' = outcome) %>%
    ungroup()

  # filter to the appropriate features
  data %<>% filter(variable %in% features)

  # grab some data needed to make a nice plot
  size_sm = 6
  size_lg = 7

  p = pmap(distinct(data %>% dplyr::select(-condition), variable), function(...) {
          current = tibble(...)
          var_label = unique(current$variable)
          p0 = filter(data,
                      variable == current$variable) %>%
          ggplot(aes(x = time, y = outcome, color = condition, fill = condition)) +
          geom_smooth(method = 'loess', span = 0.2, size = .2, fill = NA) +
          geom_vline(xintercept = 60, linetype = 'dotted', color = 'grey50') +
          geom_vline(xintercept = 120, linetype = 'dotted', color = 'grey50') +
          scale_x_continuous(expand = c(0,0), breaks = seq(0,180, 30)) +
          labs(x = "Time (s)", y = paste0(outcome, " ", var_label)) +
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
                legend.position = 'top',
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
                            discrete = T) +
        scale_color_viridis(option = palette,
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
                           na.value = 'white') +
        scale_color_brewer(palette = palette,
                          na.value = 'white')
    } else {
      p0 = p0 +
        scale_color_manual('', values = palette) +
        scale_fill_manual('', values = palette)
    }
    return(p0)
                     })

    if (combine_plots) {
      p %<>% wrap_plots()
    }

  return(p)
}
