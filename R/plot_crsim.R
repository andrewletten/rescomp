#' Plot consumer and resource dynamics from deSolve output
#'
#' @param odeobj odeobject
#' @param pars parameters from make_par_list()
#'
#' @return ggplot object
#' @export
#'
# #' @examples
plot_crsim <- function(odeobj, pars){
  plot.df <-  frame_and_name(odeobj, pars)
  comp.gg <- tidyr::pivot_longer(plot.df, cols = !c(.data$time), names_to = "state.var", values_to = "count")
  comp.gg$state.var.type <- "Consumers"
  comp.gg$state.var.type[grep("R", comp.gg$state.var)] <- "Resources"

  cbbPalette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ggplot2::ggplot(comp.gg,
    aes(
      y = .data$count,
      x = .data$time)
    ) +

    geom_line(
      aes(
        group = .data$state.var,
        col = .data$state.var
        ),
      size = 1,
      alpha=0.9) +

    # scale_y_log10() +
    # theme(legend.position="none") +

    ylab("Population size") +

    xlab("Time") +

    #  panel_border(colour = "black") + #xlim(800,1400) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10)) +

    facet_wrap(
      ~ state.var.type,
      scales = "free") +

    scale_colour_manual(values=cbbPalette)
}
