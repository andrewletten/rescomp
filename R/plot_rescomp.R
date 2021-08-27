#' Plot consumer and resource dynamics from deSolve output
#'
#' @param model List output from `sim_rescomp()`. First element is an object of
#' class deSolve. Second element is an object of class rescomp.
#' @param consumers Plot consumer dynamics? Default = TRUE.
#' @param resources Plot resource dynamics? Default = TRUE.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
#' pars <- spec_rescomp()
#' m1 <- sim_rescomp(pars)
#' plot_rescomp(m1)
#'
plot_rescomp <- function(model, consumers = TRUE, resources = TRUE){
  plot.df <-  frame_and_name(model)
  comp.gg <- tidyr::pivot_longer(plot.df, cols = !c(.data$time), names_to = "state.var", values_to = "count")
  comp.gg$state.var.type <- "Consumers"
  comp.gg$state.var.type[grep("R", comp.gg$state.var)] <- "Resources"

  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  resSet1 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628")

  if(consumers == TRUE & resources == FALSE){
    comp.gg <- comp.gg[comp.gg$state.var.type != "Resources",]
  } else if (consumers == FALSE & resources == TRUE){
    comp.gg <- comp.gg[comp.gg$state.var.type != "Consumers",]
  }

  ggplot2::ggplot(comp.gg,
    aes(y = .data$count, x = .data$time)) +

    geom_line(aes(group = .data$state.var, col = .data$state.var),
      size = 1,
      alpha=0.9) +

    # scale_y_log10() +
    # theme(legend.position="none") +

    ylab("Population size") +

    xlab("Time") +

    theme_bw() +
    theme(strip.background = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          legend.title = element_blank()) +

    #  panel_border(colour = "black") + #xlim(800,1400) +

    facet_wrap(
      ~ state.var.type,
      scales = "free") +

    scale_colour_manual(values=c(cbbPalette, resSet1))
}
