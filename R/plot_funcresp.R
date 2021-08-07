#' Plot functional responses
#'
#' @param pars parameter list from make_par_list()
#' @param maxx maximum resource value to get percapita growth rates for (for plotting)
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
#'
plot_funcresp <- function(pars, maxx){

  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  df <- df_funcresp(pars, maxx)

  ggplot2::ggplot(df, aes(y = .data$growth, x = .data$resource.levels)) +
    geom_line(aes(col = .data$sp), size = 1, alpha=0.8) +
    theme(legend.title = element_blank(),
          strip.background = element_blank(),) +
    xlab("Resource concentration") +
    ylab("Per capita growth rate") +
    geom_hline(yintercept = pars$all_d, linetype = "dashed") +
    theme(axis.text = element_text(size = 8),
          axis.title= element_text(size = 10)) +
    coord_cartesian(expand = FALSE) +
    facet_grid(paramstate ~ resource) +
    scale_colour_manual(values=cbbPalette)
}

