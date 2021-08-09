#' Plot functional responses
#'
#' @param pars Parameter list from make_par_list()
#' @param maxx Resource value to calculate per-capita growth rates up to (xlim).
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
#' pars <- make_par_list()
#' plot_funcresp(pars)
#'
#' pars <- make_par_list(
#'     spnum = 2,
#'     resnum = 2,
#'     linear = FALSE,
#'     mumatrix = list(matrix(c(0.7,0.3,
#'                              0.4,0.5),
#'                            nrow = 2,
#'                            ncol = 2,
#'                            byrow = TRUE))
#' )
#' plot_funcresp(pars)
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

