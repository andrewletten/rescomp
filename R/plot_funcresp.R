#' Plot functional responses
#'
#' @param pars S3 object of class `rescomp` detailing model parameters and
#'     specifications.
#' @param maxx Numeric vector of length 1.
#'     Resource value to calculate per-capita growth rates up to (xlim).
#' @param display_values Named list of vectors, with names matching names of pars$params.
#'    Each vector gives the values of the respective model parameters at which to plot the functional responses.
#'    Defaults are automatically inferred for most `rescomp_param` objects, but may be overwritten.
#'    `rescomp_param_custom` parameters must have their display values specified here if they were not defined at definition.
#' @param madj Logical vector of length 1. Whether to standardise per capita growth rates by mortality.
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @return A ggplot object.
#' @export
#'
#' @details
#' It is assumed that the `funcresp` of `pars` is constructed such that the growth rate of a species on a given resource depends only on the concentration of that resource, and not on other resources.
#' This is the case for all built-in functional responses, but is not necessary the case if using `funcresp_custom()`.
#' Plots are likely to be nonsensical or incorrect if this assumption is violated.
#'
#' @examples
#' pars <- spec_rescomp()
#' plot_funcresp(pars)
#'
#' pars <- spec_rescomp(
#'   spnum = 2,
#'   resnum = 2,
#'   funcresp = "type2",
#'   mumatrix = list(matrix(
#'     c(
#'       0.7, 0.3,
#'       0.4, 0.5
#'     ),
#'     nrow = 2,
#'     ncol = 2,
#'     byrow = TRUE
#'   ))
#' )
#' plot_funcresp(pars)
#' plot_funcresp(pars, madj = TRUE)
#' # TODO: An example with display_values.
plot_funcresp <- function(pars, maxx = 1, display_values, madj = FALSE) {
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  resSet1 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628")

  df <- df_funcresp(pars, maxx, display_values, madj)
  mortdf <- df_mort(pars, display_values)

  df$sp <- get_species_names(df$sp)
  df$res <- get_resource_names(df$res)
  mortdf$sp <- get_species_names(mortdf$sp)

  resources_formula <- ifelse(pars$resnum > 1,
    "res",
    "."
  )
  parameters_formula <- ifelse(length(pars$params) > 0,
    paste(names(df)[!names(df) %in% c("sp", "res", "x", "y")], collapse = "+"),
    "."
  )
  facet_formula <- as.formula(paste0(resources_formula, "~", parameters_formula))

  p <- ggplot2::ggplot(df, aes(x = .data$x, y = .data$y)) +
    geom_line(aes(col = .data$sp), linewidth = 1, alpha = 0.8) +
    facet_grid(facet_formula, labeller = label_both) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      strip.background = element_blank(),
    ) +
    xlab("Resource concentration") +
    ylab("Per capita growth rate") + # TODO: Adjust this for essential = TRUE/FALSE.
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10)
    ) +
    coord_cartesian(expand = FALSE) +
    scale_colour_manual(values = c(cbbPalette, resSet1))

  if (madj == TRUE) {
    p <- p + geom_hline(yintercept = 0, col = "grey")
  } else {
    p <- p + geom_hline(data = mortdf, aes(yintercept = .data$mort, col = .data$sp), linetype = "dashed")
  }

  return(p)
}
