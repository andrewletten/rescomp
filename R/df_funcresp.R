#' Generate ggplot friendly dataframe for plotting functional responses
#'
#' @param pars parameter list from make_par_list()
#' @param maxx maximum resource value to get percapita growth rates (for plotting)
#'
#' @return data frame
#' @export
#'
#' @importFrom rlang .data
df_funcresp <- function(pars, maxx){
  if (missing(maxx)){
    resource.levels <- seq(0,0.2,length.out = 1000)
  } else{
    resource.levels <- seq(0,maxx,length.out = 1000)
  }

  resp.iter <-  data.frame(matrix(nrow = length(resource.levels), ncol = pars$nresources))
  resp.iter$resource.levels <-  resource.levels
  resp.list <- list()
  resp.list.TDP <- list()

  for (k in 1:length(pars$mu)){
  for (i in 1:pars$nconsumers){
    for (j in 1:pars$nresources){
      resp.iter[,j] = func_form(R = resource.levels,
                                mu = pars$mu[[k]][i,j],
                                Ks = pars$Ks[i,j],
                                #aff = pars$aff[i,j],
                                phi = pars$phi[i,j],
                                type3 = pars$type3[i,j])
      names(resp.iter)[j] <-  paste0("Resource ", letters[c(1:pars$nresources)[j]])
    }
    resp.iter$sp <-  paste0("N", c(1:pars$nconsumers)[i])
    resp.iter$paramstate <- paste0("env-state-", k)
    resp.list[[i]] <-  resp.iter
  }
    resp.list.TDP[[k]] <-  do.call("rbind", resp.list)
  }
  resp.all <- do.call("rbind", resp.list.TDP)
  tidyr::pivot_longer(resp.all, cols = !c(.data$resource.levels, .data$sp, .data$paramstate), names_to = "resource", values_to = "growth")
}
