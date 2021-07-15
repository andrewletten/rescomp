#' Define consumer resource ODE function
#'
#' @param Time time to simulate over
#' @param State vector of initial states
#' @param Pars a list
#' @param essential (default = FALSE)
#' @param chemo (default = TRUE)
#'
#' @return
#' @export
#'
# #' @examples
def_cr_ode = function(Time, State, Pars, essential = FALSE, chemo = TRUE){
  with(as.list(c(State, Pars)),
       {
         nconsumers = Pars$nconsumers
         N = State[1:nconsumers]
         R = State[(1+nconsumers):length(State)]

         # Consumer dynamics
         dN.perR = mu #matrix(pars$mu[,1:length(R)])
         dN = N
         for(i in 1:length(N)){
           for(j in 1:length(R)){
             dN.perR[i,j] = (mu[i,j] * N[i] * (R[j])^(2*type3[i,j]))/((Ks[i,j])^(2*type3[i,j]) + phi[i,j]*(R[j])^(2*type3[i,j]))
           }
           if(essential == TRUE){
             dN[i] = min(dN.perR[i,]) - (all_d * N[i])
           } else {
             dN[i] = sum(dN.perR[i,]) - (all_d * N[i])
           }
         }

         # Resource dynamics
         dR.perN = mu
         dR = R
         if(essential == TRUE){
           for(j in 1:length(R)){
             for(i in 1:length(N)){
               dR.perN[i,] = (min(dN.perR[i,]))*Qs[i,]
             }
             if (chemo == TRUE){
               dR[j] = chemospeed*(chemoconc[j]-R[j]) - sum(dR.perN[,j])
             } else{
               dR[j] = (logisr[j]*R[j]*(1-(R[j]/logisK[j]))) - sum(dR.perN[,j])
             }
           }
         }
         else {
           for(j in 1:length(R)){
             for(i in 1:length(N)){
               dR.perN[i,j] = dN.perR[i,j]*Qs[i,j]
             }
             if (chemo == TRUE){
               dR[j] = chemospeed*(chemoconc[j]-R[j]) - sum(dR.perN[,j])
             } else{
               dR[j] = (logisr[j]*R[j]*(1-(R[j]/logisK[j]))) - sum(dR.perN[,j])
             }
           }
         }
         return(list(c(dN, dR)))
       })
}
