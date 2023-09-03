#' Reliability Function
#'
#' This function calculates different estimates for reliability/internal consistency coefficients.
#' @param dat Dataframe containing the relevant variables
#' @param fm Method for factor structure estimation (default: minres)
#' @param nfactors Number of factors to estimate omega for (default: 1)
#' @keywords reliability
#' @export
#' @examples
#' reliabs()

reliabs <- function(dat, fm = "minres", nfactors = 1){
  temp <- psych::omega(dat, fm = fm, plot = F, warnings = F, two.ok = T, nfactors = nfactors)
  res <- roundy(data.frame("Alpha" = temp$alpha,
                           "Guttman.6" = temp$G6,
                           # "Omega.h" = temp$omega_h,
                           "Oemga.tot" = temp$omega.tot),2)
  return(res)
}