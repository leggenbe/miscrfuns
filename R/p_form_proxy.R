#' Format arbitrary values Function
#'
#' This function allows you to add asterisks to arbitrary values; conditional on p-values.
#' @param lead Should leading zeroes be shown? FALSE
#' @param digits How many digits should the target value be rounded to? TRUE
#' @keywords asterisks
#' @export
#' @examples
#' p_form_proxy()

p_form_proxy <- function(pval, target, lead = T, digits = 2) {
  
  if(lead){
    roundy <- function(x, digits){format(round(x, digits), nsmall = digits)}
  } else {
    roundy <- function(x, digits){weights::rd(x, digits = digits)}
  }
  ifelse(pval >= .05 , roundy(target, digits),
                ifelse(pval >= .01 , paste0(roundy(target, digits), "*"),
                       ifelse(pval >= .001 , paste0(roundy(target, digits), "**"),
                              paste0(roundy(target, digits), "***"))))
}