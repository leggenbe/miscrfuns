#' Round Function
#'
#' This function allows you to round numbers more easily w/o leading and trailing zeroes.
#' @param digits Number of digits to show 0
#' @param lead Should leading zeroes be shown? TRUE
#' @keywords round
#' @export
#' @examples
#' roundy()

roundy <- function(x, digits = 0, lead = T){
  if(lead){format(round(x, digits = digits), nsmall = digits, trim = T)} 
  else {weights::rd(x, digits = digits)}
}
