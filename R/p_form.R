#' Format p-values Function
#'
#' This function allows you to format p-values w/o stars and leading/trailing zeroes.
#' @param lead Should leading zeroes be shown? FALSE
#' @param stars Should stars/asterisks be added? TRUE
#' @keywords pvalues
#' @export
#' @examples
#' p_form()

p_form <- function(x, lead = F, stars = T) {
  ifelse(stars,
         case_when(x < 0.001 ~ paste0("<", ifelse(lead,"0.001***", ".001***")),
                   x < 0.01 ~ paste0(roundy(x, 3, lead), "**"),
                   x < 0.05 ~ paste0(roundy(x, 3, lead), "*"),
                   TRUE ~ roundy(x, 3, lead)),
         case_when(x < 0.001 ~ paste0("<", ifelse(lead,"0.001", ".001")),
                   TRUE ~ roundy(x, 3, lead))
  )
}