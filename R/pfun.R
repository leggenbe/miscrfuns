p_fun <- function(x, lead = F, stars = T) {
  ifelse(stars,
         case_when(x < 0.001 ~ paste0("<", ifelse(lead,"0.001***", ".001***")),
                   x < 0.01 ~ paste0(roundy(x, 3, lead), "**"),
                   x < 0.05 ~ paste0(roundy(x, 3, lead), "*"),
                   TRUE ~ roundy(x, 3, lead)),
         case_when(x < 0.001 ~ paste0("<", ifelse(lead,"0.001", ".001")),
                   TRUE ~ roundy(x, 3, lead))
  )
}