#' Parāda grafiku visiem kanāliem
#'
#' Šī funkcija parāda grafisko attēlojumu visiem RissFox kanāliem pēc laika
#'
#'
#' @param x Dati 'data frame' formātā
#' @return ggplot plot
#' @export
rissFoxPlot <- function(x) {
  # Datu kopas pazīmes, kas iegūtas no kolonnām
  pazimes <- colnames(x)[which(colnames(x)=="Time"):length(colnames(x))]

  color_names <- c("black", "dark green", "blue", "red", "orange", "purple")
  color_id <- 0

  p <- ggplot2::ggplot(x, ggplot2::aes(x = Time))

  for (pazime in pazimes[2:length(pazimes)]){
    color_id <- color_id + 1
    p <- p + ggplot2::geom_path(ggplot2::aes_string(y = pazime),
                                color = color_names[color_id])
  }


  p+ggplot2::scale_x_datetime(date_breaks = "6 hours")



  return(p)

}


