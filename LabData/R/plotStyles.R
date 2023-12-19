#' Plot Theme for publications
#'
#' This function defines theme to be used in ggplot function.
#'
#'
#' @return A ggplot theme.
#' @export
theme_paper <- function () {
  ggplot2::theme(text = ggplot2::element_text(size=10, family = "serif"),
                 panel.grid.minor = ggplot2::element_blank(),
                 # add border 1)
                 panel.border = ggplot2::element_rect(colour = "black", fill = NA),
                 # color background 2)
                 panel.background = ggplot2::element_rect(fill = "white"),
                 # modify grid 3)
                 panel.grid.major.x = ggplot2::element_line(colour = "black", linetype = 3, size = 0.5),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.y =  ggplot2::element_line(colour = "black", linetype = 3, size = 0.5),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 legend.key = ggplot2::element_rect(fill = "white"),
                 # legend.position = c(0.6, 0.15),
                 # legend.title = element_blank(),
                 legend.spacing.y = ggplot2::unit(-2, "pt"),
                 strip.text = ggplot2::element_text(color = "black"),
                 strip.background = ggplot2::element_blank()
  )
}