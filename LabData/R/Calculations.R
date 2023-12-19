#' Calculations according to SIA 162/6
#'
#' This function returns tensile strength and fracture energy calculated according to
#' the standard SIA 162/6, based on the round slabs.
#'
#'
#' @param F_vect a vector containing force readings, kN
#' @param w_vect a vector containing displacement readings, mm
#' @param h0 thickness of the slab, m
#' @param lf lengt of fibres, mm
#' @param n number of cracks
#' @return A list of calculated values: fct, Gf
#' @export
calc.SIA162 <- function(F_vect, w_vect, h0, lf, n) {

  df <- data.frame(F = F_vect, w = w_vect / 1000)

  lf <- lf / 1000

  w1 <- (0.07 * n - 0.10) * lf

  df$diff_w <- c(0, diff(w_vect))
  df$Wi <- df$F * df$diff_w
  W1 <- max(cumsum(df[which.min(abs(df$w - w1)),]$Wi))

  fct <- 3 * W1 / (n * h0^2 * lf)

  w2 <- 4 * w1

  W2 <- max(cumsum(df[which.min(abs(df$w - w2)),]$Wi))

  Gf <- W2 / (3 * n * h0^2)

  results <- list(w1, W1, w2, W2, fct, Gf)

  return(results)

}