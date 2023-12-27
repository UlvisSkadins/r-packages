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


#' Residual strength according to EN 14651
#'
#' This function returns residual tensile strength of notched prisms according to EN 14651.
#'
#'
#' @param F load in three point bending test, kN
#' @param b width of the prism, mm
#' @param hsp height of the prism at the notch (from the tip of the notch), mm
#' @param l span of the prism (distance between the supports), mm
#' @return residual strength f_Ri
#' @export
calc.fRi <- function (F, b, hsp, l) {
  return(3/2 * F * 1000 * l / (b * hsp^2))
}

#' Modulus of elasticity of concrete
#'
#' This function calculates modulus of elasticity of concrete according to EC2 (2023) (5.1).
#'
#'
#' @param fck concrete compressive strength (characteristic value).
#' @param kE coefficient considering type of aggregates.
#' @return modulus of elasticity Ecm
#' @export
calc.Ecm <- function (fcm, kE = 9500) {
  # This formula is according ot EC2 2023
  return(kE * (fcm)^(1/3))
}



#' FRC post-cracking strength class
#'
#' Returns FRC post-cracking strength class according to Model Code 2010.
#'
#'
#' @param fR1k residual strengths significant for serviceability.
#' @param fR3k residual strengths significant for ultimate conditions.
#' @return A string representing FRC post-cracking strength class.
#' @export
calc.FRC.class <- function (fR1k, fR3k) {
  # Strength intervals
  strenght_intervals <- c(seq(1,3,0.5), seq(4,20,1))

  # Strength ratios
  ratios <- seq(0.5, 1.5, 0.2)
  # Number of the items in the ratios vector
  number_of_rartios <- length(ratios)
  # Corresponding vector containg letters
  ratio_letters <- letters[1:number_of_rartios]

  # Obtained strength class
  strength_class <- strenght_intervals[findInterval(fR1k, strenght_intervals)]

  # Calculated ratio
  ratio <- fR3k / fR1k
  # Obtained ratio interval letter
  ratio_interval <- ratio_letters[findInterval(ratio, ratios)]
  # Return the FRC class
  frc_class <- paste0(strength_class,ratio_interval)
  return(frc_class)

}