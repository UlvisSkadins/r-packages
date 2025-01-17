#' Calculate concrete properties
#'
#' This function returns dataframe of concrete properties calculated according to EN1992-1-1 (2023).
#'
#'
#' @param x a vector containing numerical values of concrete strength
#' @param conv_factor a transition factor depending on shape and size of the specimens
#' @export
calc.concr.prop.EC2 <- function (x, conv_factor = 1){
  # Vidējā stiprība spiedē
  # Ja paraugs ir standarta cilindrs, tad conv_factor = 1,
  # Ja kubs 150, tad conv_factor = 0.8
  fcm <- conv_factor * summary(x)[["Mean"]]
  fck <- fcm - 8
  # Elastības modulis
  Ecm <- calc.Ecm(fck)
  # Stiepes stiprība
  if (fck <= 50) {
    fctm <- 0.3 * fck^(2/3)
  } else {
    fctm <- 1.1 * fck^(1/3)
  }
  # Model Code 2010 for fck > 50 MPa:
  # fctm <- 2.12 * log(1 + 0.1 * fcm)

  # raksturīgā vērtība
  fctk <- 0.7 * fctm

  df <- data.frame(fcm = round(fcm, 2),
                   fck = round(fck, 2),
                   Ecm = round(Ecm, 0),
                   fctm = round(fctm, 2),
                   fctk = round(fctk, 2)
  )

  return (df)

}



#' Calculate characteristic value
#'
#' This function returns characteristic value calculated according to EN1990, Annex D (D7).
#'
#'
#' @param x a vector containing numerical values of material properties
#' @param Vknown a boolean value T or F whether V is known (T) or not (F)
#' @param Vx known coefficient of variation
#' @export
calc.charact.value <- function (x, Vknown = F, Vx = NA){
  # Data from EN1990, Table D.1
  n <- c(1, 2, 3, 4, 5, 6, 8, 10, 20, 30, Inf)
  kn_known <- c(2.31, 2.01, 1.89, 1.83, 1.80, 1.77, 1.74, 1.72, 1.68, 1.67, 1.64)
  kn_unknown <- c(NA, NA, 3.37, 2.63, 2.33, 2.18, 2.0, 1.92, 1.76, 1.73, 1.64)

  # Sample size - number of specimens
  samp_size <- length(x)

  # selects which row of the table D.1 should be taken
  if  (Vknown) {
    kn_all <- kn_known
  } else {
    kn_all <- kn_unknown
  }

  # Interpolate kn value
  kn <- approx(n, kn_all, samp_size)$y
  # Standard deviation
  sd <- sd(x, na.rm = T)
  # Mean value
  mean <- mean(x, na.rm = T)
  # Coef. of variation
  if  (Vknown) {
    if (is.na(Vx)) {
      Vx <- sd / mean
    } else {
      Vx <- Vx
    }
  } else {
    Vx <- sd / mean
  }


  # Characteristic value (5% fractile)
  Xk <- mean * (1 - kn * Vx)

  return (Xk)

}







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
#' @return A data frame of calculated values: w1 (mm), W1 (kNm), w2 (mm), W2 (kNm), fct (MPa), Gf (kN/m)
#' @export
calc.SIA162 <- function(F_vect, w_vect, h0, lf, n) {

  df <- data.frame(F = F_vect, w = w_vect / 1000)

  # w2_lim <- df[nrow(df),]$w
  # w1_lim <- w2_lim / 4

  lf <- lf / 1000

  # w1 <- min(c((0.07 * n - 0.10) * lf, w1_lim), na.rm = T)
  w1 <- (0.07 * n - 0.10) * lf

  df$diff_w <- c(0, diff(df$w))
  df$Wi <- df$F * df$diff_w

  W1 <- max(cumsum(df[1:which.min(abs(df$w - w1)),]$Wi))


  fct <- 3 * W1 / (n * h0^2 * lf)

  w2 <- 4 * w1

  W2 <- max(cumsum(df[1:which.min(abs(df$w - w2)),]$Wi))


  Gf <- W2 / (3 * n * h0^2)

  results <- data.frame(w1 = w1*1000, W1 = W1, w2 = w2*1000, W2 = W2, fct = fct/1000, Gf = Gf)
  # results <- list(w1, W1, w2, W2, fct, Gf)

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


#' Modulus of elasticity of concrete according to Model Code 2020
#'
#' This function calculates modulus of elasticity of concrete according to Model Code 2020 (14.7-1).
#'
#'
#' @param fcm concrete compressive strength (mean value).
#' @param alE coefficient considering type of aggregates. Quartzite aggregates = 1.0, other 0.7...1.2
#' @return modulus of elasticity Ecm
#' @export
calc.Ecm.MC2020 <- function (fcm, alE = 1) {
  # This formula is according ot EC2 2023
  return(21500 * alE * (fcm/10)^(1/3))
}



#' FRC post-cracking strength class
#'
#' Returns FRC post-cracking strength class according to Model Code 2020.
#'
#'
#' @param fR1k residual strengths significant for serviceability.
#' @param fR3k residual strengths significant for ultimate conditions.
#' @return A string representing FRC post-cracking strength class.
#' @export
calc.FRC.class <- function (fR1k, fR3k) {
  # Strength intervals
  # Model Code 2010:
  # strenght_intervals <- c(seq(1,3,0.5), seq(4,20,1))
  # Model Code 2020:
  strenght_intervals <- c(seq(1, 5, 0.5), seq(6, 8, 1), seq(10, 14, 2))

  # Strength ratios
  ratios <- c(seq(0.5, 1.3, 0.2), Inf)
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



#' FRC material model in tension
#'
#' Returns FRC post-cracking strength class according to Model Code 2020.
#'
#'
#' @param fR1k residual strengths significant for serviceability.
#' @param fR3k residual strengths significant for ultimate conditions.
#' @return ...
#' @export
calc.FRC.mat.model.LoAIII.1 <- function (fcm, fct=NA, fR1k, fR3k, wu=2.5, al0=0.58/2, Act=1, lcs=125) {

  Dfc <- 8

  fck <- fcm - Dfc

  # Tensile strength
  fctm <- 1.8 * log(fck) - 3.1
  # fctk.min <- 0.7 * fctm
  # fctk.max <- 1.3 * fctm

  # Tensile strength
  if (is.na(fct)) {
    fct <- fctm
  }

  # Modulus of elasticity
  Ecm <- calc.Ecm.MC2020(fcm)

  # Fracture energy [N/mm]
  GF <- 85 * fck^0.15 / 1000




  CMOD1 <- 0.5
  CMOD3 <- 2.5

  fFts <- 0.37 * fR1k

  fFtu <- fFts - (wu - CMOD1) / (CMOD3 - CMOD1) * (fFts - 0.57*fR3k + 0.26*fR1k)
  fFtu <- max(fFtu, 0)


  # Orientation factor k0
  k0 <- al0 / 0.58


  fFts <- k0 * fFts
  fFtu <- k0 * fFtu


  # Coeficient kG
  # kG <- 1 + 0.5 * Act


  Ec <- Ecm

  eps_P <- fctm / Ecm
  eps_Q <- GF / (fctm * lcs) + (eps_P - 0.8 * fctm / Ec)
  eps_SLS <- CMOD1 / lcs
  eps_ULS <- min(wu / lcs, 0.02, 2.5 / lcs)


  # Coeffients of the bi-linear material model
  # slope of the first and socond branches
  b1 <- -0.8 * fct / (eps_Q - eps_P)
  b2 <- (fFtu - fFts) / (eps_ULS - eps_SLS)
  # intersept of the first and second branches
  a1 <- fct + b1 * eps_P
  a2 <- fFts + b2 * eps_SLS

  # Strain eps_C
  eps_C <- (a2 - a1)/(b1 - b2)

  sig_C <- a1 + b1 * eps_C
  sig_C2 <- a2 + b2 * eps_C




  if (fFts <= 0.8 * fctm) {

    eps <- c(0, eps_P, eps_C, eps_SLS, eps_ULS, eps_ULS)
    sig <- c(0, fct, sig_C, fFts, fFtu, 0)

  } else {
    print("funkcija jāpapildina gadījumiem, kad fFts > 0.8fct")
  }

  df <- data.frame(Point = c("O", LETTERS[2:6]),
                   sig = sig,
                   eps = eps
  )

  return (df)

}





#' Time dependent factor beta_cc
#'
#' This function returns time dependent factor beta_cc according to EC2 (B.2).
#'
#'
#' @param t time (days)
#' @param tref reference time (days)
#' @param sC factor according to table B.2 (EC2)
#' @return residual strength f_Ri
#' @export
calc.beta_cc <- function (t, tref, sC) {
  return(exp(sC * (1 - sqrt(tref/t)) * sqrt(28/tref)))
}