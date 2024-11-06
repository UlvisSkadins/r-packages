#' Shows samples in a folder
#'
#' This function returns a data frame showing samples in the specified directory.
#' The sample name is the corresponding file name.
#'
#'
#' @param dir_path A path to the folder where the raw data are stored
#' @return A data frame containing list of samples
#' @export
paraugu.kopas <- function(dir_path) {
  faila_nos <- list.files(dir_path)
  n_files <- length(faila_nos)
  nr_pk <- (1 : n_files)
  par_kopas <- as.data.frame(cbind(nr_pk, faila_nos))

  return(par_kopas)

}



#' Converts data frame from wide to long
#'
#' This function converts a data frame with multiple columns to long data frame,
#' where there are only three columns: x value, y value and label. It is done for
#' plotting purposes.
#'
#'
#' @param df A data frame to be converted
#' @param x_value A column name that contains values on x axis
#' @param y_value A vector of column names that contains values on y axis
#' @param repeated_columns A vector of column names that will be preserved and repeated for every label
#' @return A converted data frame with tree columns
#' @export
dataframe.wide.to.long <- function(df, x_value, y_value, repeated_columns = NULL) {

  df_long <- data.frame()

  number_of_rows <- nrow(df)

  for (y in y_value) {
    df_long_i <- data.frame(x = df[, x_value],
                            y = df[, y],
                            label = rep(y, number_of_rows))

    df_long_i <- cbind(df_long_i, df[, repeated_columns])

    df_long <- rbind(df_long, df_long_i)

  }

  # Reneming columns
  colnames(df_long) <- c(x_value, "values", "label", repeated_columns)

  return(df_long)
}


##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange.vars <- function(data, vars){
    ##stop if not a data.frame (but should work for matrices as well)
    stopifnot(is.data.frame(data))

    ##sort out inputs
    data.nms <- names(data)
    var.nr <- length(data.nms)
    var.nms <- names(vars)
    var.pos <- vars
    ##sanity checks
    stopifnot( !any(duplicated(var.nms)),
               !any(duplicated(var.pos)) )
    stopifnot( is.character(var.nms),
               is.numeric(var.pos) )
    stopifnot( all(var.nms %in% data.nms) )
    stopifnot( all(var.pos > 0),
               all(var.pos <= var.nr) )

    ##prepare output
    out.vec <- character(var.nr)
    out.vec[var.pos] <- var.nms
    out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
    stopifnot( length(out.vec)==var.nr )

    ##re-arrange vars by position
    data <- data[ , out.vec]
    return(data)
}


#' Kombinē visus datus vienā tabulā
#'
#' Šī funkcija ļauj salikt vinā tabulā visus vienā mapē esošos failus atbilstoši
#' norādītajai funkcijai.
#'
#'
#' @param dir_path Folderis (mape), kurā atrodas viena veida datu faili
#' @param funkcija Funkcija, kas jāizmanto katra faila pārveidošanai par datu masīvu.
#' @return Tiek izveidots datu masīvs (data.frame) ar visiem paraugiem un kopām.
#' @export
dataAll <- function (dir_path, funkcija) {
  # Izveido failu sarakstu
  kopu_saraksts <- paraugu.kopas(dir_path)
  # Failu skaits
  kopu_skaits <- nrow(kopu_saraksts)

  # Kopējās datubāzes (data frame) izveidošana
  # sākumā pamatojoties uz 1. kopu jeb failu
  # failu nosaukumu saraksts
  faila_nos <- list.files(dir_path)
  # pirmā faila nosaukums:
  file_1_name <- paste0(dir_path, "/", faila_nos[1], collapse = NULL)

  # datubāzes izveide
  df <- funkcija(file_1_name)
  # kolonnu nosaukumi failā, kas nepieciešami, lai paziņotu kļūdu, ja ir faili ar atšķirīgiem
  # kolonnu nosaukumiem vai skaitu
  kolonnu_nosaukumi_faila_1 <- colnames(df)
  # print(kolonnu_nosaukumi_faila_1)
  kolonnu_skaits_faila_1 <- length(colnames(df))
  # pievieno jaunu kolonnu, kurā vēlāk saglabās kopas nosaukumu
  df$Kopa <- ''
  # atstāj tikai tabulas galvu
  df <- df[0,]

  # vārdu vectors ar attiecīgajiem kolonnu numuriem
  cols.names <- setNames(1:kolonnu_skaits_faila_1,kolonnu_nosaukumi_faila_1)

  # Visas mapes failu apkopošana
  for (fails in 1:kopu_skaits) {
    # Kārtējā faila pārveidošana par data frame ar skaitliskām vērtībām
    df_i <- funkcija(paste0(dir_path, "/", faila_nos[fails], collapse = NULL))
    # kolonnu nosaukumi un skaits failā konkrētajam failam
    kolonnu_nosaukumi_faila_i<- colnames(df_i)
    # print(kolonnu_nosaukumi_faila_i)
    kolonnu_skaits_faila_i<- length(colnames(df_i))


    #   Pārbauda vai kolonnu skaits failos sakrīt
    if (kolonnu_skaits_faila_1 != kolonnu_skaits_faila_i){

      if(kolonnu_skaits_faila_1 < kolonnu_skaits_faila_i) {

        df_i <- df_i[, kolonnu_nosaukumi_faila_1]

      } else {

        df_NA <- data.frame(matrix(NA, nrow = nrow(df_i), ncol = (kolonnu_skaits_faila_1 - kolonnu_skaits_faila_i)))
        col_missing <- setdiff(kolonnu_nosaukumi_faila_1, kolonnu_nosaukumi_faila_i)
        colnames(df_NA) <- col_missing

        df_i <- cbind(df_i, df_NA)

        df_i <- arrange.vars(df_i, cols.names)

        warning(paste("Kolonnu skaits failaa",
                   as.character(kopu_saraksts$faila_nos[fails]),
                   "ir mazāks par pirmaa faila",
                   as.character(kopu_saraksts$faila_nos[1]),
                   " kolonnu skaitu! Trūkstošās kollonas aizpildītas ar NA."))
      }

    }

    # kolonnas pievienošana ar kopas nosaukumu
    extension_text <- paste0(".",
                             sub('.*\\.', '', as.character(kopu_saraksts$faila_nos[fails])))
    df_i$Kopa <- sub(extension_text, '', as.character(kopu_saraksts$faila_nos[fails]))

    # Pievienošana kopējai tabulai (data frame)
    df <- rbind(df, df_i)

  }

  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("\\.", "", colnames(df))
  return(df)
}



#' Combines load with other measurements based on time
#'
#' This function combines two data frames - one containing time and load,
#' the other - time and another measurement (e.g. deflection, crack width).
#' It is used to make diagrams like Load vs deflection or Load vs crack width.
#'
#'
#' @param dfLoad a dataframe having two columns c("Time", "Load") in this order
#' @param dfValue a dataframe having two columns c("Time", "Value") in this order
#' @param time_diff time difference: dfValue$Time - dfLoad$Time
#' @export
combine.values.with.load <- function (dfLoad, dfValue, time_diff = 0) {
  # gets the name of the value
  value_name <- colnames(dfValue)[2]
  # Make sure, the time format is correct
  if(!inherits(dfLoad[,1], "POSIXct")){
    stop("The first column in dfLoad must be of class POSIXct!")
  }
  if(!inherits(dfValue[,1], "POSIXct")){
    stop("The first column in dfValue must be of class POSIXct!")
  }

  # Make sure, the column names are correct
  colnames(dfLoad) <- c("Time", "Load")
  colnames(dfValue) <- c("Time", "Value")
  # adjust the time
  dfValue$Time <- dfValue$Time - time_diff

  # number of rows in dfLoad
  nl <- nrow(dfLoad)
  # new data frame for load values with time step 1 sec and the missing load values
  # calulated by linear interpolation
  load_approx <- with(dfLoad, data.frame(approx(Time, Load, xout = seq(Time[1], Time[nl], "sec"))))
  # replace "x" and "y" with "Time" and "Load" as the column names
  colnames(load_approx) <- c("Time", "Load")

  # Find the rows that intersect between dfValue and load_approx
  rows_intersect <- which(load_approx$Time %in% dfValue$Time)

  # Create new data frame, where the intersect values of both dataframes are stored
  df <- load_approx[rows_intersect,]
  # reset row names
  rownames(df_comb) <- NULL
  # Adjust the start row - in both data frames it should start on the same time value
  if (dfValue$Time[1] < df$Time[1]) {
    n_start <- which(dfValue$Time == df$Time[1])
  } else {
    n_start <- 1
  }
  print(n_start)
  # The end row should bet the last row of dfValue or
  # the last row of df if there are less readings than in dfValue
  n_end <- min(nrow(dfValue), nrow(df) + (n_start-1))
  print(n_end)
  print(nrow(dfValue))
  print(nrow(df))
  # Subset the dfValue data frame
  dfValue <- dfValue[n_start:n_end,]
  # reset row names
  rownames(dfValue) <- NULL


  print(nrow(dfValue))
  print(nrow(df))

  if (nrow(dfValue) == nrow(df)){
    df$value <- dfValue[,2]
    df$label <- value_name
  } else {
    stop("Bug in the function: number of rows do not match!")
  }

  return (df)


}


#' Finds peak values in measured data
#'
#' This function finds peak values in measured data, wich consists of time and value.
#' This function is used in function find.and.plot.peaks - to help to find time difference
#' caused by the time settings in different devices
#'
#'
#' @param x a dataframe conteining atleast two columns (time and value)
#' @param colTime number or name of the column containing time
#' @param colValue number or name of the column containing measured values
#' @param prob probability used as treshold to select the peak values
#' @export
find.peak <- function (x, colTime, colValue, prob = 0.05){
  df <- x[, c(colTime, colValue)]
  # Rename columns
  colnames(df) <- c("Time", "Value")
  # Calculate difference of values between rows
  df$diff <- c(0, diff(df$Value))
  # Calculate time difference and convert to numeric
  df$diffT <- c(0, diff(as.numeric(df$Time)))
  # Calculate gradient
  df$grad <- df$diff / df$diffT
  # Calculate difference between gradients
  df$diffgrad <- c(0, diff(df$grad))

  df <- df[-(which.min(df$diffgrad) & which.max(df$diffgrad)), ]

  # Treshold of the difference of gradients used to fillter
  diffgrad_tresh <- quantile(df$diffgrad, probs = prob, na.rm = T)

  # New data frame to store peak values
  df_peak <- subset(df, diffgrad < diffgrad_tresh)
  df_peak <- subset(df_peak, diff != 0)
  rownames(df_peak) <- NULL

  return (df_peak)

}


#' Plots diagram with peak values
#'
#' This function plots a diagram consisting of two curves and loacal peak values
#' to be compared.
#'
#'
#' @param x1,x2 data frames containing measured data that needs to be compared
#' @param peaks1,peaks2 data frames containing peak values of x1 and x2
#' @param cols1,cols2 column names of the data frames - c("Time", "Value") excpected
#' should be the same in x1 and peaks1 and in x2 and peaks2
#' @export
plot.diagram.peaks <- function (x1, x2, peaks1, peaks2, cols1 = c(1, 2), cols2 = c(1, 2)){
  df1 <- x1[, cols1]
  df2 <- x2[, cols2]
  dfp1 <- peaks1[, cols1]
  dfp2 <- peaks2[, cols2]
  colnames(df1) <- c("Time", "Value")
  colnames(df2) <- colnames(df1)
  colnames(dfp1) <- colnames(df1)
  colnames(dfp2) <- colnames(df1)
  max1 <- max(df1$Value, na.rm = T)
  max2 <- max(df2$Value, na.rm = T)

  koef <- max1 / max2

  p <- ggplot2::ggplot(df1)+
    ggplot2::geom_path(ggplot2::aes(Time, Value/koef))+
    ggplot2::geom_path(data = df2, ggplot2::aes(Time, Value))+
    ggplot2::geom_point(data = dfp1, ggplot2::aes(Time, Value/koef), size = 3, colour = "red")+
    ggplot2::geom_point(data = dfp2, ggplot2::aes(Time, Value), size = 3, colour = "blue")+
    ggplot2::annotate("text", x = dfp1$Time, y = dfp1$Value*1.05/koef, colour = "red",
             label = paste("(", rownames(dfp1), ")", strftime(dfp1$Time, format="%H:%M:%S")))+
    ggplot2::annotate("text", x = dfp2$Time, y = dfp2$Value*1.05, colour = "blue",
             label = paste("(", rownames(dfp2), ")", strftime(dfp2$Time, format="%H:%M:%S")))

  return (p)
}

#' Finds peaks and plots diagram with the peak values
#'
#' This function finds peak values in measured data (two data frames), wich consists of time and value.
#' Then it plots the data in a diagram, pointing out the time at peak values.
#'
#'
#' @param x1,x2 data frames containing measured data that needs to be compared
#' @param cols1,cols2 column names of the data frames - c("Time", "Value") excpected
#' should be the same in x1 and peaks1 and in x2 and peaks2
#' @param prob1,prob2 probability used as treshold to select the peak values
#' @param peak1,peak2 numbers of peaks that represent the same time in the test (measuring process)
#' @export
find.and.plot.peaks <- function (x1, x2, cols1, cols2, prob1=0.05, prob2=0.05, peak1=1, peak2=1){
  df1 <- x1[, cols1]
  df2 <- x2[, cols2]

  # Rename columns
  colnames(df1) <- c("Time", "Value")
  colnames(df2) <- c("Time", "Value")
  cols1 <- colnames(df1)
  cols2 <- colnames(df2)

  dfp1 <- find.peak(df1, cols1[1], cols1[2], prob1)
  dfp2 <- find.peak(df2, cols2[1], cols2[2], prob2)

  p <- plot.diagram.peaks(df1, df2, dfp1, dfp2, cols1, cols2)

  time1 <- dfp1$Time[peak1]
  time2 <- dfp2$Time[peak2]

  time_diff <- as.numeric(time1 - time2,units="secs")

  print(paste("Time 1 is:", time1, "; time 2 is:", time2, "; time difference is:", time_diff, "sec"))

  return (p)

}