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
    df_i$Kopa <- gsub('\\..*', '', as.character(kopu_saraksts$faila_nos[fails]))

    # Pievienošana kopējai tabulai (data frame)
    df <- rbind(df, df_i)

  }

  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("\\.", "", colnames(df))
  return(df)
}