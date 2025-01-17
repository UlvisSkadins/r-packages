#' Form+Test test results
#'
#' This function returns a data frame from a text file exported from Proteus software.
#' In the file usualy results from one or more test specimens are stored.
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing test results - raw data.
#' @export
formTestDataFrame <- function (file) {
  # Read the file
  data <- read.table(file, sep=';', stringsAsFactors=FALSE, fill = TRUE)
#   converts to data frame
  data <- as.data.frame(data)

  ## Atlasa rindas, kurās sākās jauni paraugi (tabulas):
  # 1) tabulu nosaukumu rindas
  tabulu_virsraksti <- data[data[, 1]=='Index',]
  tab_virsr_indeksi <- as.numeric(rownames(tabulu_virsraksti))
  # 2) indeksi pirmajai skaitļu rindai
  tab_sak_indeksi <- tab_virsr_indeksi + 2
  # 3) indeksi tabulu beigu rindām
  tab_beig_indeksi <- tab_virsr_indeksi[-1] - 2
  tab_beig_indeksi <- append(tab_beig_indeksi, nrow(data))
  # 4) indeksi paraugu nosaukumiem
  par_nos_indeksi <- tab_virsr_indeksi - 1

  # Parugu nosaukumi
  par_nosaukumi <- data[par_nos_indeksi,1]


  # Paraugu skaits
  n <- length(par_nosaukumi)


  # Noņem liekās kolonnas
  # kolonnu nosaukumu rinda
  kolonnu_nosaukumu_rinda <- tab_virsr_indeksi[1]
  # print("Kolonnu nosaukumi:")
  # print(data[kolonnu_nosaukumu_rinda, ])


  # kolonnu filtrēšana
  if(any(is.na(data[kolonnu_nosaukumu_rinda, ])|
           as.character(data[kolonnu_nosaukumu_rinda, ]) == "")){
    data <- data[, -c(which(is.na(data[kolonnu_nosaukumu_rinda, ])),
                     which(as.character(data[kolonnu_nosaukumu_rinda, ]) == ""))]
  }


  # - kolonnu nosaukumi
  kol_nosaukumi <- data[kolonnu_nosaukumu_rinda, ]
  colnames(data) <- kol_nosaukumi

  # Paraugu nosaukumi
  data$Paraugi <- 0
  # pievieno nosaukumus
  for (i in 1:n){
    data[tab_sak_indeksi[i]:tab_beig_indeksi[i],"Paraugi"] <- par_nosaukumi[i]
  }


  # atstāj tikai skaitliskās vērtības, noņem virsrakstu līnijias
  data <- data[data["Paraugi"] != 0,]

  # Konvertē tekstu uz skaitļiem
  # Izņemot laika kolonnu
  cols <- which(kol_nosaukumi != "Time")
  data[, cols] <- apply(data[, cols], 2, function (x) as.numeric(as.character(x)))

  # Konvertē laika kolonnu uz laiku
  # konvertāciju veic, ja ir reģistrēts laiks formātā H:M:S. Citos gadījumos,
  # kad ir tikai sekundes, pārtaisa uz numeric
  if(grepl(":", data$Time[1])){

    if(nchar(data$Time[1]) > 12) {
      data[, "Time"] <- as.POSIXct(data$Time, format="%d.%m.%Y %H:%M:%OS", tz = Sys.timezone())
    } else {
      data[, "Time"] <- as.POSIXct(data$Time, format="%H:%M:%OS", tz = Sys.timezone())
    }


  } else {
    data[, "Time"] <- as.numeric(as.character(data$Time))
  }

  # Nomet tās rindas, kurās parādās 'NA'
  # data <- na.omit(data)


  return (data)


}










#' Form+Test test results
#'
#' This function returns a data frame containing all the data obtained from files in a folder.
#' All the files must be of the same type, containing results of the same type of test procedure.
#'
#'
#' @param dir_path A path to the folder where the files are stored.
#' @return A data frame containing test results - raw data.
#' @export
# Funkcija, kas izveido kopēju datubāzi (data frame) visām datu kopām
formTestAll <- function (dir_path){
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
  df <- formTestDataFrame(file_1_name)
  # kolonnu nosaukumi failā, kas nepieciešami, lai paziņotu kļūdu, ja ir faili ar atšķirīgiem
  # kolonnu nosaukumiem vai skaitu
  kolonnu_nosaukumi_faila_1 <- colnames(df)
  # print(kolonnu_nosaukumi_faila_1)
  kolonnu_skaits_faila_1 <- length(colnames(df))
  # pievieno jaunu kolonnu, kurā vēlāk saglabās kopas nosaukumu
  df$Kopa <- ''
  # atstāj tikai tabulas galvu
  df <- df[0,]

  # Visas mapes failu apkopošana
  for (fails in 1:kopu_skaits) {
    # Kārtējā faila pārveidošana par data frame ar skaitliskām vērtībām
    df_i <- formTestDataFrame(paste0(dir_path, "/", faila_nos[fails], collapse = NULL))
    # kolonnu nosaukumi un skaits failā konkrētajam failam
    kolonnu_nosaukumi_faila_i<- colnames(df_i)
    # print(kolonnu_nosaukumi_faila_i)
    kolonnu_skaits_faila_i<- length(colnames(df_i))


    #   Pārbauda vai kolonnu skaits failos sakrīt
    if (kolonnu_skaits_faila_1 != kolonnu_skaits_faila_i){

      if(kolonnu_skaits_faila_1 < kolonnu_skaits_faila_i) {

        df_i <- df_i[, kolonnu_nosaukumi_faila_1]

      } else {

        stop(paste("Kolonnu skaits failaa",
                   as.character(kopu_saraksts$faila_nos[fails]),
                   "ir mazāks par pirmaa faila",
                   as.character(kopu_saraksts$faila_nos[1]),
                   " kolonnu skaitu!"))
      }



    } else {
      #   Pārbauda vai kolonnu nosaukumi failos sakrīt
      if (all(kolonnu_nosaukumi_faila_1 == kolonnu_nosaukumi_faila_i)){


      } else {

        stop(paste("Kolonnu(as)",
                     which(kolonnu_nosaukumi_faila_1 != kolonnu_nosaukumi_faila_i),
                     "nosaukumi(s) failaa",
                     as.character(kopu_saraksts$faila_nos[fails]),
                     "nesakriit ar pirmaa faila",
                     as.character(kopu_saraksts$faila_nos[1]),
                     "kolonnu nosaukumiem(u)!"))

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




#' Form+Test CMOD test results
#'
#' This function returns a data frame from a text file exported from Proteus software.
#' This function is similar to formTestDataFrame, but takes only the summary data,
#' which are in the same line with specimen number
#'
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing summary of the results of CMOD tests.
#' @export
formTestCMODSumarry <- function (file) {
  # Read the file
  data <- read.table(file, sep=';', stringsAsFactors=FALSE, fill = TRUE)
#   converts to data frame
  data <- as.data.frame(data)

  # Kopējie dati
  # rinda, kurā ir kopas nosaukums un datumi
  row_1 <- 2
  # lielumi
  kopa <- as.character(data[row_1, 1])
  izgat_datums <- as.POSIXct(paste(as.character(data[row_1, 2]), as.character(data[row_1, 3])),
                             format = "%d.%m.%Y %H:%M")
  test_datums <- as.POSIXct(paste(as.character(data[row_1, 4]), as.character(data[row_1, 5])),
                             format = "%d.%m.%Y %H:%M")



  ## Atlasa rindas, kurās sākās jauni paraugi (tabulas):
  par_nos_indeksi <- which(data[, 1]=='Index')-1

  # Izveido datubāzi, kurā ir tikai kopsavilkuma rindas
  res_data <- data[par_nos_indeksi,]

  # Piešķir datubāzei kolonnu nosaukumus
  col_nos <- c("Paraugs", "h", "b", "L", "V5", "V6", "hsp", "x", "y", "L0", "velocity",
               "fRLOP", "FR1", "FR2", "FR3", "FR4")
  colnames(res_data) <- col_nos

  # Pievieno papildus kolonnas
  res_data$Kopa <- kopa
  res_data$Time_prod <- izgat_datums
  res_data$Time_test <- test_datums
  res_data$Age <- round(test_datums - izgat_datums, 1)

  # Atlasa tikai tās kolonnas, kuras tiks izmantotas galējā datubāzē
  selected_columns <- c("Kopa", "Time_prod", "Time_test", "Age", "Paraugs", "b", "hsp", "L0",
                        "velocity", "fRLOP", "FR1", "FR2", "FR3", "FR4")

  res_data <- res_data[, selected_columns]


  return (res_data)


}



#' Form+Test CMOD test results
#'
#' This function returns a data frame containing all the summary data of CMOS tests
#' obtained from files in a folder.
#' It is valid only with the files from CMOD standard test.
#'
#'
#' @param dir_path A path to the folder where the files are stored.
#' @return A data frame containing test results - summary of CMOD tests.
#' @export
# Funkcija, kas izveido kopēju datubāzi (data frame) visām datu kopām
formTestCMODSumAll <- function (dir_path){
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
  df <- formTestCMODSumarry(file_1_name)
  # kolonnu nosaukumi failā, kas nepieciešami, lai paziņotu kļūdu, ja ir faili ar atšķirīgiem
  # kolonnu nosaukumiem vai skaitu
  kolonnu_nosaukumi_faila_1 <- colnames(df)
  # print(kolonnu_nosaukumi_faila_1)
  kolonnu_skaits_faila_1 <- length(colnames(df))
  # atstāj tikai tabulas galvu
  df <- df[0,]

  # Visas mapes failu apkopošana
  for (fails in 1:kopu_skaits) {
    # Kārtējā faila pārveidošana par data frame ar skaitliskām vērtībām
    df_i <- formTestCMODSumarry(paste0(dir_path, "/", faila_nos[fails], collapse = NULL))
    # kolonnu nosaukumi un skaits failā konkrētajam failam
    kolonnu_nosaukumi_faila_i<- colnames(df_i)
    # print(kolonnu_nosaukumi_faila_i)
    kolonnu_skaits_faila_i<- length(colnames(df_i))


    #   Pārbauda vai kolonnu skaits failos sakrīt
    if (kolonnu_skaits_faila_1 != kolonnu_skaits_faila_i){

          stop(paste("Kolonnu skaits failaa",
               as.character(kopu_saraksts$faila_nos[fails]),
               "nesakrīt ar pirmaa faila",
               as.character(kopu_saraksts$faila_nos[1]),
               " kolonnu skaitu!"))




    } else {
      #   Pārbauda vai kolonnu nosaukumi failos sakrīt
      if (all(kolonnu_nosaukumi_faila_1 == kolonnu_nosaukumi_faila_i)){


      } else {

        stop(paste("Kolonnu(as)",
                     which(kolonnu_nosaukumi_faila_1 != kolonnu_nosaukumi_faila_i),
                     "nosaukumi(s) failaa",
                     as.character(kopu_saraksts$faila_nos[fails]),
                     "nesakriit ar pirmaa faila",
                     as.character(kopu_saraksts$faila_nos[1]),
                     "kolonnu nosaukumiem(u)!"))

      }
    }


    # Pievienošana kopējai tabulai (data frame)
    df <- rbind(df, df_i)

  }

  # nodzēš mantotos rindu nosaukumus
  rownames(df) <- NULL

  return(df)
}



#' Form+Test prism bending test results
#'
#' This function returns a data frame from a text file exported from Proteus software.
#' This function is similar to formTestCMODSumAll, but is used in plain concrete
#' flexural tensile tests according to EN 12390-5:2009
#'
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing summary of the results of flexural tensile tests.
#' @export
formTestFlexSumarry <- function (file) {
  # Read the file
  data <- read.table(file, sep=';', stringsAsFactors=FALSE, fill = TRUE)
#   converts to data frame
  data <- as.data.frame(data)

  # Kopējie dati
  # rinda, kurā ir kopas nosaukums un datumi
  row_1 <- 2
  # lielumi
  kopa <- as.character(data[row_1, 1])
  izgat_datums <- as.POSIXct(paste(as.character(data[row_1, 2]), as.character(data[row_1, 3])),
                             format = "%d.%m.%Y %H:%M")
  test_datums <- as.POSIXct(paste(as.character(data[row_1, 4]), as.character(data[row_1, 5])),
                             format = "%d.%m.%Y %H:%M")



  ## Atlasa rindas, kurās sākās jauni paraugi (tabulas):
  par_nos_indeksi <- which(data[, 1]=='Index')-1

  # Izveido datubāzi, kurā ir tikai kopsavilkuma rindas
  res_data <- data[par_nos_indeksi,]

  # Konvertē tekstu uz skaitļiem
  # Izņemot parauga nosaukuma kolonnu
  cols <- 2:length(colnames(res_data))
  res_data[, cols] <- apply(res_data[, cols], 2, function (x) as.numeric(as.character(x)))


  # Piešķir datubāzei kolonnu nosaukumus
  col_nos <- c("Specimen", "h", "b", "L", "Mass", "Density", "Fmax", "Strength")
  colnames(res_data) <- col_nos

  # Pievieno papildus kolonnas
  res_data$Kopa <- kopa
  res_data$Time_prod <- izgat_datums
  res_data$Time_test <- test_datums
  res_data$Age <- round(test_datums - izgat_datums, 1)

  # Atlasa tikai tās kolonnas, kuras tiks izmantotas galējā datubāzē
  selected_columns <- c("Kopa", "Time_prod", "Time_test", "Age", "Specimen",
                        "h", "b", "L", "Mass", "Density", "Fmax", "Strength")

  res_data <- res_data[, selected_columns]


  return (res_data)


}





#' CMOD dataframe from long to wide format
#'
#' This function converts long format data frame to wide format.
#' This is needed for notched prism bending test results to calculate min, max and mean values
#' for different CMOD values. This function also calculates the min, max and mean values.
#'
#'
#'
#' @param df A long format data frame for Load - CMOD test results.
#' @param id_kopa name of the column corresponding to the sample (sērija/kopa) in long table.
#' @param id_paraugi name of the column containgin specimen numbers in long table.
#' @param id_CMOD name of the column containing CMOD readings in long table.
#' @param id_Load name of the column containing Load or residual strength values in long table.
#' @param cmod_vals vector of predefined CMOD values. If the first value is set to 0.05,
#' the max Load value in this range is calculated.
#' @return A wide format data frame separating Load or residual strength values in separate columns for each specimen.
#' @export
cmod.long.to.wide <- function (df, id_kopa, id_paraugi, id_CMOD, id_Load, cmod_vals = seq(0, 4, 0.01)) {
  df_wide <- data.frame()

  number_cols <- length(unique(df[, id_paraugi]))

  df_0 <- data.frame(matrix(nrow = length(cmod_vals), ncol = number_cols))
  colnames(df_0) <- unique(df[, id_paraugi])


  for (k in unique(df[, id_kopa])) {
    df_k <- df[df[,id_kopa] == k, c(id_kopa, id_paraugi, id_CMOD, id_Load)]

    df_1 <- df_0

    for (p in unique(df_k[, id_paraugi])) {
      df_kp0 <- df_k[df_k[, id_paraugi] == p, ]
      df_kp <- df_kp0[sapply(cmod_vals, function (x){which.min(abs(df_kp0[, id_CMOD] - x))}),]

      df_1[, p] <- df_kp[, id_Load]

      if (cmod_vals[1] == 0.05) {
        df_LOP <- df_kp0[df_kp0[, id_CMOD] <= 0.05, ]
        fLOP <- max(df_LOP[, id_Load])
        df_1[1, p] <- fLOP
      }

    }

    df_2 <- cbind(df_kp[, c(id_kopa, id_CMOD)], df_1)
    # df_2 <- df_2[-grep("NA", rownames(df_2)),]

    df_wide <- rbind(df_wide, df_2)

  }

  col_of_last_specimen <- ncol(df_wide)

  colnames(df_wide)[3:col_of_last_specimen] <- paste0("p", colnames(df_wide)[3:col_of_last_specimen])

  df_wide$min <- apply(df_wide[, 3:col_of_last_specimen], 1, min, na.rm = T)
  df_wide$max <- apply(df_wide[, 3:col_of_last_specimen], 1, max, na.rm = T)
  df_wide$mean <- rowMeans(df_wide[, 3:col_of_last_specimen], na.rm = T)
  df_wide$char <- apply(df_wide[, 3:col_of_last_specimen], 1, calc.charact.value, Vknown = T, Vx = NA)


  return (df_wide)
}



#' Form+Test SIA162 test results
#'
#' This function returns a data frame from a text file exported from Proteus software.
#' This function is similar to formTestCMODSumarry, but takes the summary data in case of SIA162 tests.
#' The data are in the same line with specimen number.
#'
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing summary of the results of SIA 162/6 tests.
#' @export
formTestSIASumarry <- function (file) {
  # Read the file
  data <- read.table(file, sep=';', stringsAsFactors=FALSE, fill = TRUE)
#   converts to data frame
  data <- as.data.frame(data)

  # Kopējie dati
  # rinda, kurā ir kopas nosaukums un datumi
  row_1 <- 2
  # lielumi
  kopa <- as.character(data[row_1, 1])
  izgat_datums <- as.Date(as.POSIXct(paste(as.character(data[row_1, 2]), as.character(data[row_1, 3])),
                             format = "%d.%m.%Y %H:%M"))
  # test_datums <- as.Date(as.POSIXct(paste(as.character(data[row_1, 4]), as.character(data[row_1, 5])),
  #                            format = "%d.%m.%Y %H:%M"))



  ## Atlasa rindas, kurās sākās jauni paraugi:
  par_nos_indeksi <- which(data[, 1]=='Index')-1
  # Datumu indeksi
  df <- formTestDataFrame(file)
  paraugi <- unique(df$Paraugi)
  test_datumi <- df[match(paraugi, df$Paraugi), "Time"]



  res_data <- data[par_nos_indeksi,]



  # Konvertē tekstu uz skaitļiem
  # Izņemot parauga nosaukuma kolonnu
  cols <- 2:length(colnames(res_data))
  res_data[, cols] <- apply(res_data[, cols], 2, function (x) as.numeric(as.character(x)))

  col_nos <- c("Paraugs", "d", "t", "V4", "V5", "V6", "Pr", "P", "D1", "Dw",
               "Wr", "W", "RC")
  colnames(res_data) <- col_nos

  res_data$Kopa <- kopa
  res_data$Time_prod <- izgat_datums
  res_data$Time_test <- res_data$Time_prod

  i <- 1
  for (specimen in unique(res_data$Paraugs)){
    res_data[which(res_data$Paraugs == specimen), "Time_test"] <- test_datumi[as.numeric(i)]
    i <- i + 1
  }

  res_data$Age <- round(res_data$Time_test - res_data$Time_prod, 1)

  selected_columns <- c("Kopa", "Time_prod", "Time_test", "Age", "Paraugs", "d", "t",
                        "Pr", "P", "D1", "Dw", "Wr", "W", "RC")

  res_data <- res_data[, selected_columns]

  rownames(res_data) <- NULL


  return (res_data)


}


#' Form+Test ASTM test results
#'
#' This function returns a data frame from a text file exported from Proteus software.
#' This function is similar to formTestCMODSumarry, but takes the summary data in case of ASTM round panel tests.
#' The data are in the same line with specimen number.
#'
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing summary of the results of ASTM round panel tests.
#' @export
formTestASTMSumarry <- function (file) {
  # Read the file
  data <- read.table(file, sep=';', stringsAsFactors=FALSE, fill = TRUE)
#   converts to data frame
  data <- as.data.frame(data)

  # Kopējie dati
  # rinda, kurā ir kopas nosaukums un datumi
  row_1 <- 2
  # lielumi
  kopa <- as.character(data[row_1, 1])
  izgat_datums <- as.Date(as.POSIXct(paste(as.character(data[row_1, 2]), as.character(data[row_1, 3])),
                             format = "%d.%m.%Y %H:%M"))
  # test_datums <- as.Date(as.POSIXct(paste(as.character(data[row_1, 4]), as.character(data[row_1, 5])),
  #                            format = "%d.%m.%Y %H:%M"))



  ## Atlasa rindas, kurās sākās jauni paraugi:
  par_nos_indeksi <- which(data[, 1]=='Index')-1
  # Datumu indeksi
  df <- formTestDataFrame(file)
  paraugi <- unique(df$Paraugi)
  test_datumi <- df[match(paraugi, df$Paraugi), "Time"]



  res_data <- data[par_nos_indeksi,]



  # Konvertē tekstu uz skaitļiem
  # Izņemot parauga nosaukuma kolonnu
  cols <- 2:length(colnames(res_data))
  res_data[, cols] <- apply(res_data[, cols], 2, function (x) as.numeric(as.character(x)))

  col_nos <- c("Paraugs", "d", "t", "V4", "V5", "V6", "Pr", "P", "D1", "Dw",
               "Wr", "W", "RC")
  colnames(res_data) <- col_nos

  res_data$Kopa <- kopa
  res_data$Time_prod <- izgat_datums
  res_data$Time_test <- res_data$Time_prod

  i <- 1
  for (specimen in unique(res_data$Paraugs)){
    res_data[which(res_data$Paraugs == specimen), "Time_test"] <- test_datumi[as.numeric(i)]
    i <- i + 1
  }

  res_data$Age <- round(res_data$Time_test - res_data$Time_prod, 1)

  selected_columns <- c("Kopa", "Time_prod", "Time_test", "Age", "Paraugs", "d", "t",
                        "Pr", "P", "D1", "Dw", "Wr", "W", "RC")

  res_data <- res_data[, selected_columns]

  rownames(res_data) <- NULL


  return (res_data)


}


#' Form+Test SIA162 test results
#'
#' This function returns a data frame containing all the summary data of SIA162 tests
#' obtained from files in a folder.
#' It is valid only with the files from SIA162 standard test.
#'
#'
#' @param dir_path A path to the folder where the files are stored.
#' @return A data frame containing test results - summary of SIA162 tests.
#' @export
# Funkcija, kas izveido kopēju datubāzi (data frame) visām datu kopām
formTestSIASumAll <- function (dir_path){
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
  df <- formTestSIASumarry(file_1_name)
  # kolonnu nosaukumi failā, kas nepieciešami, lai paziņotu kļūdu, ja ir faili ar atšķirīgiem
  # kolonnu nosaukumiem vai skaitu
  kolonnu_nosaukumi_faila_1 <- colnames(df)
  # print(kolonnu_nosaukumi_faila_1)
  kolonnu_skaits_faila_1 <- length(colnames(df))
  # atstāj tikai tabulas galvu
  df <- df[0,]

  # Visas mapes failu apkopošana
  for (fails in 1:kopu_skaits) {
    # Kārtējā faila pārveidošana par data frame ar skaitliskām vērtībām
    df_i <- formTestSIASumarry(paste0(dir_path, "/", faila_nos[fails], collapse = NULL))
    # kolonnu nosaukumi un skaits failā konkrētajam failam
    kolonnu_nosaukumi_faila_i<- colnames(df_i)
    # print(kolonnu_nosaukumi_faila_i)
    kolonnu_skaits_faila_i<- length(colnames(df_i))


    #   Pārbauda vai kolonnu skaits failos sakrīt
    if (kolonnu_skaits_faila_1 != kolonnu_skaits_faila_i){

          stop(paste("Kolonnu skaits failaa",
               as.character(kopu_saraksts$faila_nos[fails]),
               "nesakrīt ar pirmaa faila",
               as.character(kopu_saraksts$faila_nos[1]),
               " kolonnu skaitu!"))




    } else {
      #   Pārbauda vai kolonnu nosaukumi failos sakrīt
      if (all(kolonnu_nosaukumi_faila_1 == kolonnu_nosaukumi_faila_i)){


      } else {

        stop(paste("Kolonnu(as)",
                     which(kolonnu_nosaukumi_faila_1 != kolonnu_nosaukumi_faila_i),
                     "nosaukumi(s) failaa",
                     as.character(kopu_saraksts$faila_nos[fails]),
                     "nesakriit ar pirmaa faila",
                     as.character(kopu_saraksts$faila_nos[1]),
                     "kolonnu nosaukumiem(u)!"))

      }
    }


    # Pievienošana kopējai tabulai (data frame)
    df <- rbind(df, df_i)

  }

  # nodzēš mantotos rindu nosaukumus
  rownames(df) <- NULL

  return(df)
}