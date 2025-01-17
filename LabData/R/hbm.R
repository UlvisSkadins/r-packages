#' HBM QuantumX data
#'
#' This function returns a data frame from a text file exported from Catman Easy software.
#' In the file usualy results from one test measured by QuantumX 440B/1615B are stored.
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing test results - raw data.
#' @export
HBMDataFrame <- function (file) {
  # Nolasa failu
  hbm <- read.table(file, sep = '\t',  row.names = NULL, stringsAsFactors = FALSE,
                     skipNul = TRUE, na.strings = '-1000000' #šis pārtaisa overflow data par NA
  )

  if (ncol(hbm) == 1) {
    hbm <- read.table(file, sep = '\t',
                      skip = 8,
                      row.names = NULL,
                      stringsAsFactors = FALSE,
                      skipNul = TRUE,
                      header = TRUE,
                      # fill = TRUE,
                      na.strings = '-1000000' #šis pārtaisa overflow data par NA
    )
  }

  # Noņem kolonnu ar dublējošo laiku (katrai iekārtai 440/1615 ir sava laika uzskaite, kas sakrīt)
  if (length(grep("Time", colnames(hbm))) == 2) {
    # Time 2 kolonnas nr.
    col_numb_time2 <- grep("Time", colnames(hbm))[2]
    # pēdējās kolonnas numurs
    col_numb_last <- length(colnames(hbm))
    # atjaunotā tabula ar izņemto kolonnu
    hbm <- hbm[, c(1:(col_numb_time2-1), (col_numb_time2+1):col_numb_last)]
  }



  # aizstāj liekās frāzes datu kolonnu nosaukumos
  col_names <- sub( 'MX1615B_2', 'MX16', names(hbm))
  col_names <- sub( 'MX440B', 'MX4', col_names)
  col_names <- sub( '..µm.m.', '', x = col_names, useBytes = TRUE)
  col_names <- sub( '\\.CH.*', '', col_names)
  col_names <- sub( '\\.', '', col_names)
  col_names[1] <- 'Time'


  # Testa laika sākums
  row_time <- grep("T0 =", hbm[,1])
  start_time <- sub("T0 =", "", hbm[row_time, 1])
  start_time <- as.POSIXlt(start_time, ,"%d.%m.%Y %H:%M:%OS")


  # Kanālu informācijas noņemšana
  # Skaitlisko datu sākuma rinda
  # row_data_1st <- which(hbm[,1] == 0)
  suppressWarnings(row_data_1st <- which(as.numeric(sub(",", ".",hbm[,1])) == 0))
  hbm <- hbm[row_data_1st:nrow(hbm),]

  # Nosaka faktisko kolonnu skaitu, pamatojoties uz kolonnu nosaukumu vektora garumu
  #   ņemot vērā to, ka liekā ir noņemta.
  col_num <- length(col_names)
  # print(col_names)
  # Atlasa tikai tās kolonnas, kurās ir dati - pēdējā kolonna pie ienešanas tiek izveidota un
  # aizpildīta ar NA, jo kolonnu nosaukumi pārbīdās pa vienu uz priekšu. Tātad, pēdējo ir jāatmet.
  hbm <- hbm[, 1:col_num] #gsub(',', '.', )
  # Pārdēvētas kolonnas atbilstoši to saturam (novērsta sākotnējā kļūdainā nobīde)
  colnames(hbm) <- col_names
  # print(head(hbm))
  hbm <- data.frame(apply(hbm, 2, function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))


  # Noņem overflow datus, kuri iepriekš tika pārtaisīti par NA
  # hbm <- na.omit(hbm)


  # Tabulas papildināšana ar laika kolonnu
  time <- start_time + lubridate::seconds_to_period(hbm$Time)
  hbm$Time_full <- as.POSIXct(time, format = "%Y-%m-%d %H:%M:%OS6")


  return(hbm)
}










