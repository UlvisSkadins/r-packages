#' Nolasa datus no RissFox Mini un pārveido par 'data frame' tabulu
#'
#' Šī funkcija paņem datus, kas eksportēti no RissFox Mini data logger.
#' Kolonnu nosaukumi tiek nolasīti no izejas faila.
#' Šī funkcija veidota RissFox iekārtām (Mini un Multisensor), kas iegādātas
#' 20... gadā ... (serijas Nr., modelis ..) un vadītas ar programmu ...
#'
#'
#' @param fails Ceļš līdz failam un faila nosaukums ar paplašinājumu.
#' Svarīgi, ka tas ir txt fails. Jo programma eksportē kā teksta failu, bet
#' kļūdaini paplašinājumu uzdod xls. Tāpēc šis paplašinājums katram failam ir
#' jāpārmaina uz txt.
#' @return Tabula 'data frame' formātā ar atbilstošiem datu formātiem.
#' @export
rissFoxDataFrame <- function (fails){
  # Nolasa tabulu
  table <- read.table(fails,
                    fileEncoding = "UTF-8",
                    sep = "\t",
                    fill = T,
                    stringsAsFactors = F)

  # ------
  # Nosaka tabulas izmantojamās daļas robežas

  # sākuma rinda ar kolonnu nosaukumiem:
  header_row <- which(table[,1]=="Date")

  # jāatmet pēdējā rinda, kurā ir vienkārši vārds "End"
  last_row <- which(table[,1]=="End") - 1

  # pēdējā kolonna tiek uzskatīta tā, kurā vēl ir dati
  # pēc tam ir tukšā kolonna, kas sastāv no NA
  # tāpēc to atmetam
  last_col <- sum(!is.na(table[1,]))

  # ------
  # Kolonnu nosaukumi
  #
  # definē jaunu lielumu, kam piešķir kolonnu nosaukumiem
  # atbilstošo tabulas rindu
  header <- table[header_row, 1:last_col]

  # pārveido vektoru formā
  header <- unname(unlist(header))

  # novienkāršo nosaukumus, nodzēšot pirmo atstarpi un visu aiz tās
  # tā tiek iegūts vektors ar vienkāršotiem kolonnu nosaukumiem
  header <- sub(" .*", "", header)

  # ------
  # Datu tabulas jaunais veidols
  # Tiek izveidota jauna tabula ar nosaukumu 'data', kura sastāv
  # tikai no derīgā datu apgabala un tai tiek piešķirti vienkāršotie
  # kolonnu nosaukumi

  # derīgā datu apgabala piešķiršana
  data <- table[(header_row+2):last_row, 1:last_col]

  # kolonnu nosaukumu piešķiršana
  colnames(data) <- header



  # ------
  # Datu formāti
  # Tiek konvertēts esošais datu formāts (character) uz datiem atbilstošo formātu

  # datums un laiks
  # datums tiek konvertēts uz 'Date' fromātu (pieraksts teksta failā, piem., 02.10.23.)
  data[,"Date"] <- as.Date(data$Date,"%d.%m.%y")

  # laiks tiek konvertēts uz POSIXct formātu, datumu ņemot no "Date" kolonnas
  data[,"Time"] <- as.POSIXct(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M:%S")

  # skaitļi (numeric)
  # pārējās kolonnas satur skaitliskās vērtības, kas tiek pārveidotas
  # uz 'numeric' fromātu
  # pārējo kolonnu diapazons (vektors) ir tās, kuru nosaukumi nav Date vai Time:
  num_col_range <- header[which(header != c("Date", "Time"))]

  # pirms konvetācijas komats tiek aizstāts ar punktu
  data[,num_col_range] <- as.data.frame(sapply(data[,num_col_range], function(x)
                                     as.numeric(gsub(",",".",x))))

  # -----
  # Noņem liekās rindas (tās, kurās ir NA), ja tādas ir.
  # Šeit vadās pēc datuma kolonnas, jo mērījumu kolonnās var būt
  # citi iemesli, kāpēc nav nolasījumi un parādās NA
  data <- data[!is.na(data$Date),]


  return(data)
}




