#' Matest compression machine test results
#'
#' This function returns a data frame from a text file exported from Matest compression machine.
#' In the file usualy results from one test specimens are stored. This function reads only 4 columns,
#' which is the case if only force, piston displacement and time is measured.
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing test results - raw data.
#' @export
matestDataFrame <- function (file) {

  df_full <- read.table(text = chartr("[]", "''",
                                   readLines(file, skipNul = T)),
                     fileEncoding = "UTF-16LE",
                     sep = "\t",
                     col.names = 1:4,
                     header = F,
                     fill = T)

  # Heaer row
  row_header <- grep("GRAPH", df_full[,1]) + 1

  # Row numbers, where the data for plotting diagrams starts and ends
  row_start <- grep("GRAPH", df_full[,1]) + 2
  row_end <- grep("CHECKSUM", df_full[,1]) - 1

  # Subsetting data frame to include only the necessary data
  df <- df_full[row_start:row_end, 1:3]

  # Column names
  col_names <- gsub("\\.", "",
                  gsub(" .*", "", df_full[row_header, 1:3]))

  colnames(df) <- col_names

  # Resseting row numbers
  rownames(df) <- NULL

  # Converting tu numeric
  df <- as.data.frame(apply(df, 2, function (x) as.numeric(as.character(x))))


}


#' Matest compression machine test results
#'
#' This function returns a data frame from a text file exported from Matest compression machine.
#' In the file usualy results from one test specimens are stored. This function reads 5 columns,
#' which is the case if E modulus according to ASTM is being determined.
#'
#'
#' @param file A file name of a text file where the exported data are saved.
#' @return A data frame containing test results - raw data.
#' @export
matestDataFrameEmod <- function (file) {

  df_full <- read.table(text = chartr("[]", "''",
                                   readLines(file, skipNul = T)),
                     fileEncoding = "UTF-16LE",
                     sep = "\t",
                     col.names = 1:7,
                     header = F,
                     fill = T)


  # Heaer row
  row_header <- grep("GRAPH", df_full[,1])[2] + 1

  # Row numbers, where the data for plotting diagrams starts and ends
  row_start <- grep("GRAPH", df_full[,1])[2] + 2
  row_end <- grep("CHECKSUM", df_full[,1]) - 1


  # Subsetting data frame to include only the necessary data
  df <- df_full[row_start:row_end, 1:5]

  # Column names
  col_names <- gsub("\\.", "",
                  gsub(" .*", "", df_full[row_header, 1:5]))

  colnames(df) <- col_names

  # Resseting row numbers
  rownames(df) <- NULL



  # Converting tu numeric
  df <- as.data.frame(apply(df, 2, function (x) as.numeric(as.character(x))))


}



#' Matest compression machine test results
#'
#' This function returns data of a tested specimen such as:
#' Test name,
#' Test date,
#' Test time,
#' Start load,
#' Area,
#' Maximum load,
#' Maximum strength,
#' Deform
#'
#'
#' @param file A file name of a text file where the test data are saved.
#' @return A data frame containing test results - raw data.
#' @export
matestResult <- function (file) {

  df_full <- read.table(text = chartr("[]", "''",
                                   readLines(file, skipNul = T)),
                     fileEncoding = "UTF-16LE",
                     sep = "\t",
                     col.names = 1:4,
                     header = F,
                     fill = T)


  row_start <- 2
  row_end <- grep("GRAPH", df_full[,1]) - 1

  df_res <- strcapture("(.*): (.*)", as.character(df_full[row_start:row_end, 1]),
                                      data.frame(Param = "", Value = ""))


  selected <- c("Test",
                "Test date",
                "Test time",
                "Start load",
                "Area",
                "Maximum load",
                "Maximum strength",
                "Deform."
  )




  df_res <- df_res[grep(paste(selected, collapse = "|"), df_res$Param), ]



  test_date <- df_res[grep("Test date", df_res$Param), 2]
  test_time <- df_res[grep("Test time", df_res$Param), 2][1]


  time_tested <- lubridate::parse_date_time(paste(test_date, test_time),
                                            "%d/%m/%Y %I:%M:%S %p", tz = "Europe/Riga")

  df <- data.frame(Specimen = df_res[1,2],
                   TestTime = time_tested,
                   Area = as.numeric(sub(" .*", "",df_res[grep("Area", df_res$Param),2])),
                   MaxLoad = as.numeric(sub(" .*", "",df_res[grep("Maximum load", df_res$Param),2])),
                   Strength = as.numeric(sub(" .*", "",df_res[grep("Maximum strength", df_res$Param),2])))


  return (df)

}


#' Matest compression machine test results
#'
#' This function returns data of a tested specimen such as:
#' Test name,
#' Test date,
#' Test time,
#' Axial gage length,
#' Axial youngs modulus,
#' Poissons ratio
#'
#'
#' @param file A file name of a text file where the test data are saved.
#' @return A data frame containing test results.
#' @export

matestResultEmod <- function (file) {

  df_full <- read.table(text = chartr("[]", "''",
                                   gsub("'s", "s",
                                        readLines(file, skipNul = T))),
                     fileEncoding = "UTF-16LE",
                     sep = "\t",
                     col.names = 1:7,
                     header = F,
                     fill = T)


  row_start <- 2
  row_end <- grep("CALCULATION", df_full[,1])[1] - 1


  df_res <- strcapture("(.*): (.*)", as.character(df_full[row_start:row_end, 1]),
                                      data.frame(Param = "", Value = ""))



  selected <- c("Test",
                "Test date",
                "Test time",
                "Maximum load",
                "Axial gage length",
                "Lateral gage length",
                "Area",
                "Axial youngs modulus",
                "Poissons ratio"
  )



  df_res <- df_res[grep(paste(selected, collapse = "|"), df_res$Param), ]







  test_date <- df_res[grep("Test date", df_res$Param), 2]
  test_time <- df_res[grep("Test time", df_res$Param), 2][1]


  time_tested <- lubridate::parse_date_time(paste(test_date, test_time),
                                            "%d/%m/%Y %I:%M:%S %p", tz = "Europe/Riga")


  df <- data.frame(Specimen = df_res[1,2],
                   TestTime = time_tested,
                   Area = as.numeric(sub(" .*", "",
                                         df_res[grep("Area", df_res$Param),2])),
                   LoadMax = as.numeric(sub(" .*", "",
                                            df_res[grep("Maximum load", df_res$Param),2])),
                   gageLength = as.numeric(sub(" .*", "",
                                               df_res[grep("Axial gage length", df_res$Param),2])),
                   Emod = as.numeric(sub(" .*", "",
                                         df_res[grep("Axial youngs modulus", df_res$Param),2])),
                   Poison = as.numeric(sub(" .*", "",
                                           df_res[grep("Poissons ratio", df_res$Param),2])))


  return (df)

}





#' Calculate E modulus from Matest diagrams
#'
#' This function returns data frame containing E modulus and Poisson's ratio,
#' taking form Matest load vs strain diagrams.
#' It assumes that each specimen is in a separate file and all the files ar in one folder.
#'
#'
#' @param folder A folder path conteining txt files (E modulus) exported from Matest machine.
#' @param load_levels a vector consisting of two values defining load levels at which E modulus is calculated.
#' @return A data frame containing test results.
#' @export
matestCalcEmod <- function (folder, load_levels) {
  # Dataframe to store E modulus calculated from diagrams
  df_Emod <- data.frame()

  # Load levels to calculate E modulus
  load_levels <- c(35, 327)

  # Loop through files (each file is one specimen)
  for (f in list.files(folder)) {
    # full file path
    file <- paste0(folder, "/", f)
    # data frame containig data for load-strain diagrams
    df01 <- matestDataFrameEmod(file)

    # cross-section area, mm2
    area <- matestResultEmod(file)[, "Area"]

    # labels of the specimen
    Kopa <- sub("\\.txt", "", f)

    label <- sub(" .*", "",
                 gsub("-", "_", Kopa))


    # find rows in current data frame (diagram) corresponding the given load levels
    p1 <- which.min(abs(df01$Load - load_levels[1]))
    p2 <- which.min(abs(df01$Load - load_levels[2]))

    # select corresponding axial strains
    eps <- c(df01$Axial[p1], df01$Axial[p2])
    # select corresponding lateral strains
    epsh <- c(df01$Lateral[p1], df01$Lateral[p2])

    # select corresponding loads (exact values from the data)
    load <- c(df01$Load[p1], df01$Load[p2])

    # Calculate stresses from loads
    sig <- load * 1000 / area

    # Calculate axial E modulus
    Emod <- diff(sig) / (diff(eps)/10^3)
    # Calculate lateral E modulus
    Emodh <- diff(sig) / (diff(epsh)/10^3)
    # Poisson's ratio
    mu <- Emod / Emodh

    # Store the results in a data frame
    df_Ei <- data.frame(Kopa = Kopa, label = label, Emod = Emod, Poisson = mu)

    # Combine current results with previous
    df_Emod <- rbind(df_Emod, df_Ei)


  }

  return (df_Emod)
}



#' Matest compression machine test results
#'
#' This function function matestResult to all the files in a directory and all the subdirectories.
#'
#'
#' @param file A file name of a text file where the test data are saved.
#' @return A data frame containing test results - raw data.
#' @export
matestResultAll <- function (dir) {

  directories <- list.dirs(dir, recursive = FALSE)

  if (length(directories) != 0) {

    for (d in directories) {
      files <- paste0(d, "/", list.files(d))

      for (f in files) {
        dfij <- matestResult(f)
        if (f == files[1]) {
          dfi <- dfij
        } else {
          dfi <- rbind(dfi, dfij)
        }
      }

      if (d == directories[1]) {
        df <- dfi
      } else {
        df <- rbind(df, dfi)
      }

    }

    df$type <- basename(dir)

    return (df)

  } else {

    files <- paste0(dir, "/", list.files(dir))

    for (f in files) {
        dfi <- matestResult(f)
        if (f == files[1]) {
          df <- dfi
        } else {
          df <- rbind(df, dfi)
        }
      }

    df$type <- basename(dir)
    return (df)
  }

}