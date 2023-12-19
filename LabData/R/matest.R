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