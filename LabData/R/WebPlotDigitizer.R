#' Converts WebPlotDigitizer csv file to list of dataframes
#'
#' This function converts csv files obtained from WebPlotDigitizer to list of data frames.
#' It assumes that the file consists of X and Y coordinates - two columns for each data frame.
#'
#'
#' @param csv_file a csv file exported form WebPlotDigitizer
#' @return list of data frames
#' @export
wpdDataFrames.xy <- function (csv_file) {
  # Reads csv file
  df <- read.csv(csv_file)
  # Number of dataframes asuming 2 columns (x and y) in each dataframe
  n_df <- length(colnames(df)) / 2


  # List to store dataframes
  list_df <- list()
  # Loop going through the data frames
  for (n in 1:n_df) {
    # increment
    i <- n-1
    # subsetting dataframe
    dfi <- df[-1, (n+i):(n+i+1)]
    # Label is the name of the first column.
    # Exporting data from WebPlotDigitizer, the name of the dataset is stored in there.
    label <- colnames(dfi)[1]
    # Renaming columns to X and Y
    colnames(dfi) <- c("X", "Y")

    # converting to numeric values
    dfi <- sapply(dfi, as.numeric)

    # removing empty rows
    dfi <- na.omit(dfi)
    # converting to data frame (possibly from matrix).
    # It is necessary to add a column with label
    dfi <- as.data.frame(dfi)
    # adding column with label
    dfi$label <- label

    # Combine all the datafrmes in one list
    list_df[[n]] <- dfi

  }
  return (list_df)
}