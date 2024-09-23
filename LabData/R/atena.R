#' Returns list of monitor files
#'
#' This function returns list of monitor files in a folder.
#'
#'
#' @param dir_path a folder containing monitor files
#' @export
tuvinajumi.saraksts <- function(dir_path) {
  faila_nos <- list.files(dir_path)
  n_files <- length(faila_nos)
  nr_pk <- (1 : n_files)
  par_kopas <- as.data.frame(cbind(nr_pk, faila_nos))

  return(par_kopas)

}

