% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{dataAll}
\alias{dataAll}
\title{Kombinē visus datus vienā tabulā}
\usage{
dataAll(dir_path, funkcija)
}
\arguments{
\item{dir_path}{Folderis (mape), kurā atrodas viena veida datu faili}

\item{funkcija}{Funkcija, kas jāizmanto katra faila pārveidošanai par datu masīvu.}
}
\value{
Tiek izveidots datu masīvs (data.frame) ar visiem paraugiem un kopām.
}
\description{
Šī funkcija ļauj salikt vinā tabulā visus vienā mapē esošos failus atbilstoši
norādītajai funkcijai.
}
