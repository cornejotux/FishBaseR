\name{fb_metabolism}
\alias{fb_metabolism}
\title{
Obtain a list of metabolism data of the species.
}
\description{
Download, as a table, the list of data available on metabolism. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_metabolism(idFB, StockCode, Genus, Species, server)
}
\arguments{
  \item{idFB}{
FishBase id of the specie to download the data
}
  \item{StockCode}{
FishBase StockCode of the specie to download the data
}
  \item{Genus}{
Genus part of the scientific specific name to download the data
}
  \item{Species}{
Species part of the scientific specific name to download the data
}
  \item{server}{
Optional, set up the desired (closer, faster) FishBase server
}
}
\details{
idFB is required. If is not available, it is possible to use Genus and Species and the function \link{fb_ids} will obtain the idFB.
}
\value{
    \item{OxCon}{Oxygen Consumption in mg/kg/h.}
    \item{OxCon_20degC}{Oxygen Consumption in mg/kg/h at 20 degC.}
    \item{Weight}{Weight in grams (g).}
    \item{Salinity}{Do not use utils or PSU.}
    \item{Activity}{The activity done during the measurement.}
    \item{Stess}{What kind of stress wass applied during the measurement.}
  }
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_metabolism(idFB = 236, StockCode = 250)
}
\keyword{FishBase}
\keyword{List}
\keyword{Countries}
\keyword{Common}
\keyword{Vernacular}
\keyword{Synonyms}
