\name{fb_foodconsumption}
\alias{fb_foodconsumption}
\title{
Parse the data on Food Consumption from fishbase, when available.
}
\description{
Download, as a table, the data on Food Consumption. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_foodconsumption(idFB, Genus, Species, server)
}
\arguments{
  \item{idFB}{
FishBase id of the specie to download the data
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
idFB is required, if are not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_foodconsumption(idFB = 24)
}
\keyword{FishBase}
\keyword{Diet}
\keyword{Food}
\keyword{Consumption}
