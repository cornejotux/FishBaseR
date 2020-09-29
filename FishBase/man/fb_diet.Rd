\name{fb_diet}
\alias{fb_diet}
\title{
Parse the data on Diet from fishbase Diet page, when available.
}
\description{
Download, as a table, the data on Diet. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_diet(idFB, StockCode, Genus, Species, server)
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
idFB and Stock code are required, if are not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_diet(idFB = 368, StockCode=314, server = "http://www.fishbase.de/")
}
\keyword{FishBase}
\keyword{Diet}
