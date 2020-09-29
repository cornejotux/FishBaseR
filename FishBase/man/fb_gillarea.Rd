\name{fb_gillarea}
\alias{fb_gillarea}
\title{
Obtain the list of the relative gill area studies for the specie.
}
\description{
Download, as a table, the list of the relative gill area stutied for the specie. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_gillarea(idFB, StockCode, Genus, Species, server)
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
idFB and Stock code are required, if they are not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\value{
  \item{BodyWeight}{The weight of the body in grams (g).}
  \item{GillArea}{Gill area in cm^2.}
  \item{GillAreaWeight}{Ratio of Gill Area over the weight in cm^2/g.}
  \item{GAWReference}{Reference of the the gill area and gill area weight ratio study.}
  \item{BlodWaterDist}{Blood water distance in m microns (um).}
  \item{BWDReference}{Reference of the blodd water distance study.}
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_gillarea(idFB = 24, StockCode = 32)
}
\keyword{FishBase}
\keyword{Gill}