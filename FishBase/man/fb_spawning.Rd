\name{fb_spawning}
\alias{fb_spawning}
\title{
Obtain a list of months where the specie is spawning at each country and locality listed.
}
\description{
Download, as a table, the list of month where the specie is spawning at each countri and locality. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_spawning(idFB, StockCode, Genus, Species, server)
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
idFB and StockCode are required. If they are not available, it is possible to use Genus and Species and the function \link{fb_ids} will obtain the idFB.
}
\note{
The number on each month are not very clear where are coming or what the exactly mean, please use it with care.
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_spawning(idFB = 24, StockCode = 32)
}
\keyword{FishBase}
\keyword{Spawning}
\keyword{Redroduction}
\keyword{Country}


