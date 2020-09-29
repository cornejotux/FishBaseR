\name{fb_fao}
\alias{fb_fao}
\title{
Ontain a list of the FAO areas where specie is present.
}
\description{
Download, as a table, the list of FAO areas where records of the specie exist. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_fao(idFB, Genus, Species, server)
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
idFB is required, if is not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_fao(idFB = 24, server = "http://www.fishbase.de/")
}
\keyword{FishBase}
\keyword{List}
\keyword{FAO}
\keyword{Distribution}
