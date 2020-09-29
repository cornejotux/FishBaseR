\name{fb_agesize}
\alias{fb_agesize}
\title{Parse the data on Age at Size from fishbase Age/Size page, when available.}
\description{
Download, as a table, the data on Age at Size. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_agesize(idFB, Genus, Species, server)
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
idFB and Stock code are required, if are not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_agesize(server = "http://www.fishbase.de/", idFB = 111)
}
\keyword{FishBase}
\keyword{Age}
\keyword{Size}
