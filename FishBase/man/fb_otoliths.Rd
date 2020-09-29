\name{fb_otoliths}
\alias{fb_otoliths}
\title{
Obtain the list of otoliths measurements for this specie.
}
\description{
Download, as a table, the list of otoliths measurements for the specie. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_otoliths(idFB, Genus, Species, server)
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
idFB is required, if it is not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\value{
    \item{Type}{What otolith was use for the measurement.}
    \item{FishLength}{Length of the fish in mm.}
    \item{OtolithLength}{Length of the otolith in mm.}
    \item{OtolithHeight}{Height of the otolith in mm.}
    \item{Face}{}
    \item{Position}{}
    \item{Locality}{ Where the sample was take.}
  }
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_otoliths(idFB = 1477)
}
\keyword{FishBase}
\keyword{Otoliths}