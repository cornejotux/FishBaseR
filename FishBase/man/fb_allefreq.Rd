\name{fb_allefreq}
\alias{fb_allefreq}
\title{
Obtain the list of populations with available electrophoretic studies.
}
\description{
Download, as a table, the list of populations with available electrophoretic studies. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_allefreq(idFB, Genus, Species, server)
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
idFB is required. If it is not available, it is possible to use Genus and Species and the function \link{fb_ids} will obtain the idFB.
}

  \value{
    \item{Locality}{Where the population with electrophoretic studies was sampled.}
    \item{Country}{Country where the population with electrophoretic studies was sampled.}
    \item{TotalLoci}{Total numer of loci in the study.}
    \item{HeterObs}{NA}
    \item{HeterExp}{NA}
    \item{PolyMoLoci}{NA}
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_allefreq(idFB = 2944, server = "http://www.fishbase.de/")
}
\keyword{FishBase}
\keyword{Electrophoretic}
\keyword{Electrophoresis}
\keyword{Population}