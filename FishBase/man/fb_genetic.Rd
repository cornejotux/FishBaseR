\name{fb_genetic}
\alias{fb_genetic}
\title{
Obtain the list of genetic information for the specie.
}
\description{
Download, as a table, the list Chromosome numbers for locallities and countries. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_genetic(idFB, Genus, Species, server)
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
    \item{Locality}{Where the genetic sample was taken.}
    \item{Country}{Country where the genetic sample is coming from.}
    \item{HaploidGam}{Chromosome number of the haploid gamete.}
    \item{DiplidZygI}{Chromosome number of the diploid gamete.}
    \item{DiplidZygII}{Chromosome number of the diploid gamete.}
    \item{GenMarkers}{Genetic markers.}
    \item{Reference}{Reference id used by FishBase.}
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_genetic(idFB = 244)
}
\keyword{FishBase}
\keyword{Genetic}



