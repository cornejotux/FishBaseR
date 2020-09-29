\name{fb_ecotox}
\alias{fb_ecotox}
\title{
Obtain a list of CL50 by chemical used.
}
\description{
Download, as a table, the list of chemical and their CL50 for the specie. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_ecotox(idFB, Genus, Species, server)
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
idFB is required. If is not available, it is possible to use Genus and Species and the function \link{fb_ids} will obtain the idFB.
}
\value{
    \item{ChemName}{Chemical name used in the Ecotoxycology assay.}
    \item{CL50}{Concentration (mg/l) at half of the individual dies.}
    \item{ExpTime}{Duration time of the assay.}
    \item{Reference}{Reference ID to the cite in FishBase system.}
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_ecotox(idFB = 236, server = "http://www.fishbase.de/")
}
\keyword{FishBase}
\keyword{CL50}
\keyword{bioassay}
\keyword{ecotoxycoloy}