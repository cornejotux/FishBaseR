\name{fb_comnames}
\alias{fb_comnames}
\title{
Obtain a list of common names of the species and where are used.
}
\description{
Download, as a table, the list of the common names of the species, where are used and the language. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_comnames(idFB, Genus, Species, server)
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
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_comnames(idFB = 24, server = "http://www.fishbase.de/")
}
\keyword{FishBase}
\keyword{List}
\keyword{Countries}
\keyword{Common}
\keyword{Vernacular}
