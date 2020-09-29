\name{fb_ecosystems}
\alias{fb_ecosystems}
\title{
Obtain a list of ecosystems were the specie is present.
}
\description{
Download, as a table, the list of ecosystems where records of the specie exist. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_ecosystems(idFB, Genus, Species, server)
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
idFB, Genus and Species are required. If is not available, it is possible to use Genus and Species and the function \link{fb_ids} will obtain the idFB.
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_ecosystems(idFB = 24, server = "http://www.fishbase.de/")
}
\keyword{FishBase}
\keyword{List}
\keyword{ecosystems}
\keyword{Distribution}
