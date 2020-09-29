\name{fb_synonyms}
\alias{fb_synonyms}
\title{
Obtain a list of synonyms of the species.
}
\description{
Download, as a table, the list of the synonyms. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_synonyms(idFB, Genus, Species, server)
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
fb_synonyms(idFB = 24)
}
\keyword{FishBase}
\keyword{List}
\keyword{Countries}
\keyword{Common}
\keyword{Vernacular}
\keyword{Synonyms}
