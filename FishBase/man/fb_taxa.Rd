\name{fb_taxa}
\alias{fb_taxa}
\title{
Obtain the taxonomic classification
}
\description{
Parse the XML file to obtain the Taxonomic classification of the specie.
}
\usage{
fb_taxa(idFB, Genus, Species, server)
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
idFB code is required, if is not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\seealso{
	\code{\link{fb_ids}}
}
\examples{
fb_taxa(idFB = 111)
}
\keyword{FishBase}
\keyword{Taxa}
\keyword{Clasification}

