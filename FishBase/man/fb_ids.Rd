\name{fb_ids}
\alias{fb_ids}
\title{
Parse the main page of the specie to get specie id u and Stock Code used in the FishBase database.
}
\description{
Download the main webpage of the specie as html. Then process the code searching for the id and Stock Code used in the FishBase database.
This function is called by all the other functions when the idFB or StockCode are nor provided.
}
\usage{
fb_ids(Genus, Species, server)
}
\arguments{
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

\examples{
fb_ids(Genus = "Carangoides", Species = "bartholomaei")
}
\keyword{FishBase}
