\name{fb_fecundity}
\alias{fb_fecundity}
\title{This function do stuff}
\description{
This function do stuff
}
\usage{
fb_fecundity(idFB = NA, Genus = NA, Species = NA, server = "http://www.fishbase.org/")
}
%- maybe also 'usage' for other objects documented here.
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
idFB code is required, if are not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}

\examples{
fb_fecundity(idFB = 4)
  }

\keyword{ FishBaseR }
\keyword{ FishBase }
\keyword{ Fecundity }
