\name{fb_growth}
\alias{fb_growth}

\title{
Parse the data on Growth from fishbase Growth page, when available.
}
\description{
Download, as a table, the data on Growth. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_growth(idFB, Genus, Species, server)
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
\seealso{
	\code{\link{fb_ids}}
}
\details{
idFB code is required, if is not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB .
}
\examples{
fb_growth(Genus = "Trachurus", Species = "murphyi")
}
\keyword{FishBase}
\keyword{Growth}
