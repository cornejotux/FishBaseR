\name{fb_lengthweigth}
\alias{fb_lengthweigth}

\title{
Parse the Length-Weigth parameters from fishbase, when available.
}
\description{
Download, as a table, the parameters of Length-Weight relation. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_lengthweigth(idFB, Genus, Species, server)
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
fb_lengthweigth(idFB = 73)
}

\keyword{FishBase}
\keyword{Length}
\keyword{Weigth}
