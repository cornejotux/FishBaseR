\name{fb_maturity}
\alias{fb_maturity}
\title{
Parse the Maturity data from fishbase, when available.
}
\description{
Download, as a table, the data on maturity. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_maturity(idFB, Genus, Species, server)
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
fb_maturity(idFB = 24)
}
\keyword{FishBase}
\keyword{Maturity}
\keyword{Reproduction}
