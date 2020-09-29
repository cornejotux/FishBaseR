\name{fb_speed}
\alias{fb_speed}
\title{
Obtain the list of speed measurements for the specie.
}
\description{
Download, as a table, the data on Swimming Speeds. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_speed(idFB, Genus, Species, server)
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
  \value{
    \item{Speed}{Swimming speed on m/s.}
    \item{Lengths}{Swimming speed measured in Lengths/s.}
    \item{Mode}{Swimming type mode on what is base the measure.}
    \item{LengthType}{Type of length measurement (e.g. Standat length, total length, fork length, etc.)}
  }
\seealso{
	\code{\link{fb_ids}}
}
\examples{
fb_speed(Genus = "Clupea", Species = "harengus")
}

\keyword{FishBase}
\keyword{Swiming}
\keyword{speed}
