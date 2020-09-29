\name{fb_introductions}
\alias{fb_introductions}
\title{
Parse the webpage of introductions info.
}
\description{
Download as a table, the data on Introcutions. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.

The Introducion data include year, from and where the introducion accurs.
}
\usage{
fb_introductions(idFB, StockCode, Genus, Species, server)
}
\arguments{
  \item{idFB}{FishBase id of the specie to download the data}
  \item{StockCode}{FishBase StockCode of the specie to download the data}
  \item{Genus}{Genus part of the scientific specific name to download the data}
  \item{Species}{Species part of the scientific specific name to download the data}
  \item{server}{Optional, set up the desired (closer, faster) FishBase server}
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_introductions(idFB = 236, StockCode = 250)
}

\keyword{ FishBase }
\keyword{ Introductions }
\keyword{ Countries }
