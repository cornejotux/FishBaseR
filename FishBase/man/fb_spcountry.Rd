\name{fb_spcountry}
\alias{fb_spcountry}
\title{
Obtain all the fish species recorded in FishBase for an especific country. 
}
\description{
Download, as a table, the species list registered in FishBase for an specific country. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
It is required to know the country id, here is provided as data.frame named \link{countries}.
}
\usage{
fb_spcountry(idCountry, server)
}
\arguments{
  \item{idCountry}{id used by Fishbase for each country}
  \item{server}{Server from where geting the data}
  }
\examples{
data(countries)
fb_spcountry(idCountry = countries$idCountry[10], server= "http://www.fishbase.de/")
}

\keyword{FishBase}
\keyword{Country}
\keyword{Species}