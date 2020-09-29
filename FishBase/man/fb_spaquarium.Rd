\name{fb_spaquarium}
\alias{fb_spaquarium}
\title{
Obtain the a table with a list of aquarium specioes.
}
\description{
Download, as a table, the a lsit of aquarium species for the country. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_spaquarium(idCountry, server)
}
\arguments{
  \item{idCountry}{
FishBase id for the country the the aquarium list of speciel will be obtained.
}
  \item{server}{
Optional, set up the desired (closer, faster) FishBase server
}
}

\details{
idCountry is required. A list of countries and their id can be obtained as data(\link{countries}).
}
\seealso{
  \link{countries}
}
\examples{

fb_spaquarium(idCountry = 32)
}
\keyword{FishBase}
\keyword{Species}
\keyword{Aquarium}
