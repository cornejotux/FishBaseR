\name{fb_tl}
\alias{fb_tl}

\title{
Parse the html code of the main page to obtain the Trophic Level.
}
\description{
Download the html code from the main webpase of the specie, and parse the code to find the Trophic Level with the stander deviation of the trophic level.
}
\usage{
fb_tl(Genus, Species, server)
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
\details{
In this function the Genus and Species is required.
}
\value{
This return a data frame containing:
\item{tl}{Trophic Level}
\item{tl_se}{Stantadar deviation of the trophic level as in FishBase}
}

\examples{
fb_tl(Genus = "Clupea", Species = "harengus")
}
\keyword{ FishBase }
\keyword{ Trophic }
\keyword{ Level }
