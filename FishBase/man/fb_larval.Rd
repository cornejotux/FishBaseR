\name{fb_larval}
\alias{fb_larval}
\title{
Parse the data on larval dynamics and energetics from fishbase, when available.
}
\description{
Parse the data on larval dynamics and energetics from fishbase, when available.
}
\usage{
fb_larval(idFB, StockCode, Genus, Species, server)
}

\arguments{
  \item{idFB}{
FishBase id of the specie to download the data
}
  \item{StockCode}{
FishBase StockCode of the specie to download the data
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
idFB and Stock code are required, if they are not available, it is possible to use Genus and Species. In the case when Genus and Species is used, the function will invoque \link{fb_ids} to obtain idFB and StockCode.
}
\seealso{
	\code{\link{fb_ids}}
}
\value{
  \item{MainRef}{Main reference for the data}
  \item{Ecosystem}{Ecosystem type}
  \item{Temp}{Temperature in degC }
  \item{DryWHatch}{Dry weigth at hatching in micrograms (um)}
  \item{DryWMeta}{Dray weigth at metamorphosis in micrograms (um)}
  \item{GrowthCoef}{Growth coefficient in grams per day (g/d)}
  \item{MRate}{Mortality rate (1/d)}
  \item{O2Comp}{Oxygen consumption in microliters per miligram per hour (ul/mg/h)}
  \item{FoodInge}{Food ingestion (1/d)}
  \item{Comments}{General comments}
  \item{References}{ids for other references used}
}
\examples{
fb_larval(idFB = 24, StockCode=32)
}
\keyword{FishBase}
\keyword{Larvae}
\keyword{Dynamics}
\keyword{Energetics}
