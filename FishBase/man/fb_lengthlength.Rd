\name{fb_lengthlength}
\alias{fb_lengthlength}
\title{
Obtain a list of parameters estimation to transform length measurements.
}
\description{
Download, as a table, the list of parametes to use a linear regrestion to transform between defferent length measurements, this is from Total Length to Fork Length, Standar Length to Total Length, etc. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_lengthlength(idFB, Genus, Species, server)
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
idFB is required. If it is not available, it is possible to use Genus and Species and the function \link{fb_ids} will obtain the idFB.
}
\value{
    \item{UnkLength}{The dependent variable, the type of length obtained using the linear transformation.}
    \item{a}{Slope of the regresion line.}
    \item{b}{Intercept of the regresion line.}
    \item{KwnonLength}{Independent variable, the type of length measured and to be transformed into the UnkLength using the regresion.}
    \item{r}{}
    \item{Sex}{Sex of the fish used on the parameters estimation.}
    \item{MinLength}{Minimun length in the range that can be used the transformation.}
    \item{MaxLength}{Maximum length in the range that can be used the transformation.}
}
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_lengthlength(idFB = 1477)
}
\keyword{FishBase}
\keyword{Length}
\keyword{Tranformation}



