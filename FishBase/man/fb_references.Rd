\name{fb_references}
\alias{fb_references}
\title{
Obtain the list of publication where the data was collected from.
}
\description{
Download, as a table, the list of references from where the data collected and available in FishBase is coming from. Read the HTMLwebpage, recognize the table and transform it into a Data.Frame object.
}
\usage{
fb_references(idFB, Genus, Species, server)
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
    \item{idRef}{This is the id used in the field Ref or References in the data tables coming from FishBase.}
    \item{Description}{Complete formated reference of the article/book/etc.}
    \item{Year}{Year of the publication}
    \item{SpNameUsed}{Name of the specie used in the reference.}
    \item{Page}{Number of pages.}
  }
\seealso{
  \code{\link{fb_ids}}
}
\examples{
fb_references(idFB = 1477)
}
\keyword{FishBase}
\keyword{References}
\keyword{Cite}



