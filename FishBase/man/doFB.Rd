\name{do.fb}
\alias{do.fb}

\title{ Apply a FishBase scraping function to the provided data frame.}
\description{
This function is a loop to use a function in each of the organisms in the data frame.
}
\usage{
do.fb(fun, data, iStart, wait, server)
}

\arguments{
  \item{fun}{
a function in the FishBaseR package to obtain data from FishBase
}
  \item{data}{
A data frame containint the columns idFB, Genus and Species, to be used by the function.
}
  \item{iStart}{
Row of the data frame where to start the loop. This is usefull in case the loop stop, it is possible to start in the next row and obtain the complete set of data, without requiring start from the beginning.
}
  \item{wait}{
Time in seconds to wait after 100 records are obtained.
}
  \item{server}{
The server from where the data will be obtained. e.g. 'http://www.fishbase.org/', 'http://www.fishbase.us/'
}
}
\details{
This function requires a well formated data frame. It is required idFB, Genus and Species with the proper column name.
}
\value{
This function will return a data frame with data depending on the function provided.
}

\examples{
do.fb(fun = fb_ids, data = data.frame(Genus="Engraulis", Species="ringens"), iStart = 1, wait = 0, server = 'http://www.fishbase.org/')
}
\keyword{ FishBase }
\keyword{ R }
\keyword{ FishBaseR }

