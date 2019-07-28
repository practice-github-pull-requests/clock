\name{Extract.clock}
\alias{[.clock}
\alias{[<-.clock}
\alias{[,clock-method}
\alias{[<-,clock-method}
\title{Extract or replace parts of clock objects}
\description{
   Methods for \code{"["} and  \code{"[<-"}, i.e., extraction or
   subsetting of clock objects.
}
\arguments{
  \item{x}{Object of class clock}
  \item{i}{elements to extract or replace}
  \item{value}{replacement value}
}
\value{
  Always return an object of the same class as \code{x}.
}
\author{Robin K. S. Hankin}
\examples{
modulus(13)
as.mod(11)^(1:10)

}
\keyword{math}
