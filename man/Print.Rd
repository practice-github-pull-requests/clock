\name{Print}
\alias{print.clock}
\alias{show,clock-method}
\title{Methods for printing clock objects}
\description{
Methods for printing clock objects, specifically for reminding the user
that the entries are not integers.
}
\usage{
\method{print}{clock}(x, ...)
}
\arguments{
  \item{x}{An object of class \code{clock}}
  \item{...}{Further arguments (currently ignored)}
}
\author{Robin K. S. Hankin}
\examples{
modulus(10)
a <- as.mod(1:100)
dput(a)
a

}
\keyword{math}
