\name{cclock}
\alias{cclock}
\title{Combine clock  vectors}
\description{
  Combine  clock vectors through concatenation
}
\usage{
cclock(x, ...)
}
\arguments{
  \item{x}{Object of class \code{clock}; an integer vector}
  \item{\dots}{Other arguments coerced to clock objects}
}
\details{

  If any argument has class \code{clock}, all arguments are coerced to
  \code{clocks}s.

  Function \code{cclock()} operates recursively, calling \code{.cPair()}
  repeatedly.  Function \code{.cPair()} uses S4 method dispatch to call
  \code{.clock.cpair()}.}

\author{Robin K. S. Hankin; original idea due to John Chambers and R-core}
\note{
  The structure is copied from the \pkg{Brobdingnag} package, where
  there is some discussion under \code{cbrob.Rd}.
}
\examples{
modulus(4)
a <- mod(3)
cclock(1:10,10:1,a,1:10)
}
\keyword{math}
