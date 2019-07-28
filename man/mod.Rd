\name{mod}
\alias{mod}
\alias{modulo}
\alias{as.mod}
\alias{is.mod}
\title{Working with integers to a particular modulus}
\description{
Working with integers to a particular modulus
}
\usage{
mod(x)
as.mod(x)
is.mod(x)
}
\arguments{
  \item{x}{Vector, coerced to integer}
}
\details{

After setting the modulus with function \code{modulus(n)}, function
\code{mod()} returns a \code{clock} object, that is, a vector of
integers expressed modulo \code{n}.  Function \code{is.mod()} tests for
an object being of class \code{clock}.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{clock}}}
\examples{

modulo(11)
as.mod(10)^100  # A googol, modulo 11
}