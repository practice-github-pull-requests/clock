\name{modulus}
\alias{modulus}
\title{Modulus of integers}
\description{Getting and setting the working modulus for clock arithmetic}
\usage{
modulus(n)
}
\arguments{
  \item{n}{Non-negative integer, \eqn{>1}}
}
\details{

  The \emph{sine qua non} for modular arithmetic is having a modulus to
work to.  The first thing to do is to set the modulus with function
\code{modulus()}.  The typical work-flow for the package is to define a
modulus and work in this modulus for an extended period of time.  It is
not expected that users will change the working modulus very frequently.
Indeed, doing so may result in \emph{interwoven alignment preambles are
not allowed} errors.

The package can deal with a non-prime modulus but then division is not
defined.

The function and workflow are based on the \code{sol()} function of the
\pkg{lorentz} package.

}
\author{Robin K. S. Hankin}
\note{
  Typing \dQuote{\code{modulus(5)}} is a lot easier than typing
  \dQuote{\code{options("M"=5)}}, which is why the package uses
  the idiom that it does.
}
\seealso{\code{\link{clock}}}
\examples{

modulus(7)
table(as.mod(1:1000) == 4)
}
