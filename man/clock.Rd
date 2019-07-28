\name{clock-class}
\Rdversion{1.1}
\docType{class}
\alias{clock-class}
\alias{clock}
\alias{.cPair,ANY,clock-method}
\alias{.cPair,clock,ANY-method}
\alias{.cPair,clock,clock-method}
\alias{coerce,integer,clock-method}
\alias{coerce,clock,numeric-method}
\alias{coerce,numeric,clock-method}
\alias{coerce<-,clock,numeric-method}
\alias{Compare,ANY,clock-method}
\alias{Compare,clock,ANY-method}
\alias{Compare,clock,clock-method}
\alias{initialize,clock-method}
\alias{Logic,ANY,clock-method}
\alias{Logic,clock,ANY-method}
\alias{Logic,clock,clock-method}
\alias{Math,clock-method}
\alias{Summary,clock-method}

\title{Class \code{"clock"}}
\description{
The \code{clock} class of objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{clock(...)}, but
this is not very user-friendly: the validation test is stringent. and
one should use \code{as.mod()} in day-to-day use.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{integer}  }
  }
}
\section{Extends}{
  \itemize{
\item Class \code{\linkS4class{integer}}, from data part.
\item Class \code{\linkS4class{numeric}}, directly, with explicit coerce.
\item Class \code{\linkS4class{double}}, by class
\code{\linkS4class{integer}}, distance 2, with explicit coerce.
\item Class \code{\linkS4class{vector}}, by class
\code{\linkS4class{integer}}, distance 2.
\item Class \code{\linkS4class{data.frameRowLabels}}, by class
\code{\linkS4class{integer}}, distance 2.
}
}
\section{Methods}{
  \describe{
    \item{.cPair}{\code{signature(x = "ANY", y = "clock")}: ... }
    \item{.cPair}{\code{signature(x = "clock", y = "ANY")}: ... }
    \item{.cPair}{\code{signature(x = "clock", y = "clock")}: ... }
    \item{[}{\code{signature(x = "clock")}: ... }
    \item{[<-}{\code{signature(x = "clock")}: ... }
    \item{as.numeric}{\code{signature(x = "clock")}: ... }
    \item{coerce}{\code{signature(from = "integer", to = "clock")}: ... }
    \item{coerce}{\code{signature(from = "clock", to = "numeric")}: ... }
    \item{coerce}{\code{signature(from = "numeric", to = "clock")}: ... }
    \item{coerce<-}{\code{signature(from = "clock", to = "numeric")}: ... }
    \item{Compare}{\code{signature(e1 = "ANY", e2 = "clock")}: ... }
    \item{Compare}{\code{signature(e1 = "clock", e2 = "ANY")}: ... }
    \item{Compare}{\code{signature(e1 = "clock", e2 = "clock")}: ... }
    \item{initialize}{\code{signature(.Object = "clock")}: ... }
    \item{Logic}{\code{signature(e1 = "ANY", e2 = "clock")}: ... }
    \item{Logic}{\code{signature(e1 = "clock", e2 = "ANY")}: ... }
    \item{Logic}{\code{signature(e1 = "clock", e2 = "clock")}: ... }
    \item{Math}{\code{signature(x = "clock")}: ... }
    \item{show}{\code{signature(object = "clock")}: ... }
    \item{Summary}{\code{signature(x = "clock")}: ... }
	 }
}
\references{
Wikipedia page
}
\author{Robin K. S. Hankin}
\note{
Notes here
}
\examples{
showClass("clock")
}
\keyword{classes}
\usage{
clock(...)
}
\arguments{
  \item{\dots}{
}
}