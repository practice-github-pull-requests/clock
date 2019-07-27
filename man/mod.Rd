\name{mod-class}
\Rdversion{1.1}
\docType{class}
\alias{mod-class}
\alias{mod}
\alias{.cPair,ANY,mod-method}
\alias{.cPair,mod,ANY-method}
\alias{.cPair,mod,mod-method}
\alias{Arith,ANY,mod-method}
\alias{Arith,mod,ANY-method}
\alias{Arith,mod,missing-method}
\alias{Arith,mod,mod-method}
\alias{as.numeric,mod-method}
\alias{coerce,integer,mod-method}
\alias{coerce,mod,numeric-method}
\alias{coerce,numeric,mod-method}
\alias{coerce<-,mod,numeric-method}
\alias{Compare,ANY,mod-method}
\alias{Compare,mod,ANY-method}
\alias{Compare,mod,mod-method}
\alias{initialize,mod-method}
\alias{Logic,ANY,mod-method}
\alias{Logic,mod,ANY-method}
\alias{Logic,mod,mod-method}
\alias{Math,mod-method}
\alias{show,mod-method}
\alias{Summary,mod-method}

\title{Class \code{"mod"}}
\description{
Mod classes
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{mod(...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"integer"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{integer}"}, from data part.
Class \code{"\linkS4class{numeric}"}, directly, with explicit coerce.
Class \code{"\linkS4class{double}"}, by class "integer", distance 2, with explicit coerce.
Class \code{"\linkS4class{vector}"}, by class "integer", distance 2.
Class \code{"\linkS4class{data.frameRowLabels}"}, by class "integer", distance 2.
}
\section{Methods}{
  \describe{
    \item{.cPair}{\code{signature(x = "ANY", y = "mod")}: ... }
    \item{.cPair}{\code{signature(x = "mod", y = "ANY")}: ... }
    \item{.cPair}{\code{signature(x = "mod", y = "mod")}: ... }
    \item{Arith}{\code{signature(e1 = "ANY", e2 = "mod")}: ... }
    \item{Arith}{\code{signature(e1 = "mod", e2 = "ANY")}: ... }
    \item{Arith}{\code{signature(e1 = "mod", e2 = "missing")}: ... }
    \item{Arith}{\code{signature(e1 = "mod", e2 = "mod")}: ... }
    \item{as.numeric}{\code{signature(x = "mod")}: ... }
    \item{coerce}{\code{signature(from = "integer", to = "mod")}: ... }
    \item{coerce}{\code{signature(from = "mod", to = "numeric")}: ... }
    \item{coerce}{\code{signature(from = "numeric", to = "mod")}: ... }
    \item{coerce<-}{\code{signature(from = "mod", to = "numeric")}: ... }
    \item{Compare}{\code{signature(e1 = "ANY", e2 = "mod")}: ... }
    \item{Compare}{\code{signature(e1 = "mod", e2 = "ANY")}: ... }
    \item{Compare}{\code{signature(e1 = "mod", e2 = "mod")}: ... }
    \item{initialize}{\code{signature(.Object = "mod")}: ... }
    \item{Logic}{\code{signature(e1 = "ANY", e2 = "mod")}: ... }
    \item{Logic}{\code{signature(e1 = "mod", e2 = "ANY")}: ... }
    \item{Logic}{\code{signature(e1 = "mod", e2 = "mod")}: ... }
    \item{Math}{\code{signature(x = "mod")}: ... }
    \item{show}{\code{signature(object = "mod")}: ... }
    \item{Summary}{\code{signature(x = "mod")}: ... }
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
showClass("mod")
}
\keyword{classes}
\usage{
mod(...)
}
\arguments{
  \item{\dots}{
}
}