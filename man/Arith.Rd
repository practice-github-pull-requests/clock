\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{clock.inverse}
\alias{Arith,ANY,clock-method}
\alias{Arith,clock,ANY-method}
\alias{Arith,clock,missing-method}
\alias{Arith,clock,clock-method}
\alias{as.numeric,clock-method}
\title{Methods for Function Arith in package clock}
\description{
Methods for Arithmetic functions in the clock package: \code{+},
\code{-}, \code{*}, \code{/}, \code{^}.
}

\details{
  The unary arithmetic functions (viz \dQuote{\code{+}} and
  \dQuote{\code{-}}) do no coercion. 
  
  The binary arithmetic functions coerce \code{numeric <op> clock} and
  \code{clock <op> numeric} to \code{clock}, except for \code{^}, for
  which only \code{clock^numeric} is defined.

  Multiplicative inverses are calculated using Fermat's theorem: if
  \eqn{p} is prime and \eqn{(a,p)=1} then \eqn{a^{p-1}=1\, (\mbox{mod}\,
  p)}{a^(p-1)=1 (mod p)}.  From this we deduce that \eqn{a\cdot
  a^{p-2}=1\, (\mbox{mod}\, p)}{a.a^(p-2)=1 (mod p)}, or
  \eqn{a^{-1}=a^{p-2}\, (\mbox{mod}\, p)}{a^-1=a^(p-2) (mod p)}.  It
  might be slightly more efficient to use a look-up table but the
  bookkeeping overhead for dealing with zero and \code{NA} would be
  large.  Multiplicative inverses are not defined for composite moduli.

  Powers are calculated using Euler's generalization of Fermat: if
  \eqn{(a,n)=1} then \eqn{a^{\phi(n)}=1\, (\mbox{mod}\, n)}{a^phi(n)=1
  (mod n)}.  So if \eqn{n=p\cdot\phi(n)+q}{m=p.phi(n)+q} we would have
  \eqn{a^n=a^q\,(\mbox{mod}\,n)}{a^n=a^q (mod n)}, and this is used in
  the package.  It might be possible to use the multiplicative order of
  \eqn{a} for composite \eqn{n} but again overheads would be
  considerable.
}
\author{Robin K. S. Hankin}
\examples{
modulus(11)
a <- as.mod(sample(11))
b <- as.mod(sample(11))
a+b
a-b

a*b
a/b   # NB division by zero gives NA
a^2   # NB 2^a and a^a not defined

## Negative powers work as expected:
stopifnot(all(a^-1 == 1/a,na.rm=TRUE))

}
\keyword{methods}
\keyword{math}
