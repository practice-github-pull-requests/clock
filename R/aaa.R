`modulo` <- function(n){   # workflow modelled on sol() in the lorentz() package
  if(missing(n)){  # return modulus
    return(getOption("M"))
  } else {  # working modulus supplied

    stopifnot(is.numeric(n))
    stopifnot(length(n)==1)
    stopifnot(n==round(n))
    stopifnot(n>1)

    options("M" = n)
    options("prompt" = paste("Modulo(", n, ") > ",sep=""))
  }
}

