`modulus` <- function(n){ # Workflow modelled on sol() in the lorentz() package
    if(missing(n)){ # An empty argument returns the working modulus, just like in the lorentz package sol() returns speed of light
        return(getOption("M"))
    } else {  # working modulus supplied
        
        stopifnot(is.numeric(n))
        stopifnot(length(n)==1)
        stopifnot(n==round(n))
        stopifnot(n>1)
        
        options("M" = n)
        options("phi" = numbers::eulersPhi(n))
        options("prompt" = paste("Modulo(", n, ") > ",sep=""))
    }
}

