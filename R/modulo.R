`mod` <- setClass("mod", contains = "integer")

setValidity("mod", 
            function(object){
              x <- as.numeric(object)
              x <- x[!is.na(x)]  # NA or NaN entries are fine
              if(is.na(modulus())){
                return("working modulus not defined.  Type something like 'modulus(7)' to work modulo 7 to get started")
              } else if(any(x != round(x))){
                return("non integer")
              } else if(any(x<0)){
                return("negative elements")
              } else if(any(x >= getOption("M"))){
                return("elements should be strictly < Modulus")
              } else {
                return(TRUE)
              }
            }
            )

setMethod("initialize", "mod", 
          function(.Object, ...) {
              .Object <- callNextMethod()
              .Object
          }
          )

setAs(from="mod"    ,to="numeric", function(from){  return(from@.Data)})  # No occurences of '@' below here.
setAs(from="numeric",to="mod",def=function(from){mod(as.integer(round(from)) %% modulus())})
setAs(from="integer",to="mod",def=function(from){mod(from %% modulus())})

setMethod("as.numeric",signature(x="mod"),function(x){as(x,"integer")})

"is.mod" <- function(x){is(x,"mod")}
"as.mod" <- function(x){ return(as(x,"mod"))}

".mod.print" <- function(x){ as.numeric(x) }
    
"print.mod" <- function(x, ...){
  jj <- .mod.print(x, ...)
  print(jj)
  cat(paste("members of Z/",modulus(),"Z\n",sep=""))
  return(invisible(jj))
}

setMethod("show", "mod", function(object){print.mod(object)})

setGeneric(".cPair", function(x,y){standardGeneric(".cPair")})
setMethod(".cPair", c("mod", "mod"), function(x,y){.mod.cPair(x,y)})
setMethod(".cPair", c("mod", "ANY"), function(x,y){.mod.cPair(x,as.mod(y))})
setMethod(".cPair", c("ANY", "mod"), function(x,y){.mod.cPair(as.mod(x),y)})
setMethod(".cPair", c("ANY", "ANY"), function(x,y){c(x,y)})

"cmod" <- function(x, ...) {
   if(nargs()<3)
      .cPair(x,...)
    else
      .cPair(x, Recall(...))
}

".mod.cPair" <- function(x,y){
  x <- as.mod(x)
  y <- as.mod(y)
  mod(c(as.numeric(x),as.numeric(y)))
}

setMethod("sqrt",",mod", function(x){
  stop("not yet implemented; need Legendre symbols")
} )
          
setMethod("Math", "mod",
          function(x){stop(paste(.Generic, "not allowed on mod objects"))}
          )

".mod.negative" <- function(e1){ as.mod(-as.numeric(e1)) }
"mod.inverse" <- function(e1){
    stopifnot(is.mod(e1))
    stopifnot(numbers::isPrime(modulus()))
    e1 <- as.numeric(e1)
    e1[e1==0] <- NA
    as.mod(e1^(modulus()-2))
}
".mod.add" <- function(e1,e2){ as.mod(as.numeric(e1) + as.numeric(e2)) }
".mod.mult" <- function(e1,e2){ as.mod(as.numeric(e1)*as.numeric(e2)) }
".mod.power"<- function(e1,e2){
    a <- as.numeric(e1)
    if(is.mod(e2)){stop(paste("a^p not defined for p a member of Z/",mod(),sep=""))}
    p <- e2    # notionally calculating a^p
    # notionally calculating a^p

    jj <- cbind(seq_along(a),seq_along(p))  # handles vectorization
    a <- a[jj[,1]]
    p <- p[jj[,2]]
    
    phi <- getOption("phi")
    negs <- which(p<0)
    if(any(negs)){
        p <- abs(p)
        a[negs] <- as.numeric(mod.inverse(as.mod(a[negs])))
    }
    return(as.mod(a^(p%%phi)))
}

".mod.quotient" <- function(e1,e2){
    stopifnot(is.mod(e2))
    as.mod(as.numeric(e1)*as.numeric(mod.inverse(e2)))
}

setMethod("Arith",signature(e1 = "mod", e2="missing"),
          function(e1,e2){
            switch(.Generic,
                   "+" = e1,
                   "-" = .mod.negative(e1),
                   stop(paste("Unary operator", .Generic,
                              "not allowed on mod objects"))
                   )
          } )

".mod.arith" <- function(e1,e2){
  switch(.Generic,
         "+" = .mod.add  (e1, e2),
         "-" = .mod.add  (e1, .mod.negative(as.mod(e2))),
         "*" = .mod.mult (e1, e2),
         "/" = .mod.quotient(e1,e2),
         "^" = .mod.power(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for mod objects"))
         ) }

setMethod("Arith", signature(e1 = "mod", e2="ANY"), .mod.arith)
setMethod("Arith", signature(e1 = "ANY", e2="mod"), .mod.arith)
setMethod("Arith", signature(e1 = "mod", e2="mod"), .mod.arith)


".mod.equal" <- function(e1,e2){ as.numeric(e1) == as.numeric(e2) }

".mod.greater" <- function(e1,e2){
  stop(paste("Z/",modulus(),"Z is not an ordered set.\n",sep=""))
}

".mod.compare" <- function(e1,e2){

  e1 <- as.mod(e1)
  e2 <- as.mod(e2)
  switch(.Generic,
         "==" =  .mod.equal(e1,e2),
         "!=" = !.mod.equal(e1,e2),
         ">"  =  .mod.greater(e1,e2),
         "<"  = !.mod.greater(e1,e2) & !.mod.equal(e1,e2),
         ">=" =  .mod.greater(e1,e2) |  .mod.equal(e1,e2),
         "<=" = !.mod.greater(e1,e2) |  .mod.equal(e1,e2),
         stop(paste(.Generic, "not supported for mod objects"))
         )
}

setMethod("Compare", signature(e1="mod", e2="ANY"), .mod.compare)
setMethod("Compare", signature(e1="ANY", e2="mod"), .mod.compare)
setMethod("Compare", signature(e1="mod", e2="mod"), .mod.compare)

".mod.logic" <- function(e1,e2){
  stop("No logic currently implemented for mod objects")
}

## The following lines deal with idiom like mod(1:10) & mod(10:1) [which are meaningless]
setMethod("Logic",signature(e1="mod",e2="ANY"), .mod.logic)
setMethod("Logic",signature(e1="ANY",e2="mod"), .mod.logic)
setMethod("Logic",signature(e1="mod",e2="mod"), .mod.logic)

if(!isGeneric("prod")){   # prod(1:10) returns factorial(10); cf Wilson's theorem
setGeneric("prod", function(x, ..., na.rm = FALSE)
	{
		standardGeneric("prod")
	},
	useAsDefault = function(x, ..., na.rm = FALSE)
	{
		base::prod(x, ..., na.rm = na.rm)
	},
	group = "Summary")
}

if(!isGeneric("sum")){
setGeneric("sum", function(x, ..., na.rm = FALSE)
	{
		standardGeneric("sum")
	},
	useAsDefault = function(x, ..., na.rm = FALSE)
	{
		base::sum(x, ..., na.rm = na.rm)
	},
	group = "Summary")
}

".mod.prod" <- function(x){  
    stop("not yet implemented")
}

".mod.sum" <- function(x){ mod(as.numeric(x)) }

setMethod("Summary", "mod",
          function(x, ..., na.rm=FALSE){
            switch(.Generic,
                   prod   =  .mod.prod(x),
                   sum    =  .mod.sum(x),
                   stop(paste(.Generic, "not implemented on mod objects"))
                   )
          }
          )


