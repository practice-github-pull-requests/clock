`clock` <- setClass("clock", contains = "integer")
setAs(from="clock"  ,to="numeric", function(from){  return(from@.Data)})  # There are no occurences of '@' below here.

setValidity("clock",   #S4 setmethods used here
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

setMethod("initialize", "clock", 
          function(.Object, ...) {
              .Object <- callNextMethod()
              .Object
          }
          )

setAs(from="numeric",to="clock",def=function(from){clock(as.integer(round(from)) %% modulus())})
setAs(from="integer",to="clock",def=function(from){clock(from %% modulus())})

setMethod("as.numeric",signature(x="clock"),function(x){as(x,"integer")})

`is.mod` <- function(x){is(x,"clock")}
`mod`    <- function(x){ return(as(x,"clock"))}
`as.mod`    <- function(x){ return(as(x,"clock"))}

".clock.print" <- function(x){ as.numeric(x) }
    
"print.clock" <- function(x, ...){
  jj <- .clock.print(x, ...)
  print(jj)
  cat(paste("members of Z/",modulus(),"Z\n",sep=""))
  return(invisible(jj))
}

setMethod("show", "clock", function(object){print.clock(object)})

setGeneric(".cPair", function(x,y){standardGeneric(".cPair")})
setMethod(".cPair", c("clock", "clock"), function(x,y){.clock.cPair(x,y)})
setMethod(".cPair", c("clock", "ANY"), function(x,y){.clock.cPair(x,as.mod(y))})
setMethod(".cPair", c("ANY", "clock"), function(x,y){.clock.cPair(as.mod(x),y)})
setMethod(".cPair", c("ANY", "ANY"), function(x,y){c(x,y)})

"cclock" <- function(x, ...) {
   if(nargs()<3)
      .cPair(x,...)
    else
      .cPair(x, Recall(...))
}

".clock.cPair" <- function(x,y){
  x <- as.mod(x)
  y <- as.mod(y)
  mod(c(as.numeric(x),as.numeric(y)))
}

setMethod("Math", "clock",
          function(x){stop(paste(.Generic, "not allowed on clock objects"))}
          )

".clock.negative" <- function(e1){ as.mod(-as.numeric(e1)) }
"clock.inverse" <- function(e1){
    stopifnot(is.mod(e1))
    stopifnot(numbers::isPrime(modulus()))
    e1 <- as.numeric(e1)
    e1[e1==0] <- NA
    as.mod(e1^(modulus()-2))
}
".clock.add" <- function(e1,e2){ as.mod(as.numeric(e1) + as.numeric(e2)) }
".clock.mult" <- function(e1,e2){ as.mod(as.numeric(e1)*as.numeric(e2)) }
".clock.power"<- function(e1,e2){
    a <- as.numeric(e1)
    if(is.mod(e2)){stop(paste("a^p not defined for p a member of Z/",modulus(),sep=""))}
    p <- e2    # notionally calculating a^p
    # notionally calculating a^p

    jj <- cbind(seq_along(a),seq_along(p))  # handles vectorization
    a <- a[jj[,1]]
    p <- p[jj[,2]]
    
    phi <- getOption("phi")
    negs <- which(p<0)
    if(any(negs)){
        p <- abs(p)
        a[negs] <- as.numeric(clock.inverse(as.mod(a[negs])))
    }
    return(as.mod(a^(p%%phi)))
}

".clock.quotient" <- function(e1,e2){
    stopifnot(is.mod(e2))
    as.mod(as.numeric(e1)*as.numeric(clock.inverse(e2)))
}

setMethod("Arith",signature(e1 = "clock", e2="missing"),
          function(e1,e2){
            switch(.Generic,
                   "+" = e1,
                   "-" = .clock.negative(e1),
                   stop(paste("Unary operator", .Generic,
                              "not allowed on clock objects"))
                   )
          } )

".clock.arith" <- function(e1,e2){
  switch(.Generic,
         "+" = .clock.add  (e1, e2),
         "-" = .clock.add  (e1, .clock.negative(as.mod(e2))),
         "*" = .clock.mult (e1, e2),
         "/" = .clock.quotient(e1,e2),
         "^" = .clock.power(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for clock objects"))
         ) }

setMethod("Arith", signature(e1 = "clock", e2="ANY"), .clock.arith)
setMethod("Arith", signature(e1 = "ANY", e2="clock"), .clock.arith)
setMethod("Arith", signature(e1 = "clock", e2="clock"), .clock.arith)


".clock.equal" <- function(e1,e2){ as.numeric(e1) == as.numeric(e2) }

".clock.greater" <- function(e1,e2){
  stop(paste("Z/",modulus(),"Z is not an ordered set.\n",sep=""))
}

".clock.compare" <- function(e1,e2){

  e1 <- as.mod(e1)
  e2 <- as.mod(e2)
  switch(.Generic,
         "==" =  .clock.equal(e1,e2),
         "!=" = !.clock.equal(e1,e2),
         ">"  =  .clock.greater(e1,e2),
         "<"  = !.clock.greater(e1,e2) & !.clock.equal(e1,e2),
         ">=" =  .clock.greater(e1,e2) |  .clock.equal(e1,e2),
         "<=" = !.clock.greater(e1,e2) |  .clock.equal(e1,e2),
         stop(paste(.Generic, "not supported for clock objects"))
         )
}

setMethod("Compare", signature(e1="clock", e2="ANY"), .clock.compare)
setMethod("Compare", signature(e1="ANY", e2="clock"), .clock.compare)
setMethod("Compare", signature(e1="clock", e2="clock"), .clock.compare)

".clock.logic" <- function(e1,e2){
  stop("No logic currently implemented for clock objects")
}

## The following lines deal with idiom like mod(1:10) & mod(10:1) [which are meaningless]
setMethod("Logic",signature(e1="clock",e2="ANY"), .clock.logic)
setMethod("Logic",signature(e1="ANY",e2="clock"), .clock.logic)
setMethod("Logic",signature(e1="clock",e2="clock"), .clock.logic)

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

".clock.prod" <- function(x){  
    stop("not yet implemented")
}

".clock.sum" <- function(x){ mod(as.numeric(x)) }

setMethod("Summary", "clock",
          function(x, ..., na.rm=FALSE){
            switch(.Generic,
                   prod   =  .clock.prod(x),
                   sum    =  .clock.sum(x),
                   stop(paste(.Generic, "not implemented on clock objects"))
                   )
          }
          )

setMethod("[", "clock",
          function(x, i, j,  drop){
            if(!missing(j)){
              warning("second argument to extractor function ignored")
            }
            as.mod(as.numeric(x)[i])
          } )

setReplaceMethod("[",signature(x="clock"),
                 function(x,i,j,value){
                     out <- as.numeric(x)
                     out[i] <- value
                     return(as.mod(out))
                 } )
