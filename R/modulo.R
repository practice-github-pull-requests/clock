setClass("mod",
         representation = representation(x="numeric"),
         prototype      = list(x=numeric(),positive=logical()),
         contains       = "swift"
         )

".mod.valid" <- function(object){
  if(any(object@x != round(object@x))){
    return("non integer")
  } else {
    return(TRUE)
  }
}

setValidity("mod", .mod.valid)

"mod" <- function(x=integer()){
  new("mod",x=as.integer(x))
}

"is.mod" <- function(x){is(x,"mod")}


"as.mod" <- function(x){
  if(is.mod(x)){
    return(x)
    } else {
    return(mod(round(x)))
  }
}

setAs("mod", "numeric", function(from){
  return(from@x)
} )

setMethod("as.numeric",signature(x="mod"),function(x){as(x,"integer")})

".mod.print" <- function(x){
  print(x)
  print("modulo DSFADFSFADSASDF")
   }
    
"print.mod" <- function(x, ...){
  jj <- .mod.print(x, ...)
  print(jj)
  return(invisible(jj))
}

setMethod("show", "mod", function(object){print.mod(object)})



setGeneric(".cPair", function(x,y){standardGeneric(".cPair")})
setMethod(".cPair", c("mod", "mod"), function(x,y){.Mod.cPair(x,y)})
setMethod(".cPair", c("mod", "ANY"),  function(x,y){.Mod.cPair(x,as.mod(y))})
setMethod(".cPair", c("ANY", "mod"),  function(x,y){.Mod.cPair(as.mod(x),y)})
setMethod(".cPair", c("ANY", "ANY"),   function(x,y){c(x,y)})

"cmod" <- function(x, ...) {
   if(nargs()<3)
      .cPair(x,...)
    else
      .cPair(x, Recall(...))
}

".mod.cPair" <- function(x,y){
  x <- as.mod(x)
  y <- as.mod(y)
  mod(c(x@x,y@x))
}

setMethod("sqrt",",mod", function(x){
  stop("not yet implemented; need Legendre symbols")
} )
          
setMethod("Math", "mod",
          function(x){
            switch(.Generic,
                   abs    = mod(x@x),
                   stop(paste(.Generic, "not allowed on mod objects"))
                     )
          } )

".mod.negative" <- function(e1){
  stop("not yet implemented")
}


".mod.add" <- function(e1,e2){
  stop("not yet implemented")
}

".mod.mult" <- function(e1,e2){
  stop("not yet implemented")
}

".mod.power"<- function(e1,e2){
  stop("not yet implemented")
}

".mod.inverse" <- function(b){  stop("not yet implemented")}

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
         "/" = .mod.mult (e1, .mod.inverse(as.mod(e2))),
         "^" = .mod.power(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for mod objects"))
         ) }

setMethod("Arith", signature(e1 = "mod", e2="ANY"), .mod.arith)
setMethod("Arith", signature(e1 = "ANY", e2="mod"), .mod.arith)
setMethod("Arith", signature(e1 = "mod", e2="mod"), .mod.arith)


".mod.equal" <- function(e1,e2){
  (e1@x==e2@x)
}

".mod.greater" <- function(e1,e2){
  stop("meaningless")
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

setMethod("Compare", signature(e1="mod", e2="ANY" ), .mod.compare)
setMethod("Compare", signature(e1="ANY" , e2="mod"), .mod.compare)
setMethod("Compare", signature(e1="mod", e2="mod"), .mod.compare)

".mod.logic" <- function(e1,e2){
  stop("No logic currently implemented for mod objects")
}

setMethod("Logic",signature(e1="mod",e2="ANY"), .mod.logic)
setMethod("Logic",signature(e1="ANY",e2="mod"), .mod.logic)
setMethod("Logic",signature(e1="mod",e2="mod"), .mod.logic)

if(!isGeneric("prod")){
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

".mod.sum" <- function(x){
  stop("not yet implemented")
}

setMethod("Summary", "mod",
          function(x, ..., na.rm=FALSE){
            switch(.Generic,
                   prod   =  .mod.prod(x),
                   sum    =  .mod.sum(x),
                   stop(paste(.Generic, "not implemented on mod objects"))
                   )
          }
          )


