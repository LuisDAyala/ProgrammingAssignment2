#The first function, create a new object that store the value of the matrix 
#given as an argument as well as four functions. The second function take an 
#object created by the first function and evaluate if there is a inverse´s
#matrix already calculate and print it if it is the case, if not, calculate the
#inverse´s matrix, print it and saved it inside the object given as an argument.

makeCacheMatrix <- function(x = matrix()) {
        Inv<- NULL
        setMatx <- function(l){
                x<<-l
                Inv<<- NULL
        }
        getMatx<- function()x
        setInv<- function(MatxInv) Inv <<- MatxInv
        getInv <- function() Inv
        
        list(setMatx = setMatx, getMatx=getMatx,
             setInv=setInv, getInv=getInv)
}


cacheSolve <- function(x, ...) {
        Inv<-x$getInv()
        if (!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        data<-x$getMatx()
        Inv<-solve(data, ...)
        x$setInv(Inv)
        Inv
}
