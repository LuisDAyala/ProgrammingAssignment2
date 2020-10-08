## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
        ## Return a matrix that is the inverse of 'x'
}
