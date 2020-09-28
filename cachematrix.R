## These functions creates a special matrix capable of caching its inverse and 
## returns the inverse if it isn't already available. 
## It is assumed that the matrix created is always invertible.


## This function creates the matrix object.
## The solve function was used for the computation.

makeCacheMatrix <- function(x = matrix()) {
        cac <- NULL
        set <- function(y){
                x <<- y
                cac <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) cac <<- solve
        getinverse <- function() cac
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function solves for the inversion of the created matrix.
## The computation is only carried out if it is not already avaialable.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cac <- x$getinverse()
        if(!is.null(cac)){
                message("getting cached data")
                return(cac)
        }
        mydata <- x$get()
        cac <- solve(mydata, ...)
        x$setinverse(cac)
        cac
}
