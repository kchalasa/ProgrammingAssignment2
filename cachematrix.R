## My R Programming Assignment 2
## Function makeCacheMatrix 
## This function creates a customized list that contains four functions
## Function 1 - "set" - Assigns matrix initial values and resets the CACHE
## Function 2- "get" - Returns the matrix
## Function 3- "setInverse" - Sets the matrix Inverse
## Function 4- "getInverse" - Return the matrix Inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     list(set = set, get = get,
	 setInverse = setInverse,
 	 getInverse = getInverse)
}

## Function cacheSolve 
## This function takes our customized list (makeCacheMatrix) as input parameter
## Uses functions in this customized list to return Inverse of a Matrix
## It uses "solve" R-function to take a square matrix and return its Inverse.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
         message("getting cached data")
         return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m  ## Return a matrix that is the inverse of 'x'
}
