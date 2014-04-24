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
                  x <<- y           ## Store Original Matrix
                  m <<- NULL        ## Clear Matrix Inverse
                 }

     get <- function() x            ## Return Original Matrix

     setInverse <- function(solve) { 
     		      m <<- solve   ## Store Matrix Inverse
	           }
     getInverse <- function() m     ## Return Matrix Inverse

     list(set = set, get = get,     ## Return list of all the above functions
	 setInverse = setInverse,
 	 getInverse = getInverse)
}

## Function cacheSolve 
## This function takes our customized list (makeCacheMatrix) as input parameter
## Uses functions in this customized list to return Inverse of a Matrix
## It uses "solve" R-function to take a square matrix and return its Inverse.

cacheSolve <- function(x, ...) {
     m <- x$getInverse()            ## Func. to get matrix Inverse from Cache.
     if(!is.null(m)) {
          message("getting cached data")
          return(m)                 ## Found in CACHE so return matrix inverse
     }
     data <- x$get()                ## Invoke function to get orig. matrix 'x'
     m <- solve(data, ...)          ## R function to compute matrix inverse
     x$setInverse(m)                ## Invoke function for matrix inverse
     m                              ## Return matrix that is the inverse of 'x'
}
