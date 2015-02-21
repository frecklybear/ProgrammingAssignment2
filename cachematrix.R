## Description: Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.
## Rationale: These functions are useful as matrix inversion is usually a costly
## computation and we can benefit from caching the inverse rather than
## computing it repeatedly. 
## ======================================================


## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initialise m var
  set <- function(y) { # function to set matrix
    x <<- y  # sets x in the whole function to be the value of y passed in
    m <<- NULL  # reset inverse to NULL when matrix changes
  }
  get <- function() x # function to get matrix from parent environment
  setinverse <- function(inverse) m <<- inverse # function to set inverse of matrix called by cacheSolve that assigns m for whole function
  getinverse <- function() m # function to get inverse of matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. It will retrieve the inverse from the cache
## if it has already been calculated (and the matrix has not changed).


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  ## Used cached data if inverse already calculated for that matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  # Return cached data
  }
  
  ## Calculate and return inverse of matrix
  data <- x$get() #Stores the matrix in a var
  m <- solve(data, ...) # Calculates the inverse of matrix
  x$setinverse(m) # Calls the setinverse() function in makeCacheMatrix with m passed as inverse
  m # Return inverse of matrix
}
