## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that catches it's inverse.
##Defines the getter and setter functions for both the Matrix and 
##the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the catched inverse if it exists otherwise computes it
## then catches the result the returnes the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matData <- x$get()
  
  inv <- solve(matData)
  
  x$setinverse(inv)
  
  inv
}