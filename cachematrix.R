## This is the function that caches matrices, as 
## required in Programming Assignment 2 for R

## First part of Programming Assignment 2
## creating a list containing a function to set
## and get the value of the matrix, as well as 
## set and get the value of the inverse of the 
## matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function () x
  setinverse <- function(ginv) m <<- ginv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Second part of Programming Assignment 2
## Calculating the inverse of the matrices 
## created with part 1, but first checks if
## matrix inverse has been calculated before
## If it has been calculated, it retrieves the
## result from the cache and skips computing; 
## otherwise, it calculates the inverse, and 
## then stores it in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinverse(m)
  m
}
