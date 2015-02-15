## Coursera: Data Science Specialization
## Intro to R Programming
## Programming Assignment 2
##
## Submitted by: coffeeandchocolate
##
## makeCacheMatrix and cacheSolve are a pair of functions that can cache
## the inverse of a matrix. These functions that the matrix supplied to
## the functions is always invertible.

## makeCacheMatrix
##
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # create variable inv to hold the inverted matrix and initialize to NULL
  inv <- NULL
  
  # define set function
  # set the matrix x to a new matrix y and initialize its inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # define get function
  # return the matrix x
  get <- function() x
  
  # define setinverse function
  # set the inv variable to the inverse of the matrix x, which is 
  # computed using the solve() function
  setinverse <- function(inv) inv <<- solve(x)
  
  # define getinverse function
  # return the inverse of x
  getinverse <- function() inv
  
  # returns special vector of functions defined by makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## cacheSolve
##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  # call getinverse() on the matrix x
  inv <- x$getinverse()
  
  # if there is already a cached value for the inverse of x, return that
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, get the matrix x and store it in variable data
  data <- x$get()
  
  # solve for the inverse and store in variable inv
  inv <- solve(data, ...)
  
  # cache the inverse using setinverse
  x$setinverse(inv)
  
  # and then return the inverse
  inv
  
}
