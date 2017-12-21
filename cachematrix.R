## This script was updated as part of the second programming assignment
## towards week 3 of the r programming coursera coursera course.

## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly. The functions in this script create a
## special object that can cache the matrix and its inverse.

## The makeCaheMatrix function creates a list of functions which 
## can be used to set/get the value of the matrix and set/get the
## inverse of the matrix, leaving the values in cache.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## set the get/set functions
  set <- function(y) {
    x <<- y ## set the matrix value
    inv <<- NULL ## clear the cache for inverse
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## Save these in 'special' list as per cachemean example
  list(set=set, get=get, setInverse=setinverse, getInverse=getinverse)

}


## The cacheSolve function either returns or calculates the inverse of the
## matrix. It only calculates the inverse if it is not alreadyy in cache.
## The function assumes the matrix supplied is invertible (returns standard
## singular error from the solve function if not).

cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  
  ## Checks to see if the inverse is already saved in cache
  if(!is.null(inv)){
    message("Fetching cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
  
}

## This script was tested using the following:

## > x<-rbind(c(-1,1.5),c(1,-1))
##> x
##[,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0
##> test<-makeCacheMatrix(x)
##> test$get()
##[,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0
##> test$getInverse()
##NULL
##> cacheSolve(test)
##[,1] [,2]
##[1,]    2    3
##[2,]    2    2
##> cacheSolve(test)
##Fetching cached data
##[,1] [,2]
##[1,]    2    3
##[2,]    2    2
