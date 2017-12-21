## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setInverse=setinverse, getInverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
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

# > x<-rbind(c(-1,1.5),c(1,-1))
#> x
#[,1] [,2]
#[1,]   -1  1.5
#[2,]    1 -1.0
#> test<-makeCacheMatrix(x)
#> test$get()
#[,1] [,2]
#[1,]   -1  1.5
#[2,]    1 -1.0
#> test$getInverse()
#NULL
#> cacheSolve(test)
#[,1] [,2]
#[1,]    2    3
#[2,]    2    2
#> cacheSolve(test)
#Fetching cached data
#[,1] [,2]
#[1,]    2    3
#[2,]    2    2
