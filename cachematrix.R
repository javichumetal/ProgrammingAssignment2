## The following functions calculate and cache the inverse of a given matrix. 
## Inverse matrix calculations can be costly and therefore may benefit from a cache scheme.


## There will be two functions to calculate the inverse and cache it.


## This function creates a special "Matrix" object which is a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix<-function(x=matrix()){
  
  ## set the variable where the inverse matrix x will be cached to null
  cinv <- NULL
  
  ## create function to assign variables to parent environment 
  set <- function(y) {
    ## assign matrix to parent environment 
    x <<- y 
    ## assign inverse matrix to parent environment
    cinv <<- NULL
  }
  
  ## create function to return the matrix x
  get <- function() x
  
  ## set the cache cinv equal to the value of the inverse matrix
  setinverse <- function(inverse) cinv<<-inverse
  
  ## return the cached inverse matrix
  getinverse <- function() cinv
  
  ## create the list where the functions will be stored
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.  
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <-   function(x, ...) {
  
  ## return a matrix (inverse of x)
  cinv <- x$getinverse()
  
  ## check if the invese matrix is already cached. if so, return cached inverse matrix
  if(!is.null(cinv)) {
    message("getting cached data")
    return(cinv)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## calculate inverse matrix using solve function
  cinv <- solve(data, ...)
  
  ## set inverse matrix in cache
  x$setinverse(cinv)
  
  ## return inverse matrix
  cinv
}