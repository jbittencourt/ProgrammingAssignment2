## This functions implements a solution that caches a matrix and it's invese. It's intention is to
## avoid calculating several times the inverse of a given matrix, which can be a costly operation for
## large matrices. The cache solution uses 2 functions that must be used together.
##
## makeCacheMatrix(x,...) takes a matrix x as input and returns a vector with 4 named functions:
##
## set - sets the original matrix
## get - gets the original matrix
## setinverse - sets the inverted matrix
## getinverse - gets the inverted matrix
##
## makeCacheMatrix  doesn't compute the inverted matrix, it's only a vector that keeps the cache values.
## To compute, the cacheSolve() function must be used.
##
## cacheSolve(x,...) takes the vector x, returned by makeCacheMatrix, and returns the invese matrix. If a
##                   inverse matrix isn't in cache, the function computers it.
##
## Notice: This solution only works for square matrices.
##
## Usage:
##
## m <- matrix(1:4, ncol=2)
## cm <- makeCacheMatrix(m)
##
## cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##

## Keeps the cache of a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL # keeps the inverse matrix
  
  #test if matrix is a square matrix, otherwise return null
  if(dim(x)[1]!=dim(x)[2]) {
    message("suplied matrix isn't a square matrix")
    return(NULL)
  }
  
  set <- function(y) {
    
    #test if matrix is a square matrix, otherwise return null
    if(dim(y)[1]!=dim(y)[2]) {
      message("suplied matrix isn't a square matrix")
      return(NULL) 
    }
    
    x <<- y      #stores the cache matrix
    im <<- NULL  #since matrix was changed, clean's the inverse.
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the Cached Inverse Matrix of a given Cache Matrix. If the inverse isn't solved, compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()      
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
