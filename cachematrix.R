## This function creates a cache a matrix and it's invese. It's intention is to
## avoid calculating several times the inverse of a given matrix. It is implemented
## as a vector with 4 named columns that are pointers to function.
##
## set - sets the original matrix
## get - gets the original matrix
## setinverse - sets the inverted matrix
## getinverse - gets the inverted matrix
##
## Notice that this doesn't compute the inverted matrix. To compute, see the cacheSolve() function.
##
## Usage:
## m <- matrix(1:4, ncol=2)
## cm <- makeCacheMatrix(m)
##
## cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Keeps the cache of a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
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
    
    x <<- y
    im <<- NULL
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
