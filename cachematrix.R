##  A pair of functions that cache the inverse of a matrix.

## function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y){
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) M <<- Inverse
  getinverse <- function() M
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## function computes the inverse returned by makeCacheMatrix 

cacheSolve <- function(X, ...) {
  M <- X$getinverse()
  if(!is.null(M)) {
    message("getting cache data")
    return(M)
  }
  data <- X$get()
  M <- solve(data)
  X$setinverse(M)
  M
}
