## Objective is to create a pair of functions that will compute the inverse of a matrix
## and cacheing it. In the case where the inverse has already been calculated, will retrieve 
## the inverse from the cache instead of re-calculating.

## makeCacheMatrix creates a "special matrix object" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){  # function to set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x   # function to get the value of the matrix
  setinv <- function(inverse) inv <<- inverse   #function to set the inverse 
  getinv <- function() inv                      #function to get the inverse
  list(set = set,                               #list of things to return
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## if the inverse has already been calculated, then cacheSolve will retrieve inverse from cache

cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                
  if(!is.null(inv)){               #if inverse retrieved is not null then simply return it
    message("getting cache inverse")
    return(inv)
  }
  mat <- x$get()                   #otherwise solve for the inverse at set it as the matrix's inverse 
  inv <- solve(mat, ...)           #and return it
  x$setinv(inv)
  inv
}
