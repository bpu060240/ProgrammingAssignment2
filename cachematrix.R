
## This function creates a special "matrix" object that can cache its inverse

## @x: a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  #set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get the matrix
  get <- function() x
  
  #set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  #get the inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  ## The solve() function takes a matrix as input and returns the matrix’s inverse as output. 
  inv <- solve(data, ...)
  x$setinv(inv)
  
  ## Return the inverse of the original matrix input to makeCacheMatrix()
  inv
  
}
