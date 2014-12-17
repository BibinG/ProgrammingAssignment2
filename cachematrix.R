## These functions will help calculate the inverse of a matrix
## If the inverse of the same matrix is invoked twice, the output is printed
## from the cache the second time thus avoiding recalculation

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {         ## set the value of the matrix into x
    x <<- y              
    inv <<- NULL               ## and set inv as NULL initially
  }
  get <- function() x                        ## return the value of x
  setinv <- function(inve) inv <<- inve      ## set the inverse to inv
  getinv <- function() inv                   ## returns inv
  list(set = set, get = get,                 ## returns a list
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv1 <- x$getinv()               ## get inverse from the above function
  if(!is.null(inv1)) {             ## if calculated already, then proceed
    message("getting cached data") 
    return(inv1)
  }
  data <- x$get()                  
  inv1 <- solve(data)              ## if not calculated, then solve the inverse
  x$setinv(inv1)
  inv1                             ## return inverse
}
