## Put comments here that give an overall description of what your
## functions do

## The following function returns a list of functions that perform the basic operations
## for the chacheable matrix: set & get matrix, set & get inverse of matrix, avoiding
## recalculating if the matrix has not changed and its inverse has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize retrievable matrix to NULL, indicating the inverse has not been calculated yet
  m <- NULL
  ## set matrix passed as parameter of this function & reinitialize value of m in parent
  ## environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## return current matrix from parent environment
  get <- function() x
  ## Receive the inverse of the matrix as parameter and assign to m in the parent environment
  ## To be used only in the function cacheSolve, otherwise generates wrong results.
  setinverse <- function(inv) m <<- inv
  ## return the inverse of the matrix in the parent environment
  getinverse <- function() m
  ## return a list of closure functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to solve the inverse of matrix x, if it was not already calculated 

cacheSolve <- function(x, ...) {
  ## Assign the value of the inverse from the function defining the cacheable matrix
  mx <- x$getinverse()
  ## Check if the inverse of the matrix if already calculated and return it from the cache
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  ## If the inverse of x has not been calculated, assign value of x to temp variable data ...
  data <- x$get()
  ## calculate the inverse of matrix using function solve() ...
  mx <- solve(data, ...)
  ## and set the value of the inverse
  x$setinverse(mx)
  ## retrieves the calculated inverse of matrix x
  mx
}
