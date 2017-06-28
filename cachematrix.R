## makeCacheMatrix creates a special matrix object that can cache it's inverse.
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse is already calculated and the matix has not been changed, 
## cacheSolve retrives the inverse from cache.

## makeCacheMatrix creates a special matrix object that can cache it's
## inverse. it returns a list containing functions to set the value of the
## matrix, get the value of the matrix, set the inverse of the matrix and get
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(data) { 
    x <<- data
    inv <<- NULL
  }
  
  getData <- function() {x}
  
  setInv <- function(inverse) {inv <<- inverse}
  getInv <- function() {inv}
  
  list(set = set, getData = getData,
       setInv = setInv, getInv = getInv)

}


## cacheSolve calculates the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse is already computed and the matrix has not
## been changed, cacheSolve just retrives the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return (inv)
  }
  
  mat_data <- x$getData()
  inv <- solve(mat_data)
  x$setInv(inv)
  inv
}
