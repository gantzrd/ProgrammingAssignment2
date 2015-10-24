## This functions are create a matrix with the property of storing in
## cache the solution for its inverse


## This function creates a matrix with the properties get and set its
## values as well as setting and getting its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This funciton returns the inverse of a matrix. It first searches 
## the cache of the matrix for the inverse value and returns it, if 
## the inverse is not found the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
