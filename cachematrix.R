## The makeCacheMatrix function creates a Matrix that caches its inverse. 
## It starts by creating the matrix with the existing matrix() function, and then sets up the process for creating the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
      x <<- y
    inv <<- NULL
  }
  get = function() x
  set_inv = function(inverse) inv <<- inverse 
  get_inv = function() inv
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## The cacheSolve function computes the inverse of the matrix from makeCacheMatrix, checking first if the inverse has already been cached, thus saving time.

cacheSolve <- function(x, ...) {
   inv = x$get_inv()
    if (!is.null(inv)){
    return(inv)
  }
   mat.data = x$get()
  inv = solve(mat.data, ...)
  x$set_inv(inv)
  return(inv)
}
