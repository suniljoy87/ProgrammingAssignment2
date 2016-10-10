## creates a Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x = y
    inv = NULL
  }
  get <- function()
    x
  setInverse = function(inverse)
    inv = inverse
  getInverse = function()
    inv
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
  
  
}


## Function to create an inverse of the matric created above.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
