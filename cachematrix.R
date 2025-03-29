## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# It returns a list of functions to:
# 1 - Set the matrix
# 2 - Get the matrix
# 3 - Set the inverse of the matrix
# 4 - Get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setInv(inv)
  inv
}

# Test
test_matrix <- matrix(runif(100, 1, 100), 10, 10)
test_matrix

cache_list <- makeCacheMatrix(test_matrix)
inverse_matrix <- cacheSolve(cache_list)
inverse_matrix

identity_matrix <- test_matrix %*% inverse_matrix
identity_matrix

all.equal(test_matrix %*% identity_matrix, test_matrix)

