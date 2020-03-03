## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function that creates a square matrix that can 
## cache its inverse output

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_solve <- function(solve) inv <<- solve
  get_solve <- function() inv
  list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
}


## Write a short comment describing this function
## cacheSolve solves the matrix (computes the inverse) supplied by the makeCacheMatrix. 
## If makeCacheMatrix hasn't changed the function just returns the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_solve()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_solve(inv)
  inv
}

#The result:
m <- matrix(sample(1:100, 9), nrow = 3, ncol = 3)
res <- makeCacheMatrix(m)
cacheSolve(res)