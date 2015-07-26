# DEV: Clear the environment first, comment out for production!
# rm(list = ls())

#################################################################################
# These functions are used to create and cache the inverse of a given matrix
# by using a list to store the matrix and its cached values. The goal is
# to avoid recomputing a matrix if it's already cached. Basic input validation
# has been implemented, though it isn't bulletproof. Patches accepted!
#
# Example usage:
# 
# > x = makeCacheMatrix(matrix(runif(9),3))
# > cacheSolve(x)
# [,1]       [,2]       [,3]
# [1,]  0.8487011  2.1067086 -2.9200341
# [2,] -1.2998476 -0.6424739  3.1318424
# [3,]  1.8243850 -2.0574523  0.1386315
# > cacheSolve(x)
# getting cached data
# [,1]       [,2]       [,3]
# [1,]  0.8487011  2.1067086 -2.9200341
# [2,] -1.2998476 -0.6424739  3.1318424
# [3,]  1.8243850 -2.0574523  0.1386315
#################################################################################

# Returns a list containing a matrix, which has the capability
# of caching its inverse when run through cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x))          { stop("x isn't a matrix") }
  if(dim(x)[1] != dim(x)[2]) { stop("x isn't a square matrix, cannot solve") }
  
  m = NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}

# Solve a list-ified matrix as created with makeCacheMatrix(), and
# return the solution. If it's already solved with cacheSolve,
# return the cached solution (and inform the user of cache use).
cacheSolve <- function(x, ...) {
  if(!is.list(x))                   { stop("x wasn't created with makeCacheMatrix") }
  if(!('get' %in% names(x)))        { stop("x is missing a 'get' function") }
  if(!(class(x$get()) == 'matrix')) { stop("x$get() isn't a matrix") }
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
