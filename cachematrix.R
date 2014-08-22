# Author - Jeffrey Drake.

#' Returns a cacheable matrix from a matrix
#' 
#' @param x The matrix 
#' @return The cache matrix
#' 
#' The cache matrix has four methods:
#' $get - get the stored matrix
#' $set - set the stored matrix
#' $setinvert <- set the inverted matrix
#' $getinvert <- get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinvert <- function(invert) m <<- invert
  getinvert <- function() m
  list(set = set, get = get,
       getinvert = getinvert,
       setinvert = setinvert)
}


#' Solves/inverts a matrix, caches the first time and returns
#' the cached value any time afterwards
#' 
#' @param x   A cached matrix from makeCacheMatrix()
#' @param ... Parameters to be passed to solve()
#' @return The solved matrix
#' @example
#'  m1 <- matrix(c(2,3,2,2), nrow = 2, ncol = 2, byrow = TRUE)
#'  m <- makeCacheMatrix(m1)
#'  cacheSolve(xx)   # calculates solution
#'  cacheSolve(xx)   # returns cached copy

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
        
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}
