## Matrix inversion is usually a costly computation;
## hence, there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.
##
## The following two functions will take advantage
## of the scoping rules of the R language and how
## they can be manipulated to preserve state inside
## of an R object, in order to build a special "matrix
## object" that can cache its inverse and retrieve it
## whenever needed.


## Example of usage
##
## > m <- matrix(rnorm(10000), nrow = 100, ncol = 100)
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)                // the inverse is calculated and stored
## > cacheSolve(cm)                // this time the cached inverse is used


## makeCacheMatrix()
##
## Creates a special "matrix object" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # will store the cached inverse
    inv <- NULL

    # setter for the matrix data
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
  
    # getter for the matrix data
    get <- function() x
  
    # setter for the inverse
    set.inverse <- function(inverse) inv <<- inverse
  
    # getter for the inverse
    get.inverse <- function() inv
  
    # return the new "matrix object"
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## cacheSolve()
##
## Computes the inverse of the special "matrix" returned
## by makeCacheMatrix() above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # tries to return the cached inverse
    inv <- x$get.inverse()
    if(!is.null(inv)) {
        return(inv)
    }
  
    # the inverse was not cached; calculate it now
    data <- x$get()
    inv <- solve(data, ...)
  
    # store the inverse in cache for late retrieval
    x$set.inverse(inv)
  
    # return the inverse
    inv
}
