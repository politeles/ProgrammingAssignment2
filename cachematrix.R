## CacheMatrix file
## Implementation of a cached version of the inverse matrix.
## USAGE: first compute a cached matrix from your original matrix
## Example: 
## Original matrix:
## b = matrix(c(2,5,3,1,5,7,5,4,6), nrow = 3, ncol = 3)
## Cached matrix:
## cached<- makeCacheMatrix(b)
## Calculate the inverse:
## cacheSolve(cached)


## this function makes a cached matrix from standard R matrix

makeCacheMatrix <- function(x = matrix()) {
    s<- NULL # here is where the inverse would be stored
    set <- function(y){
      x <<- y #here is where the matrix would be stored
      s <<- NULL
    }
    get <- function() x # returns the matrix
    setinv <- function(solve) s<<- solve # stores te inverse
    getinv <- function() s # returns the inverse value.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function implements a cached version of the solve method
## for a given cached matrix x:

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<- x$getinv()
    if(!is.null(m)){
      message("getting cached data")
      return (m)
    }
    data <- x$get()
    m<- solve(data, ...)
    x$setinv(m)
    m
}
