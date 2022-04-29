## The following code is for caching the Inverse of Matrix, Matrix conversion is complex and usually costly and there may be a feasible way
## by caching the inverse of a matrix rather than computing repeatedly.
##Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(a){
    x<<-a
    inver <- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        matrix <- x$get()
        inver <- solve(matrix, ...)
        x$setInverse(inver)
        inver
}


