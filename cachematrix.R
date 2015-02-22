## The functions below are made to cache the inverse of a matrix rather than 
## compute it repeatedly

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(sol) {
    s <<- sol
  }
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The Cachesolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache
 
cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
        
}
