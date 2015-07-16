
 ## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse
  ##computes the inverse of the special "matrix" returned by makeCacheMatrix and 
 ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
 ## retrieve the inverse from the cache.

##  makeCacheMatrix creates a special "matrix" object that can cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      a <- NULL
      set  <- function(y){
              x <<- y
            a <<- NULL 
    }
      get  <- function() x
      setinverse  <- function(inverse) a  <<- inverse
     getinverse  <- function() a
    list(set= set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
 
 }



## The second function cacheSolve calls functions stored in the special "matrix" returned by makeCacheMatrix (above). 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache. If the input is new, 
## it calculates the inverse of the data and sets the inverse in the cache.

 
 cacheSolve <- function(x, ...) {
      a <- x$getinverse()
    if (!is.null(a)){
            message("getting cached data")
            return(a)
   }
   data  <- x$get()
   a  <- solve(data, ...)
   x$setinverse(a)
  a
 }


