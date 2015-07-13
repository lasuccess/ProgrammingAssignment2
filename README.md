 makeCacheMatrix <- function(x = matrix()) {
+       A  <- NULL
+        set  <- function(y){
+                x <<- y
+                A <<- NULL 
+        }
+        get  <- function() x
+        setinverse  <- function(inverse) A  <<- inverse
+        getinverse  <- function() A
+        list(set= set, get = get, 
+             setinverse = setinverse, 
+             getinverse = getinverse)
 
 }
## makecacheMatrix creates a special "matrix" that can cache its inverse


+## cachesolve Computes the inverse of a special matrix returned by the `makeCacheMatrix` above If the inverse has already been calculated (and the matrix has not changed),
+## then the cachesolve should retrieve the inverse from the cache.
 
 cacheSolve <- function(x, ...) {
-        ## Return a matrix that is the inverse of 'x'
+        A  <- x$getinverse()
+        if (!is.null(A)){
+                message("getting cached data")
+                return(A)
+        }
+        data  <- x$get()
 i  <- solve(data, ...)
+        x$setinverse(A)
+        A
 }
