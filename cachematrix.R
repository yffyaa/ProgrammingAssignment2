## The below functions are used to catch the inverse of a matrix by creating a object that stores a matrix
## and catch its inverse.


## This function creates a matrix object that can cache it inverse.

makeCacheMatrix <- function(x = matrix()) {
             inv <- NULL
	     set <- function (y){
	            x <<- y
		    inv <<- NULL
	     }
             get <- function() x
             setinv <- function(inverse) inv <<- inverse
             getinv <- function() inv
	     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the matrix created by the function above. If the inverse has already
## been calculated, then just get it from the cache.

cacheSolve <- function(x, ...) {
              inv <- x$getinv()
	      if(!is.null(inv)){
		       message ("getting cached data")
		       return(inv)
	       }
	       data <- x$get()
	       inv <- solve(data, ...)
	       x$setinv(inv)
	       inv        
}




