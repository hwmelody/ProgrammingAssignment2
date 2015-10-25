## The following two functions are created to cache the inverse of a matrix when calculated the first time
## so that the function will return the inverse of the matrix from cache rather than computing it again the
## next time 


## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse,
## in the following steps

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix
	
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function()inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This second function computes the inverse of the special "matrix" returned by the first function
## in the following steps
## 1. if the inverse has been calculated, it returns the inverse from the cache
## 2. if the inverse has not been calculated, it calculates the inverse of the matrix using the solve function

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
