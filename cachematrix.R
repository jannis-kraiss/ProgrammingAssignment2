## The following two functions are used to create a special object that stores a matrix and caches the
## inverse of this matrix. 

## This first function creates a special matrix and returns a list containing functions to
## set the value of the matrix, get the value of the matrix, set the value of the inverse
## and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by the function makeCacheMatrix. 
## If the inverse has already been calculated using the function above, CacheSolve retrieves 
## the inverted matrix from the cache and skips computation. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

### Testing the functions

x <- matrix(1:4, 2,2) #Create matrix

y <- makeCacheMatrix(x) #Create a special matrix using makeChacheMatrix function

cacheSolve(y) #Return inverse of the matrix using CacheSolve

