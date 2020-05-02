## These two functions are used to create a special object that stores a matrix and caches the
## inverse of this matrix. 

## This first function creates a special matrix and returns a list containing functions to
## set the value of the matrix, get the value of the matrix, set the value of the inverse
## and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by the function makeCacheMatrix. 
## If the inverse has been calculated using the function above, CacheSolve retrieves the 
## inverted matrix from the cache. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

### Testing

x <- matrix(1:4, 2,2) #Create matrix

y <- makeCacheMatrix(x) #Create a special matrix using makeChacheMatrix function

cacheSolve(y) #Return inverse of the matrix using CacheSolve

