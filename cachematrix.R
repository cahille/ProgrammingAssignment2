## these two functions are used to create an object to store a matrix, find
## the inverse of your matrix, then cache the inverse in case the inverse
## is requested again

## use this function to create an object that can store a matrix,
## find the inverse and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function either returns the inverse from cache or
## computes, stores and returns the inverse

cacheSolve <- function(matrix=matrix(), ...) {
        inverse <- matrix$getinverse()
        if(!is.null(inverse)) {
                return(inverse)
        }
        m <- matrix$get()
        inverse <- solve(m, ...)
        matrix$setinverse(inverse)
        inverse
}
