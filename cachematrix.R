## makeCacheMatrix creates a special matrix object that has the ability to cache any computed data on the matrix
## cacheSolve calculates the inverse of the matrix when it doesnt find it in the cache. Makes sure to cache it 
## before returning the result

## makeCacheMatrix returns the list of functions that can be used set or get the inverse of the matrix 
## along with functions to set and get the matrix itself

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inv) inverse_x <<- inv
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve returns the inverse of a matrix A created with makeCacheMatrix function
## If the cached inverse is available, cacheSolve retrieves and returns it
## If not, it computes, caches, and then returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("return the cached inverse")
        return(inverse_x)
    } else {
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
    }
}
