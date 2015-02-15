##Jamie Signorile
## The following two functions are used to efficiently store and compute the inverse of a matrix. The first matrix will concentrate on caching data, while the second, will be used to effectly pull or compute the inverse of a matrix.

## Function used to create a special matrix used to cache the inverse of the matrix. Returns a list of functions used to set the matrix, get the matrix, set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    setinverse <- function(inverse) i<<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks to see if the matrix has a cached inversed, to save computation time. If there is no cached inverse, the function calculates the inverse and sets it via "set inverse"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
    if(!is.null(i)){
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
