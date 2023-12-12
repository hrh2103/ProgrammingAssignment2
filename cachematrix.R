## makeCacheMatrix(matrix ==> list)
## Function returning a list with four matrix methods:
## Set matrix value
## Get matrix value
## Set matrix inverse (not computed here)
## Get matrix inverse
## This is effectively a small matrix class where
## the only way to print the value is via the get method.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(list, ... ==> matrix)
## Function that retrieves the cached inverse
## of a makeCacheMatrix object if it exists.
## Otherwise it updates the inverse in-place and returns it.
## This function actually takes any list and will error out
## if it doesn't contain names like a makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
