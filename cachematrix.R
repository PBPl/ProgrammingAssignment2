## Create a set of functions that will place matrices in cache, 
## invert them and provide functionality to retrieve inverted
## matrices from cache.

## The function creates an object that provides functionality
## to cache matrices and to retrieve inverted matrices.

makeCacheMatrix <- function(x = matrix()) {
        # Creates a special "matrix" object that caches its inverse
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        # Create operations for manipulating matrices to be inverted
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# Checks cache for the inverse of the specified
# matrix and returns it. If the inverted matrix
# is not in cache it is created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        s <- x$getsolve()
        #If inverse already exists return cache
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)                             
        s
}
