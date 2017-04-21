## This function creates a special "matrix" object that can cache its 
## inverse.

## Set x to default to matrix
makeCacheMatrix <- function(x = matrix()) {
        ## Clear values
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cachesolve <- function(x, ...) {
        inv <- x$getinv()
        ## If not null then cache exists and can be retrieved
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If cache does not exist then compute inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
