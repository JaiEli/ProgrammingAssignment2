
makeCacheMatrix <- function(x = matrix()) {
        inp <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setVerse <- function(inverse) inp <<- inverse
        getVerse <- function() inp
        list(set = set,
             get = get,
             setVerse = setVerse,
             getVerse = getVerse)
}


## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inp <- x$getVerse()
        if (!is.null(inp)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inp <- solve(mat, ...)
        x$setVerse(inp)
        inp
}
