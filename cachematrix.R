
## makeCacheMatrix creates a special matrix object that can be cached
makeCacheMatrix <- function(x = matrix()) {
        # initialise the cache value
        cache <- NULL

        # set the matrix
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        # get the matrix
        get <- function() x
        # store an inverted matrix in cache
        setMatrix <- function(inv) cache <<- inv
        # get the inverted matrix from cache
        getInverse <- function() cache

        # create a list containing the functions introduced
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}



## cacheSolve either computes the inverse of the object
## stored in makeCacheMatrix() or gets it from cache
cacheSolve <- function(x, ...) {
        ## attempt to get a cached inverse, if one exists
        cache <- x$getInverse()

        # return inverted matrix from cache, or...
        if (!is.null(cache)) {
                message("getting cached data")

                # display cached matrix
                return(cache)
        }
        
        # compute the required inverted matrix, by...

        # retrieving the original matrix
        matrix <- x$get()

        # inverting it and assigning to the cache value
        cache <- solve(matrix, ...)
        # using the setMatrix function defined above to cache the calc'd inverse
        x$setMatrix(cache)

        # displaying the calc'd inverse matrix
        return (cache)
}
