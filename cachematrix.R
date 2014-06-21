makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## returns x
        get <- function() x
        ## finds the inverse of the user-inputed matrix
        setInverse <- function(solve) m <<- solve
        ## returns m, or the cached inverse matrix
        getInverse <- function() m
        ## returns a list of functions 
        list(set = set, get = get,
             setInverse=setInverse,
             getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
        ## tries to get previously solved inverse if any
        m <- x$getInverse()
        ## returns m if m has been solved before
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## gets the user-inputed matrix
        data <- x$get()
        ## assigns the matrix inverse to m
        m <- solve(data, ...)
        ## caches the new matrix inverse as m
        x$setInverse(m)
        ## returns m
        m
}