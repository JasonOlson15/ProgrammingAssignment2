## The functions in this file calculate the inverse of a matrix but leverage a 
## cache so the calculation is only performed if it has not yet been done.

## ============================================================================
## makeCacheMatrix
## ============================================================================
## Initializes the cache matrix and contains the functions necessary to get and 
## set the data.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setResult <- function(mean) m <<- mean
        
        getResult <- function() m
        
        list(set = set, get = get,
             setResult= setResult,
             getResult = getResult)
}

## ============================================================================
## cacheSolve
## ============================================================================
## Returns the inverse of a matrix. First checks to see if that matrix is in 
## cache and if so it pulls from there otherwise it is calculated.

cacheSolve <- function(x, ...) {
        
        m <- x$getResult()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data,)
        
        x$setResult(m)
        
        m
}
