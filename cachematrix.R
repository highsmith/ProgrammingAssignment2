## Together these two functions calculate the inverse of a matrix and save the
## results to a cache. Sucessive calls for the inverse do not require 
## recalculation, which can result in much better performance.

## makeCacheMatrix returns a list of 4 functions
##      setMatrix - assigns a matrix input to a cached local variable
##      getMatrix - returns the matrix definition from cache
##      setInvr - accepts the inverse calculation and assigns it to local cache
##      getInvr - returns the cached value or NULL
##      Example: mat<-makeCacheMatrix(rnorm(2500,50,50))
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInvr <- function(inverse) m <<- inverse
        getInvr <- function() m
        list(set = set, get = get,
             setinvr = setinvr,
             getinvr = getinvr)    
}


## cacheSolve accepts the makeCacheMatrix function as its input 
## it will first run getinvr which returns the cached value or NULL
##      if a value is found, a message is printed and the cached value returns
##      if NULL returns it will use the getMatrix function to return the matrix, 
##       and calculate the nverse, and then set the inverse to cache with setInv 
## Example: cacheSolve(mat)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvr()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInv(m)
        m
}       

