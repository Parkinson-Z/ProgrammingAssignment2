## These R functions are used to cache potentially time-consuming computations. 

## 1st function, CacheMatrix creates a special "matrix", which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse of matrix
##    get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## 2nd function calculates the inverse of the special "matrix" created with the above function. However, it 
## first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
