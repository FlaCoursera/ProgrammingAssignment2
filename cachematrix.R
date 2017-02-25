#This function makeCacheMatrix creates a matrix which is a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invMtx <- NULL
    set <- function(y) {
        x <<- y
        invMtx <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) invMtx <<- solve
    getsolve <- function() invMtx
    
    list(set = set, get= get, setsolve = setsolve, getsolve = getsolve)
}


## The following function calculates the inverse (solve) of the matrix created with makeCacheMatrix function. 
## It first checks to see if the inverse of the matrix has already been calculated. If so, it gets the information from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setsolve function.

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
