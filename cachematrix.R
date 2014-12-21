## These two functions allow to create a special matrix
## variable that caches its inverse when it is calculated

## The following function creates a list to store
## the matrix x and its inverse inv once calculated.
## The functions set and get allow access to the matrix value. 
## The functions setinverse and getinverse allow access to the 
## inverse value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv  <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv  <<- solve
        getinverse <- function() inv 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following functions takes a variable created
## with makeCacheMatrix and returns its inverse calculated
## with solve().
## On the first call it calculate the inverse of x and stores 
## it in the variable inv
## On later calls, if x hasn't changed, it accesses the
## previously calculated inv variable.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
