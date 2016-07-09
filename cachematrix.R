## write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
         }
        get <- function() x
        setinv <- function(solve) iv <<- solve
        getinv <- function() iv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        iv <- x$getinv()
        if(!is.null(iv)) {
                 message("getting cached data")
                 return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setinv(iv)
        iv
}

## Test1

x <- matrix (1:4, 2, 2)
x
myiv <- makeCacheMatrix(x)
cacheSolve(myiv)


## Test2
r <- rnorm(100, 10, 2)
x <- matrix (r, nrow = 10, ncol = 10)
x
cachex <- makeCacheMatrix(x)
cacheSolve(cachex)
