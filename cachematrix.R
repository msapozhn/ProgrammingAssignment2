#######################
## Maxim Sapozhnikov ##
#######################
## Week 3 Assignment ##
#######################

## Creates object of type "Matrix with inversion cache"
## Object functions available:
##   get       returns stored value
##   set       sets stored value, erases cache
##   getsolve  returns stored cache
##   setsolve  sets stored cache

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(m) {
        x <<- m
        cache <<- NULL
    }
    get <- function() {
        x
    }
    setsolve <- function(m) {
        cache <<- m
    }
    getsolve <- function {
        cache
    }
}

## Calculates cache if necessary
## Returns NULL and throws a warning if matrix is not square

cacheSolve <- function(x, ...) {
    mout <- x$getsolve()
    if (is.null(mout)) {
        m <- x$get()
        if (length(dim(m)) == 2 & dim(m)[1] == dim(m)[2]) {
            mout <- solve(m, ...)
        }
        else {
            warning("X: not a square matrix")
        }
        x$setsolve(mout)
    }
    mout
}
