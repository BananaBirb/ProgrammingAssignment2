## Put comments here that give an overall description of what your
## functions do

## Create and cache a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## build the Inverse of the stored (cached) matrix build via makeCacheMatrix

cacheSolve <- function(x, ...) {
        m<-x$getinv()
        # make sure Matrix is squared
        d<-dim(m)
        if(!is.null(m) & isTRUE(d[1]/d[2]==1)){
                message("getting cached Matrix")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
