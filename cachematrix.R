## This program consists of two functions that will prevent needlessly 
## recalculating the inverse of a matrix if it has previously been done, which
## is more efficient and faster. 

## This function creates a special matrix object that can cache its inverse.
## We will call makeCacheMatrix with an invertible matrix. This function will
## add methods to the matrix object. 

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse
             )
}


## This function computes the inverse of a cacheable matrix object. If the 
## inverse has already been calculated and the matrix has not changed, the
## inverse is retrieved from the cache; otherwise, the inverse is calculated
## using the solve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
