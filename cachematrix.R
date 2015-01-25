## Create an R function to cache inverse matrix computations.

##The first function, makeVector creates a special "vector", which is a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
     mY <- NULL
        set <- function(y) {
                x <<- y
                mY <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) mY <<- inverseMatrix
        getInverse <- function() mY
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x' if it has not been calculated. Otherwise, get the cached inverse matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mY <- x$getInverse()
        if(!is.null(mY)) {
                message("getting cached data")
                return(mY)
        }
        data <- x$get()
        mY <- solve(data, ...)
        x$setInverse(mY)
        mY
}
