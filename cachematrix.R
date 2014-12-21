## The overall purpose of this program is to create a set of functions that is able to create a matrix that can cache it's inverse.
## First the matrix is set with the 
## Example functional code for this set of functions
## c=rbind(c(1, -1/4), c(-1/4, 1))
## blah<-makeCacheMatrix(c)
## cacheSolve(blah)
##          [,1]      [,2]
##			[1,] 1.0666667 0.2666667
##			[2,] 0.2666667 1.0666667
## A subsequent run -> yields
## cacheSolve(blah2)
##
##getting cached data
##          [,1]      [,2]
##			[1,] 1.0666667 0.2666667
##			[2,] 0.2666667 1.0666667

## The first function is used to create the inverse cachable matrix. See the heaading comments for an example of calling the function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The following function is used to calculate the inverse of the cached matrix on the first pass and on subsequent passes, it will return
## the cached value. Matrix inversion is an expensive operation and this can save significant time.

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
