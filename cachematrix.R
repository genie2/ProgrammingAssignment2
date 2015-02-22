## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Make an instance of matrix that also stores its inverse


makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Returns the inverse of matrix stored in an makeCacheMatrix object.
## If the inverse has been calculated previously, return the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
