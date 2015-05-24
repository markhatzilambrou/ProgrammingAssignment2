## makeCacheMatrix creates an object for storing and accessing a matrix and its inverse
## cacheSolve returns the stored inverse of the above matrix object or finds the inverse if it doesn't exist

## makeCacheMatrix creates an object for storing and accessing a matrix and its inverse
        ## call syntax:  CacheMatrix <- makeCacheMatrix(dataMatrix)
        ## With the above call:
                ## CacheMatrix$set(NewDataMatrix):  stores the NewDataMatrix & sets the inverse to NULL
                ## CacheMatrix$get(): returns the stored matrix
                ## CacheMatrix$setinv(MatrixInverse): stores the MatrixInverse as the inverse
                ## CacheMatrix$getinv(MatrixInverse): returns the stored inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve retrieves the inverse from the matrix object or computes it if it is not already saved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
