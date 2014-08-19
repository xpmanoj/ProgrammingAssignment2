## makeCacheMatrix creates special "matrix" whereas the cacheSolve returns the inverse of the
## matrix from cache, if it exists there. if not, it computes the inverse and returns it.

## makeCacheMatrix creates a special "matrix" ,which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve returns the inverse of a matrix if it exists in the cache. 
## if not, it computes the inverse and returns it

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

## Test both functions with test data
a = matrix( c(4,10,23,12), nrow=2, ncol=2) 
mat <- makeCacheMatrix(a)
cacheSolve(mat)
cacheSolve(mat)
