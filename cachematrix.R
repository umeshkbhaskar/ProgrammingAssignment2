##  `makeCacheMatrix` is a function which creates a special "matrix" object
that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<-solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##  `cacheSolve`is another function which computes the inverse of the special
matrix returned by `makeCacheMatrix` above. If the inverse has
already been calculated (and the matrix has not changed), then the
`cachesolve` should retrieve the inverse from the cache.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
       data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
} ## Return a matrix that is the inverse of 'x'

