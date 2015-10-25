## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will 
## find it in the cache and return it, and not calculate it again.

## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv <<-inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## if the inverted matrix is in cache, return the calculated result;
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value is 
## stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if (!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
        } 
		else {
        inv <- solve(x$get())
        x$setinverse(inv)
        inv
        }
}
