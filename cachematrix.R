## makeCacheMatrix creates the matrix and takes in the data, which is set to
## 'x'. cacheSolve calculates the inverse of 'm' and returns the inverse.


## In makeCacheMatrix, 'x' is the data input. 'm' is where the cache matrix 
## is so 'm' gets the data and is set depending on what data is set

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve sets the inverse and returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## 
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
