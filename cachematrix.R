## makeCacheMatrix makes a list of a matrix x and its inverse (using in turn the 
## solve function)
## cacheSolve will return the matrix that is the inverse of x. It will either
## calculate it or not depending on whether it has been calculated in 
## makeCacheMatrix or not


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


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## Check if previously calculated, If yes, returns cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
