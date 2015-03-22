## makeCacheMatrix function redefines a matrix to consist of 4 functions as a list.
## In this function the inverse is stored as m in the environment.
##cacheSolve passes the makeCacheMatrix list as the first argument. If the matrix has
## already calculated the inverse, then directly print out getinverse, otherwise use 
## solve function to calculate the inverse, assign setinverse the value and print out.

## Cache the inverse of the matrix if exists.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieve the inverse or calculate the inverse if it's never been done. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
