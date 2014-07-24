#overall these functions create a matrix, return the inverse of the matrix without calculation
##if it has already been calculated and stored in the cache. Otherwise the inverse is calculated.  

##This function sets the value of a matrix, gets the value of a matrix, 
##sets the value of the matrix inverse and gets the value of the matrix inverse. 
makeCacheMatrix <- function(x = numeric()) {
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

##this function calculates the inverse of the matrix created with the above function. Before 
##doing so it checks the cache to see if the inverse has already been calculated and stored  If so it gets the 
##inverse from the cache and skips the computation.  
cacheSolve <- function(x, ...) {
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
