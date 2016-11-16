
## This function is initializing a Cache matrix 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- matrix()
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function returns the Inverse of a square matrix initialized before with the
## makeCacheMatrix function. if it is already calculated before, the value wil be 
## get from the caching and no need to re-calculate it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s

}
