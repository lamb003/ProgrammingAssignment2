## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# This function creates a "special matrix", this special 
# matrix is a list of functions to set the value of the matrix,
# calculate the inverse and retreive both. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# This function checks if the inverse of the special
# matrix has been computed, if it has it returns the 
# inverse from cached memory. Otherwise it calculates the 
# inverse 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
