#the aim of these functions is to calculate an inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    t <- NULL
    set <- function(y) {
        x <<- y
        t <<- NULL
    }
    get <- function() x
    setinversematrix <- function(solve) t <<- solve
    getinversematrix <- function() t
    # the list of 4 functions is returned
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}
cacheSolve <- function(x, ...) {
    t <- x$getinversematrix()
    #check if the matrix t has already been inversed. If this is the case then cached matrix is returned
    if(!is.null(t)) { 
        message("getting cached data")
        return(t)
    }
    #otherwise, the inversion of the matric take place (in the lines below)
    data <- x$get() 
    t <- solve(data, ...) 
    x$setinversematrix(t)
    t # return the matrix that has been inversed 
}
