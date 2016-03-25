## The following functions calculate the inverse of a matrix, using the cached value when 
## available to speed up computation time.

## makeCacheMatrix creates a list containa a function to set and get a matrix value
## and then set and get the inverese of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y)
{
x <<- y
m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the above matrix,checking first
## to see if the inverse has already been calculated and using it when possible to avoid recalculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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