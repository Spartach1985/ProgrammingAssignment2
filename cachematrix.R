## Function for caching inverse of a matrix
## I initialize the matrix with makeCacheMatrix, and then I will calculate its inverse
## Only the first time when I call cacheSolve.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)

}


## We check whether getinv() of our matrix returns NULL. If yes - we calculate the inverse.
## If no, we return getinv().

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("Getting cached inverse matrix")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
