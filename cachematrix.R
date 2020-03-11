## Cache the inverse of a matrix

## Create a special "matrix" object and cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	set_inverse <- function(inv) inverse <<- inv
	get_inverse <- function() inverse

	list(set = set, get = get,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}


## Return cached inverse of matrix. If none, calculate
## the inverse and put it into the cache before returning
## the result

cacheSolve <- function(x, ...) {
	inverse <- x$get_inverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$set_inverse(inverse)
	inverse
}
