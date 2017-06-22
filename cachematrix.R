## Script contains pair of functions that cache the inverse of a matrix.
##
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).


## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	# Set default value of inverse to NULL
	inverse <- NULL

	# set the value of the Matrix
	setMatrix <- function(y) {
		# set matrix value
		x <<- y

		# clear cache
		inverse <<- NULL
	}

	# Get matrix value
	getMatrix <- function() x

	# Set inverse
	setInverse <- function(mean) inverse <<- mean

	# get the value of the mean
	getInverse <- function() inverse
	list(
		setMatrix = setMatrix,
		getMatrix = getMatrix,
		setInverse = setInverse,
		getInverse = getInverse
	)

}

## Return the reverse of the matrix input
cacheSolve <- function(x) {
	
	# This fetches the cached value for the inverse
	inverse <- x$getInverse()
	
	# If the cache was not empty, we can just return it
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	
	# The cache was empty. We need to calculate it, cache it, and then return it.
	# Get value of vector
	data <- x$getMatrix()

	# Calculate inverse
	inverse <- solve(data)

	# Cache the result
	x$setInverse(inverse)
										
	# Return the inverse
	inverse
}
