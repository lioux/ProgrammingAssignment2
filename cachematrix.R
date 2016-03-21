## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	# Always initialize inverse matrix as NULL
	inverse <- NULL

	# Set y as internal matrix. Initialize inverse matrix as NULL.
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# Get the internal matrix
	get <- function() x

	# Set inverse matrix of x
	setinverse <- function(inv) inverse <<- inv

	# Get inverse matrix of x
	getinverse <- function() inverse

	# Returns list of functions
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the inverse matrix of the special "vector" created with
# the function makeCacheMatrix above. However, it first checks to see if the inverse matrix
# has already been calculated. If so, it gets the inverse matrix from the cache and skips
# the computation. Otherwise, it calculates the inverse matrix of the data and sets the value
# of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x) {
		# get inverse matrix
		inverse <- x$getinverse()

		# if it isn't NULL, i.e., inverse matrix value has been cached
		if(!is.null(inverse)) {
			message("getting cached data")

			# return cached inverse matrix
			return(inverse)
		}

		# otherwise, calculate inverse matrix then cache it
		data <- x$get()
		inverse <- solve(data)
		x$setinverse(inverse)

		# Return a matrix that is the inverse of 'x'
		inverse
}




#####
## Usage example
#####
#
## Simple diagonal 2 by 2 matrix
# > diag(2,2)
#     [,1] [,2]
#[1,]    2    0
#[2,]    0    2
#
## Assign x from makeCacheMatrix with diagonal 2 by 2 matrix
#> x <- makeCacheMatrix(diag(2,2))
#
## Solve it for the first time. Notice that it does not retrieve cached information
#> cacheSolve(x)
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
#
## Solve it for the second time. Notice that it does retrieve cached information
#> cacheSolve(x)
#getting cached data
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
#
## Verify that the result is correct
#> solve(diag(2,2))
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
#
## Verify against x object
#> solve(x$get())
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
#
## Print cached inverse matrix
#> x$getinverse()
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
