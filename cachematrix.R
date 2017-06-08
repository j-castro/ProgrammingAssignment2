## These functions compute the inverse of a matrix and store it in
## cache to make faster computations of its inverse in the future

## Creates a special matrix  that stores its inverse in cache 
makeCacheMatrix <- function(x = matrix()) {
	I <- NULL  ## Matrix inverse
	set <- function(y) { ##Set matrix value
                x <<- y
                I <<- NULL
        }
        get <- function() x ##Get matrix value
        setInv <- function(inv) I <<- inv ##Set inverse of matrix
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Computes the inverse of special matrix 'x' returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {      
	I <- x$getInv()
	if (is.null(I)) {
		data <- x$get()
		I <- solve(data, ...)
		x$setInv(I)
	}
	else { print("Inverse of matrix retrieved from cache") }
	I
}