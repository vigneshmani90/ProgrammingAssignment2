## The objective is to create an inverse of a matrix and cahce the same in
## memory. Since the inverse of matrix is a costly operation by saving it in
## cache allows us  to reuse the value, without computing it again and again.

## This function will create a cache for a particular matrix as its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
		
	}
	get <- function() x
	setInverse <- function(solve) inv <<- solve
	getInverse <- function() inv
	list(set =set,
		 get = get,
		 setInverse = setInverse,
		 getInverse =  getInverse)

}


## This function will check if there a particular matrixs' inverse is already cached, if so return it, else 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <-x$getInverse()
		if(!is.null(inv)) {
				message("getting cahced data")
				return(inv)
		}
		mat <- x$get()
		inv <-solve(mat, ...)
		x$setInverse(inv)
		inv
}