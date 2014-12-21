## Calculates inverse to a matrix, storing / accessing a cached version if previously calculated

## Creates a list containing functions to set / get a matrix and its inverse (or NULL)

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL               ## resets inverse to NULL
	set <-function(y) {     ## sets x to matrix, m to NULL
			x <<-y
			m <<-NULL
	}
	get <- function() {x}
	setinverse <- function(inverse) {m <<- inverse}  ## sets m to inverse calculated in cacheSolve
	getinverse <- function() {m}                     ## returns inverse in cache
	list(set = set, get = get, 
		setinverse = setinverse, getinverse = getinverse)
}


## Returns cached inverse of a matrix, or calculates, then caches, inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()   ## gets m from makeCacheMatrix
		if(!is.null(m)){      ## returns the cached inverse (if not null)
			message("getting cached data")
			return(m)
		}
		data <- x$get()        ## gets value of the original matrix
		m <- solve(data, ...)  ## calculates inverse of matrix
		x$setinverse(m)        ## caches inverse
		m
}