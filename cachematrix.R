## Put comments here that give an overall description of what your
## functions do

## Creates a "matrix" - really, a list of four functions associated to a
## matrix - which do the following:
## 1) Set the matrix associated to the list
## 2) Get the matrix associated to the list
## 3) Set the inverse of the matrix associated to the list
## 4) Get the inverse of the matrix associated to the list 

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	setinv <- function(inv) xinv <<- inv
	getinv <- function() xinv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

## Computes the inverse of the "matrix" as generated above; however,
## it first checks to see if the inverse has been calculated and stored
## by calling the getinv() function. If the inverse already is stored, 
## it simply outputs the cached data.

cacheSolve <- function(x) {
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
