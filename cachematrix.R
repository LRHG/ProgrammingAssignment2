## Name:  Enita Ejoor
## Date: 28th Nov 2016
## Title:	Coursera Week 3 R Programming Assignment
## Topic:	Lexical Scoping


## This function is a constructor function which creates a matrix
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix())
{
	inverse <- NULL
	set <- function(y)
	{
		x 	  <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function() inverse <<- solve(x)
	getInverse <- function() inverse

	list	(
			set 		= set
			,get 		= get
			,setInverse = setInverse 
			,getInverse = getInverse
		)

}


## The cacheSolve function checks if the inverse of the matix
## already exists and returns that value or calculates the 
## inverse value of the matrix in question

cacheSolve <- function(x, ...) 
{
 	## Return a matrix that is the inverse of 'x'

	inverse 	<- x$getInverse()
	if(!is.null(inverse))
	{z
		message("getting cached data")
		return(inverse)
	}

	data 		<- x$get()
	inverse 	<- getInverse(data, ...)
	x$setInverse(inverse)
	inverse 
}

## RESULT:
## =======
## x <- matrix(1:4, nrow = 2, ncol = 2)
## inv4 <- makeCacheMatrix(x)
## inv4

##<environment: 0x00000000140fce80>
