## First function makeCacheMatrix takes the input of matrix from user and stores the value of inverse of matrix after first calculation for future use.
## Second function cacheSolve checks if the inverse of matrix alread calculated by lookinf in the first function and if not calculates, stores in first function and displays the result.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { 								##  This will take the input  of  matrix from the user 
		m<-NULL 																			##  sets the initial value of inverse of matrix NULL  so that if the matrix supplied is different from
																									##  the value of inverse of matrix from previous matrix is set to NULL. This one is used in 2 different instances : 
																									##  1) when this function is used for first time, 2) when input matrix is changed to different one by x$ set command
		set<-function (y){  
			x<<- y
			m<<- NULL
		}
		get<-function()x
		setinverse<-function (inverse) m <<- inverse
		getinverse<-function() m
		list( set = set , get=get, 
				setinverse=setinverse,
				getinverse= getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
		m<-x$getinverse() 																##  this is the line of code when user asks for the inverse of matrix- getting the value of inverse matrix (m) from  makeCacheMatrix function
		if(!is.null(m)) { 																		##  checking if the value of m already calculated from previous operation. If there is a store value of m it will go to next line of code below
			message("getting cached data") 										##  giving a message that its getting the value of m from previous calculations
			return(m)  																		##  output the value fo m
		}  																							##  if the value of m is NULL the following line of code is executed
		data<-x$get() 																		##  gets the value of matrix from makeCacheMatrix function
		m<-solve(data) 																	##  calculates the inverse of matrix by solve function
		x$setinverse(m) 																	##  sets the value of inverse of matrix to makeCacheMatrix  function to be used later
		m  																						##  Return a matrix that is the inverse of 'x', no message is displayed as its  calculating the value first time
}
