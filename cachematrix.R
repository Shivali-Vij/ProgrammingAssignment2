## Matrix inversion is usually a costly computation and their may be some benefit to 

##caching the inverse of a matrix 

##rather than compute it repeatedly



## function makeCacheMatrix creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
	m<-matrix()
	set <- function(y) {  			## Method to see the matrix
						## This function sets the cache m to null 

						##again if the matrix changes
            			   x <<- y
             			   m <<- NULL
        }

	get <- function() {			## Method that returns the matrix
	x

	}


	setinverse<-function(inverse){		##Method to set the inverse of matrix
					
					 m<<-inverse
	}



	getinverse <- function(){		## Method to return the inverse of matrix
				 	
					m

	}


	## To return the list of methods
	list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)

}## End of makeCacheMatrix


## this function checks if cache exits. If the inverse has already been calculated 
## and the matrix has notchanged), 

## then the "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()


	## Just return the inverse if its already set

	if(!is.null(m)) {
                message("We have already calculated this inverse. Fetching cached data")
		return(m)
               
        }

 	## Get the matrix from our object
	
	matrix <- x$get()
	inverse<-matrix()
	## Calculate the inverse using matrix multiplication
    	inverse <- solve(matrix)
	 message("Calculating inverse")

    	## Set the inverse to the object

    	x$setinverse(inverse)
	message("Caching the calculation")

    	## Return the matrix

    	inverse


}


##########################################################
##########################################################
#To see it run,
#Run the code as follows:
#
#
#t<-makeCacheMatrix()
#t$set(matrix(1:4, 2, 2))
# t$get()
#> t$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(t)
#Calculating inverse
#Caching the calculation
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(t)
#We have already calculated this inverse. Fetching cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
