# makeCacheMatrix will create a special vector with four functions:
#   1  set --> which will set the matrix to a variable x
#   2  get which will return the value of the variable x
#   3  setinverse --> which will take the inversed martix and setit to variable m
#   4  getinverse will return the cached inversed matrix

require(MASS) #load MASS to use the "ginv" function
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  ## Initialize the inverse property
    set <- function( matrix ) { m <<- matrix; 
                                inv <<- NULL} ## Method to set the matrix
    ## Method the get the matrix
    get <- function() { m  }## Return the matrix
    setInverse<- function(ginv){inv<<-ginv}## Method to set the inverse of the matrix
    ## Method to get the inverse of the matrix
    getInverse <- function()  inv    ## Return the inverse property
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }

# CacheSolve gives the inverse of a matrix: "inv".
# will check if the value stored in inv is null, 
# if it is null, it will calculate the inverse of matrix using solve function and
# make a call to setinverse function which store the value in variable inv again else
# it will returned the cached value

cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ## Just return the inverse if its already set
  if( !is.null(inv) ) { message("getting cached data")
      return(inv)  }
  data <- x$get() ## Get the matrix from our object
  inv <- ginv(data)  ## Calculate the inverse, solve() if it is a squared matrix
  x$setInverse(inv)  ## Set the inverse to the object
  inv ## Return the matrix
}
