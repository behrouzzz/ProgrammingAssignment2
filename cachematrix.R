## My function will, in general, gets a matrix as input and calculate the inverse of it
## However, if the inverse of the matrix has already been calculated and is in the memory, then
## it will not caculate it again. Instead, the function will read the inverse from cached data
## and will return the inverse from the cached data.





## ====================makeCacheMatrix=================================
## the makeCacheMatrix will get the matrix from the user. Once run, it can also get a new matrix from
## the user. It can get the inverse from the second function (cacheSolve) and consequently update 
## the value of the inverse for the matrix.
## To do that, it creates a list with four categories set, get, getInv and setInv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    ## the value of inverse is null in the beginning
  set <- function(y) {          ##new matrix
    x <<-y
    inv <<- NULL
  }
  get <- function() x         
  setInv <- function(inverse) inv <<- inverse     ##update the inverse
  getInv <- function() inv                        ## get the inverse
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}

##==================================cacheSolve==========================
## This function (cacheSolve) finds the inverse of the matrix given by the user through the function 
## (makeCacheMatrix). If the inverse of the matrix has already been calculated and is in the memory,
## then it will not calculate it again and will just read it from the cache. However, if a new matrix
## was given by the user, then this function (cacheSolve) will calculate the inverse again and
## give the inverse as output. It will also call the first function again to update the value of the
## inverse in

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()             # get the inverse from makeCahceMatrix
  if(!is.null(inv)) {           # If cache is not empty just return the value
    message("getting the inverse from cache")
    inv                         # returns the inverse without calculating it
  } else {                      # if cache is empty (else)
  mat <- x$get()                # get the matrix
  inv <- solve(mat)             # calculates the inverse
  x$setInv(inv)
  message("calculating the inverse")
  inv
  }
}

