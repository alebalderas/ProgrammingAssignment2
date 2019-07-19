## The following functions create a matrix in the global context and perform the inverse operation
## if the inverse operation is successful then the result is stored in cache so that it can be reused
## Use example:
## my_m<-matrix(c(1,3,2,2),2,2)
## my_object<-makeCacheMatrix(my_m)
## cacheSolve(my_object)


# Function: makeCacheMatrix
# This function takes a matrix as argument and stores it in the cache
# And returns a set of methods to set and retrieve the original and inverse matrix
# @args: m=matrix to be created/stored to be inversed posteriorly
makeCacheMatrix <- function(m = matrix()) {

  # Fucntion to set the initial values
  set<-function(m){
    f <<- NULL
    orig_matrix <<- m
    inverse_matrix <<- NULL
  }
  
  #This function will return the original matrix to apply the inverse
  getmatrix<-function() orig_matrix
  
  #Functions to set and get the inverse of the matrix to/from the cache
  setinverse<-function(inverse) inverse_matrix <<- inverse
  getinverse<-function() inverse_matrix
  # Call to set the initial values
  set(m)
  
  #The list of methods to be returned by this method
  list(set = set, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


# Function: cacheSolve
# This function takes the matrix created by the function makeCacheMatrix and tries to produce the inverse matrix
# If the provided matrix has not changed and the inverse has already been created, then the inverse is returned from cache
# @args: f=Object returned by the makeCacheMatrix function
cacheSolve <- function(f, ...) {
  out<-tryCatch({
    #First, try to get the inverse from cache
    inv_matrix <- f$getinverse()
    #Check if the inverse is in the cache
    if(!is.null(inv_matrix)){
      message("Returning inverse matrix from cache: ")
      return(inv_matrix)
    }    

    ## Return a matrix that is the inverse of 'orig_matrix'
    ## Try the inverse of the matrix, Singular matrix cannot doesn't have inverse
    m<-f$getmatrix()
    inverse<<-solve(m) 
    f$setinverse(inverse)
    return(inverse)
  }, 
  error=function(cond) {
    message("Error while performing matrix inverse")
    message("Original error message:")
    message(cond)
    message("\n")
    #Return value in case of error
    return(NULL)
  },
  warning=function(cond) {
    message("Warning while performing matrix inverse")
    message("Original warning message:")
    message(cond)
    message("\n")
    #Return value in case of warning
    return(NULL)
  }
  )
}
