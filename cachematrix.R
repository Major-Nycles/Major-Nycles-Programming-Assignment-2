## When used in conjunction, these two functions allow for the creation, 
## storage and retrieval of the inverse of a given matrix. makeCacheMatrix()
## creates an object with the original matrix and its inverse (initially set 
## to NULL) and cacheSolve calculates and returns the inverse of that matrix,
## or if it already exists,retrieves it from memory rather than compute it 
## again unnecessarily.

## Make cache object creates two instance variables associated with objects
## of this type (the original matrix(x) and its inverse) and defines four
## nested functions that will subsequently be used to calculate and retrieve
## the invers eof the matrix, x.

makeCacheMatrix <- function(x = matrix()) {
  ## sets the instance variable 'inverse' for each object instantiated to NULL.
  ## Each class will now have two variables; x, which has been defined in the 
  ## argument passed to the function, and inverse, which is effectively 
  ## defaulted to NULL.
  inverse <- NULL   
  ## set() allows for the reset of the object with a new argument.
  ## The <<- assignment operator looks above this function to find that x
  ## and inverse have been defined previously, and are assigned these new 
  ## instance variables.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get() simply allows the retrieval of the instance variable x
  get <- function() x
  ## setinverse() takes the input argument i and uses the assignment operator 
  ## <<- to look above the function tofind and assign i to inverse.
  setinverse <- function(i) inverse <<- i
  ## getinverse() retrieves the variable assigned to inverse
  getinverse <- function() inverse
  ## as the last line of the makeCacheMatrix function, this list is returned 
  ## whenever this function is called. By assigning a name to each function,
  ## subsequent calls to the functions (methods) nested within this function
  ## can be called by name, ie. testmatrix$get().
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve either retrieves the inverse of the matrix passed to it, or 
## creates and returns its inverse if it hasn't previously been calculated.

cachesolve <- function(s) {
## The first action of the cachesolve() function is to retrieve the inverse 
## of the s object passed to it as an arguement.
  i <- s$getinverse()
## if the inverse of s is not null, it will not be calculated again and will
## be retrieved from the s object.
  if(!is.null(i)) {
    message("getting cached inverse...")
    return(i)
  }
## However, if it hasn't been previously calculated and cached (hence, it 
## is NULL), the original matrix is retrieved using s$get(),it's inverse is 
## calculated using solve(), and the setinverse() function is called to 
## define the inverse within the s object.
  this_matrix <- s$get()
  i <- solve(this_matrix)
  s$setinverse(i)
  i
}
