##  This function stores a matrix (x) and can cache the inverse of that matrix
   # The returned list provides a set of functions that:
    #Set a new matrix (set)
    #Retrieve the current matrix (get)
    #Cache the mean of the matrix (setinverse)
    #Retrieves the cached inverse if available (getinverse)

makeCacheMatrix <- function(x = matrix()) { 
  inv<-NULL       #inv - variable to store the inverse of x (starts as NULL)
  set<-function(y) { # set() updates value of matrix x
    x<<-y        # <<- assigns a value for x (input of set())
    inv<<-NULL   # <<- if x is updated, inv is set to NULL 
  }
  get <- function() x     # get retrieves the current matrix x
  setinverse <- function(inverse) inv <<- inverse  # setinverse takes x inverse                                     
  getinverse <- function() inv               # as argument and caches it in inv
  list(set = set, get = get,         # getinverse retrieves cached inversed 
       setinverse = setinverse,      #                 inverse if it exists
       getinverse = getinverse) # Finally a list with the four functions
}                               #                         is established

## The purpose of cacheSolve is to compute the inverse of the matrix stored
#  in makeCacheMatrix (mCM), but with caching to avoid unecessary computations;

# This function requires that x is stored in mCM to run, for example:
#   Before using cacheSolve, m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
#   to store that square matrix, and 
#   m$setinverse(solve(matrix(c(1,2,3,4),2,2))) to store its inverse (optional)
#   Then, call cacheSolve(m) to obtain the inverse of that matrix

cacheSolve <-function(x, ...) {
  inv <- x$getinverse() #uses getinverse() to retrieve the cached inverse of x
  if(!is.null(inv)) {  # checks if cached inverse of x exists
    message("getting cached data")  # If so, message is displayed 
    return(inv)             # and also cached inverse is immediately returned 
  }                   #this saves time of calculation of inverse; Else:
  m <- x$get() #stores matrix (in variable m) stored in x with get() from mCM
  inv<-solve(m, ...) #calculates the inverse of m
  x$setinverse  # stores this inverse in inv using set() from mCM
  inv    #returns inv 
}
