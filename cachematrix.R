## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. 
## The assignment is to write a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object (getInverse) that can cache its inverse.
## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL #local assignment of m
  set<- function(y){ #update value of matrix 'x'
    x<<-y
    m<<-NULL
  }
  get<- function() x #get will be set to the martix 'x'
  setInverse<-function(solvevar) m <<-solvevar #the inverse matrix will be passed to m (value calculated in cacheSolve function)
  getInverse <- function() m #getInverse will be set to m from previous line of code
  ls()
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getInverse()
  if(!is.null(m)){  #execute if m has a cached value
    message("getting cached data")
    return(m)  #If m isn't null (i.e. the mean is cached), the function exits via the return statement in the if code block. If m IS null, the code in the if block isn't executed.
  }
  data<-x$get() #
  m<-solve(data, ...) #set 'm' equal to the inverse of the matrix
  x$setInverse(m) #setInverse will set to 'm' from previous line of code
  m
}


a=matrix(1:4, nrow =2, ncol =2)
b<- makeCacheMatrix (a)
inv<-cacheSolve (b)
inv
b$get()%*%inv






