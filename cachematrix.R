## The first function creates an environment to which the inverse of a given matrix is assigned, along with the matrix. e.g mymatrix<-makeCacheMatrix(x)
##The second function calculates the inverse of this matrix and assigns it to 'i' within the cache. If this inverse has already been calculated, i will be retrieved from the cache and printed, saving processing power. e.g cacheSolve(x)

## This function will create a cache for a matrix, allowing the inverse of the matrix to be assigned to memory. This will allow cachesolve to retrieve the inverted matrix from the cache, without needing to recall solve(x), thus saving memory.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #Creates an object to which inverse will later be assigned by cacheinverse. Lexical scoping allows nested functions to access this object in the parent environment.
  
  set <- function(y){ #This sets the new value of x and clears the previous inverse calculated
    x <<- y
    i <<- NULL
  }
  get <- function() x #This retrieves the current value of x
  setinverse <- function(inverse) i <<- inverse #assigns an input value to i in parent environment
  getinverse <- function() i #prints value of i from parent environment using lexical scoping
  list(set=set, get=get, getinverse=getinverse, setinverse=setinverse) #assigns above functions to a named list, allowing the $ symbol to be used when retrieving them.
}


## Solves the inverse of the matrix given and assigns it to 'i' within makeCacheMatrix environment. If the matrix has already been inverted, the function will instead retrieve this value of 'i' from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #sets i to the cached value
  if(!is.null(i)){ #checks to see if i has been calculated in the cache. 
    message("getting cached data") #prints i from the cache only if not null.
    return(i)
  }
  data <- x$get() #if i is null, retrieves the matrix from cache
  i <- solve(data, ...) #solves the inverse of the matrix and assigns to i
  x$setinverse(i) #sets the new value of i within the cache
  i #prints inverse value of matrix
}
