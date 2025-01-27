## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {   #define x to be a blank matrix, inv_matrix to be NULL value
  inv_matrix <- NULL                          
  set <- function(y) {                        #set function defines user argument y (a matrix) to be x and reset inv_matrix to be NULL
    x <<- y 
    inv_matrix <<- NULL #
  }
  get <- function() x                         #get function catches x matrix (which was defined by user through set function)
  setinv <- function(inv) m <<- inv           #setinv function defines m to be inverse function
  getinv <- function() m                      #getinv function gets m (which was defined earlier to be an inverse function)  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()             #calculate the inverse of a matrix and store to m
    if(!is.null(m)) {           # if m already exists, then return m 
      message("getting cached data")
      return(m)
    }
    data <- x$get()             # if m doesn't exists, then get the user defined matrix and store to data 
    m <- inv(data, ...)         # calculate the inverse value
    x$setinv(m)                 # set the value to be m
    m                           # return m    
  }
