## The function makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){ # Matrix to be provided as i/p
  m <- NULL                                # When function called without a specific subfunction
  set <- function(y) {                     # Sets values of x and m in the parent function
    x <<- y
    m <<- NULL                             # Clearing the cache
  }
  get <- function() x                      # Returns value stored in x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the matrix
##  it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the matrix inverse in the cache via the solve function.

cacheSolve <- function(x, ...){           # Ellipsis to let us pass extra parameters for the solve function
  m <- x$getinverse()
  if(!is.null(m)) {                       # If the matrix inverse has been cached previously
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)                   # Calculating matrix inverse
  x$setinverse(m)
  m
}


############# Test Cases provided by ##############################################

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getinverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinverse()  # Returns matrix inverse

