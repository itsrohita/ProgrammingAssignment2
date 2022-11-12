## Functions work together to efficiently compute the inverse of the matrix 

## creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversed_matrix <- NULL
    
    set <- function(y) {
      x <<- y
      inversed_matrix <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inversed_matrix <<- inverse
    
    getinverse <- function() {
      solve(x)
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## if inverse of matrix 'x' has already been computed, returns inverse from  
## cache, otherwise, computes inverse of matrix 'x'

cacheSolve <- function(x, ...) {
    inversed_matrix <- x$getinverse()
    
    if(!is.null(inversed_matrix)) {
      message("getting cached data")
      return(inversed_matrix)
    }
    
    data <- x$get()
    inversed_matrix <- solve(data, ...)
    x$setinverse(inversed_matrix)
    
    inversed_matrix
}
