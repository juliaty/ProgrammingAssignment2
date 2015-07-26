# Below is a pair of functions that will cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## x: a square invertible matrix
        ## return: a list containing a function to: 
        ##   1. set the matrix
        ##   2. get the matrix
        ##   3. set the inverse
        ##   4. get the inverse
  
        inv <- NULL
        set <- function(y) {
          # <<- operator can be used to assign a value to an object in an environment 
          # that is different from the current environment
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## x: list from output of makeCacheMatrix()
        ## Return: a matrix that is the inverse of original matrix
  
        inv <- x$getinv()
        
        # if the inverse has already been calculated
        if(!is.null(inv)) {
          # return the inverse matrix value from cache
          message("getting cached data")
          return(inv)
        }
        
        # calculate the data (to get inverse matrix)
        data <- x$get()
        inv <- solve(data, ...)
        # set the inverse matrix to cache
        x$setinv(inv)
        inv
}

