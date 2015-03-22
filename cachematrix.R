## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates environment variables to use it for cache.

makeCacheMatrix <- function(inverseMatrix = matrix()) {
    inv_matrix <- NULL
    print(environment())
    evn <- environment()
    print(parent.env(evn))
    set <- function(y) {
        inverseMatrix <<- y
        inv_matrix <<- NULL
    }
    get <- function() inverseMatrix
    setinverse <- function(inverseMatrix) inv_matrix <<- inverseMatrix
    getinverse <- function() inv_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         
    if(!is.null(inverseMatrix)) {
        print("getting cached data - inverted matrix ")
        return(inverseMatrix$getinverse())
    }else{
        print("couldn't get from cache...")
    }
    inv_matrix <- solve(x)
    ##set it in the environment for future use. 
    inverseMatrix <-makeCacheMatrix(inv_matrix)
    inverseMatrix$setinverse(inv_matrix)
    return(inv_matrix)   
}
