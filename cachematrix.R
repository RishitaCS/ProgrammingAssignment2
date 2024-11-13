# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ##initialize the inverse matrix
    invMatrix <- NULL
    
    # set the matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    # get the matrix
    get <- function() {
        x 
    }
    
    # set the inverse
    setInvMatrix <- function(invM){
        invMatrix <<- invM 
    } 
    
    # get the inverse
    getInvMatrix <- function(){
        invMatrix
    }
    
    # Return a list of the functions defined
    list(set = set,
         get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Try to get the cached inverse
    invM <- x$getInvMatrix()
    
    # If the inverse is already cached, return it
    if (!is.null(invM)){
        if ( identical( x$get() %*% invM, invM %*% x$get() ) ){
            print("getting cached data")
            return(invM)
        }
    }
    
    # If not cached, compute the inverse, cache it, and return it
    data <- x$get()
    invM <- solve(data, ...)
    x$setInvMatrix(invM)
    
    ## Return a matrix that is the inverse of 'x'
    print("getting new computated data")
    return(invM)
}
