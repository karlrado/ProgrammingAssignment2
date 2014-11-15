## Create a matrix object that stores the original matrix and its inverse, with
## the intent of caching the inverse so that it does not need to be recomputed
## each time it is needed.

makeCacheMatrix <- function(x = matrix()) {

    matInv <- NULL
    
    # Store a matrix in this object and clear the inverse.
    # This basically reinitializes the object with a new matrix.
    #
    set <- function(inMat) {
        x <<- inMat
        matInv <<- NULL
    }
    
    # Retrieve the original matrix from this object.
    #
    get <- function() x
    
    # Store the matrix inverse in this object.
    #
    setInv <- function(inMatInv) matInv <<- inMatInv
    
    # Retrieve the matrix inverse from this object.
    #
    getInv <- function() matInv
    
    # Return the function list.
    #
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Return the inverse of a cached-matrix object, but use the cached inverse if
## it is available.

cacheSolve <- function(x, ...) {
    
    # Return the cached inverse if it exists.
    #
    matInv <- x$getInv()
    if (!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    
    # Get the original matrix and compute its inverse.
    #
    mat <- x$get()
    matInv = solve(mat, diag(nrow(mat)))
    
    # Cache the inverse and return it.
    x$setInv(matInv)
    matInv
}
