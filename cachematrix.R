## Create a matrix object that stores the original matrix and its "solved
## version", with the intent of caching the solved matrix so that it does not
## need to be recomputed each time it is needed.

makeCacheMatrix <- function(mat = matrix()) {

    matSolved <- NULL

    # Store a matrix in this object and clear the solved matrix.
    # This basically reinitializes the object with a new matrix.
    #
    set <- function(inMat) {
        mat       <<- inMat
        matSolved <<- NULL
    }

    # Retrieve the original matrix from this object.
    #
    get <- function() mat

    # Store the solved matrix in this object.
    #
    setSolved <- function(inMatSolved) matSolved <<- inMatSolved

    # Retrieve the solved matrix from this object.
    #
    getSolved <- function() matSolved

    # Return the function list.
    #
    list(set       = set,
         get       = get,
         setSolved = setSolved,
         getSolved = getSolved)
}


## Solve a matrix using the solve() function. Note that if only the first
## argument is provided, solve() computes the inverse of the matrix. Return the
## solved version, but use the cached version if it is available.

cacheSolve <- function(x, ...) {

    # Return the cached matrix if it exists.
    #
    matSolved <- x$getSolved()
    if (!is.null(matSolved)) {
        message("getting cached data")
        return(matSolved)
    }

    # Invoke solve() on the original matrix.
    #
    matSolved <- solve(x$get(), ...)

    # Cache the solved matrix and return it.
    #
    x$setSolved(matSolved)
    matSolved
}

## Unit Tests

cacheSolveTest <- function() {

    ## Test basic caching.
    ##
    n <- 3
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeCacheMatrix(mat)
    matSolved1 <- cacheSolve(matCached)
    matSolved2 <- cacheSolve(matCached)
    if (!identical(matSolved1, matSolved2))
        message("Cached version does not match solved version")

    ## Use a time test to see if we really save time
    ##
    n <- 128
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeCacheMatrix(mat)
    time1 <- system.time(matSolved1 <- cacheSolve(matCached))
    time2 <- system.time(matSolved2 <- cacheSolve(matCached))
    if (time1["user.self"] < time2["user.self"])
        message("Solve time is less than cache time")

    ## Compute machine epsilon (floating precision)
    ## This should be about 2^-53 on most machines.
    ##
    eps <- 0.5
    while(1.0 - eps != 1.0)
        eps <- eps / 2.0
    eps <- eps * 2.0

    ## This really just tests to see if the matrix inverse is computed
    ## correctly, which is more of a test of the solve() function. Multiply the
    ## original matrix by the inverse.  Subtract the identity and the result
    ## should be a zero matrix, within the limits of machine precision.
    ##
    ## Note that the allowable error will be much more than machine epsilon due
    ## to the large number of operations in solve().  We just take a guess here,
    ## since all we are doing is looking for a result close to zero.  The
    ## allowable error should be porportional to the size of the matrix, and
    ## hence, the number of operations.
    ##
    n <- 20
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeCacheMatrix(mat)
    matSolved <- cacheSolve(matCached)
    tmp <- mat %*% matSolved
    tmp <- abs(tmp - diag(n))
    b <- tmp > (.Machine$double.eps * n * 64)
    if (sum(b) != 0)
        message("Incorrect matrix inversion")

    ## Test some of the internals
    ##
    if (!identical(mat, matCached$get()))
        message("get() not working")
    if (!identical(matSolved, matCached$getSolved()))
        message("getSolved() not working")
    n <- 5
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached$set(mat)
    if (!identical(mat, matCached$get()))
        message("get() after set() not working")
    if (!is.null(matCached$getSolved()))
        message("getSolved() after set() not working")
}