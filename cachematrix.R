## Programming Assignment 2 for "R Programming"
## Uses lexical scoping to cache the inverse of a matrix once it is computed.

## This function creates a wrapper for a matrix that is able to cache its inverse
## The returned value is a list of getter and setter functions for the matrix itself
## as well as its inverse.

makeCacheMatrix <- function(x = matrix())
{
    #cache for inverse of matrix
    xinv <- NULL
    #getter and setter functions for the matrix object itself
    get <-  function() {x}
    set <-  function(y)
            {
                x<<-y
                xinv <<- NULL;
            }
    
    #getter and setter functions for the inverse matrix
    getinv <- function() {xinv;}
    setinv <- function(inv) {xinv <<-inv}
    
    invisible(list(set=set, get=get, setinv = setinv, getinv=getinv))
}


## This function computes the inverse of a given matrix. x must be a list as 
## returned by the makeCacheMatrix function. If an inverse of the matrix was
## already computed, a cached value is returned.

cacheSolve <- function(x, ...)
{
        inverse <- x$getinv();
        if(!is.null(inverse))
        {
            message("Returning cached inverse matrix.")
            inverse;
        }
        else
        {
            message("Performing matrix inversion.")
            inverse <- solve(x$get(),...)
            x$setinv(inverse)
            inverse
        }
}


##generation of a random matrix of specified dimensions for test purposes
## mean, sd: mean and standard deviation of underlying normal distribution
## nrow, ncol: dimensions of the matrix
randmatrix <- function(mean=0, sd=1, nrow=1, ncol=1,...)
{
    ndat <- nrow*ncol;
    dat <- rnorm(ndat, mean, sd)
    matrix(data=dat, nrow=nrow, ncol=ncol)
}
