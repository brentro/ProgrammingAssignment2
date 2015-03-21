## The following functions create, store, recall a matrix as well as it's cache 
## to supply the inverse of a matrix.

## This function sets then retrives the value of the matrix and the sets and gets 
## the inverse value of the matrix

makeCacheMatrix = function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inv <<- inverse
        getinverse = function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##  Under the assumption that the matrix is always invertible, this function uses
##  the matrix created by makeCacheMatrix and formulates the matrix's inverse.
##  Then cacheSolve first checks to see if the inverse has already been formulated.
##  If the inverse was caculated the result is displayed thus negating the
##  computation.  If the inverse was not created then it creates the inverse,
##  stores th inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("obtaining data cache.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

