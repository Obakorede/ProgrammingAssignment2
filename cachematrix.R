## A function that creates a special matrix objet that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## List for input to function cacheSolve()
        inv <- NULL
        set <- function(y){
        ## assign values to objects in another environment
        x <<- y
        inv <<- NULL
        }
        ## get the value of matrix
        get <- function() x
        ## set the value of inverse matrix
        setinv <- function(inverse) inv <<- inverse
        ## get the value of inverse matrix
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## A function that that computes the inverse of a special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        ## in cases where the inverse has already been calculated
        if(!is.null(inv)){
                ## obtain object from the cache instead of calculating again
                message("Obtaining cached data")
                return(inv)
        }
        ## calculate inverse
        mat.data = x$get()
        inv <- solve(mat.data, ...)
        
        ## inverse value setting
        x$setinv(inv)
        
        return(inv)
}
