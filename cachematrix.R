 
        ## create matrix object to store cached inverse
        
        makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                
                ## set current matrix
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
        ## retrieves matrix
                get <- function() x
        ## set inverse manually
                setinverse <- function(inv) i <<- inv
         ## get inverse of matrix
                getinverse <- function() i
                list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
                
}


## computes inverse of function, if matrix is not computed already (!is.null)
        ## if already computed, retrieves inverse from cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
                
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
## Return a matrix that is the inverse of 'x'
}

