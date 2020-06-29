## These functions are creating for caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Assuming that matrix to be invertible
        ## Initialize of matrix+
        invMatrix <- NULL
        ## Define the set for the matrix
        set <- function(y){
        x <<- y
        invMatrix <- NULL
        }
        ## Define the return matrix object by defining it as get
        get <- function(){x ## return special matrix}
        ## Setting the inverse matrix
        setInverse <- function(inverse){invMatrix <<- inverse }
        ## Define function for returning the inverse matrix
        getInverse <- function(){inverseMatrix}
        ## Retunr the list for future call within the environment
        list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        ## Verified if the inverse is not NULL
        if(!is.null(invMatrix)){
                ## Verified if the invMatrix es identical
                if ( identical( x$get() %*% invM, invM %*% x$get() ) ){
                       print("getting cached data")
                       return(inverseMatrix)
                        }
                }
        ## Compute the inverse if inverse matrix is change or null
         data <- x$get()
        inverseMatrix <- solve(data, ...)
        ## set the value of the inverse
        x$setInverse(inverseMatrix)
        ## Return inverse matrix
        print("getting computeded data")
        return(inverseMatrix)
}
