## These functions are creating that cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse
        inv <- NULL
        ## Set the matrix
        set <- function (matrix){
        mat <<- matrix
        inv << - NULL
        }
        ## Get the matrix
        get <- function(){
        ## Return matrix
        mat
        }
        ## Set the inverse of the matrix
        setInverse <- function(inverse){
        inv <<- inverse
        }
        ## Get inverse of the matrix
        getInverse <- function(){
        ## Return inverse
        inv
        }
        ## List with matrix and inverse
        list(
        set=set, get=get, setInverse = setInverse, getInverse = getInverse
        )
}


## This functioin cumputes the inverse of the special matrix returned by function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse()
        ## Return inverse if mat its already yet
        if(!is.null(m)){
        message("getting cached data")
        return(mat)
        }
        ## Get matrix
        data <- x$get()
        ## Compute the inverse with solve()
        mat <- solve(data) %*% data
        ## Set the inverse
        x$setInverse(mat)
        ## Return matrix
        mat
        }
