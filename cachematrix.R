
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## setting value of the vector
        set <-function(y){
                x <<- y
                inv <<-NULL
        }
        ## getting value of vector
        get = function() x
        ## setting value of inverse
        setinv = function(inverse) inv <<- inverse
        ## getting value of inverse
        getinv = function() inv
        ## naming elements
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function tests whether the inverse of the given matrix has already been cache
## if not then it finds the inverse using solve.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Here we are pulling the inverse from cache (ie. inv enclosed within x)
        inv = x$getinv()
        ## Now we test to see whether the inverse has already been cached
        ## (i.e. a non-null inverse exists) and then we return it if it has
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ## If the inverse has not already been cached then here we use solve to find the
        ## inverse of the matrix delivered to the function
        mat.data = x$get()
        inv = solve(mat.data, ...)
        ## Setting the value of the inverse in the cache
        x$setinv(inv)
        return(inv)
}


