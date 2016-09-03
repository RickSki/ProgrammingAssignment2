## makeCacheMatrix stores a matrix and returns a list of functions,
## cacheSolve tests for and return the cached inverse of x or 
## solves for and returns if the cache is empty

makeCacheMatrix <- function(x = matrix()) {
        ## define m inside the function
        m <- NULL
        ## assign function output to set and assign values in parent env
        set <- function(y) {
                # assign the input to x in the parent env
                # define m in the parent environment
                x <<- y
                m <<- NULL
        }
        ## assign function with output type x to get
        get <- function() x
        ## assign function of parameter and type inverse(matrix) to setinverse
        setinverse <- function(inverse) m <<- inverse
        ## assign function of type m to getinverse
        getinverse <- function() m
        ## make list of previous assignments
        return(list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse))
}
## cacheSolve tests if there is a value in m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        ## assign the getinverse element of x to m 
        m <- x$getinverse()
        ## if m is not null, return the contents of m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## assign makeCacheMatrix$get to data 
        data <- x$get()
        ## assign solve(data) to m
        m <- solve(data, ...)
        ## calculate function (inverse)
        x$setinverse(m)
        return(m)
}
