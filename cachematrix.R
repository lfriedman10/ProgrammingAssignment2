##These two functions are the solution to Programming Assignment 2: Lexical Scoping.


# The first function, makeCacheMatrix, takes matrix input and
# creates a special "vector", which is really a list containing a function to
# set the value of the input matrix
# get the value of the input matrix
# set the value of the Inverse
# get the value of the Inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# The following function calculates the inverse of the special "vector" 
# created with the above function. However, it first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the 
# inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached Inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

