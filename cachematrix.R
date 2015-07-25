##These two functions are the solution to Programming Assignment 2: Lexical Scoping.


# The first function, makeCacheMatrix, takes matrix input and
# creates a special "vector", which is really a list containing a function to
# set the value of the input matrix
# get the value of the input matrix
# set the value of the Inverse
# get the value of the Inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set the value of the input matrix
        set <- function(y) {
                x <<- y # note the use of the <<- operator
                        # which is used to assign a value to an object
                        # in an environment that is different from the current environment
                m <<- NULL
        }
        # get the value of the input matrix
        get <- function() x
        #set the value of the inverse using solve
        setInverse <- function(solve) m <<- solve
        #get the value of the inverse
        getInverse <- function() m
        #return a list of functions
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
        # if successful, let user know we are getting our answer from cache
        # and return from function
        if(!is.null(m)) {
                message("getting cached Inverse data")
                return(m)
        }
        # if we couldn't find the inverse in cache, compute it for the first time
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        #return inverse
        m
}

