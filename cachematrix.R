## This function holds the value of a Matrix and a cached value of its inverse. 
## 
## The value of the matrix is stored in 'x' and this can be retrieved or updated using the get or set function
##
## The inverse of the matrix is stored in the cachedInverseMatrix variable. This is inistalised to Null 
## at start up. It also needs to be set to Null when the matrix is set as the previous value is 
## no longer valid. 
##
## The cachedInverseMatrix is set and retrieved using the getinversematrix and setinversematrix functions.
## Note: The setinversematrix function does not inverse the matrix but expects to be passed the 
##      inverse matrix as a parameter. A enhancement would be to have this function inverse the value of 'x' 
##      so we can always be sure the value is an inversematrix of x rather than values the user has passed
##      which may or may not be corrrect.
##
##

makeCacheMatrix <- function(x = matrix()) {

        # initialise cached value to Null
        cachedInverseMatrix <- NULL
        
        # Set x and initialise the cached inverse to null
        # because the previous value is no longer valid
        set <- function(y) {
              x <<- y
              cachedInverseMatrix <<- NULL
        }
        
        
        # Get the value of X 
        get <- function() x

        
        # Set the value of the CachedInverseMatrix
        setinversematrix <- function(matrix) cachedInverseMatrix <<- matrix
        
        # Return the  CachedInverseMatrix
        getinversematrix <- function() cachedInverseMatrix

        # return a list of the callable functions
        list(set = set, 
             get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## The cachesolve function returns an inverse of the supplied matrix.
##
## It is passed a getCacheMatrix variable and calls the getinversematrix function to
## get the cached inverse matrix and check if it is NULL. 
## 
## If the inverse matrix is not null, then this is the cached value and will be returned
##
## If the inverse matrix is NULL, then the inverse matrix hasn't been cached yet. The matrix is
## retrieved, the inverse created using solve() and then stored using 'setinversematrix'.
## The newly created inversematrix is then returned.
##

cacheSolve <- function(x, ...) {
        ## Get the cached copy of the matrix
        m <- x$getinversematrix()
        
        ## if the cached copy is not null, then there is a valid
        ## cached copy. Get it and return it
        if(!is.null(m)) {
            message("getting cached inversematrix")
            return(m)
        }
        
        ## If we have reached here, then the inverse matrix needs to be set
        ## Get the data, create the inverse matrix, cache it and then return it. 
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}

