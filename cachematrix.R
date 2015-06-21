makeCacheMatrix <- function(x = matrix()) 
    ## Creates a special lists of function to invert a matrix and keep it cached.
    ## The result of this function and its enviroment could be stored to prevent
    ## for calculate the invert matrix everytime we need it.
    ##
    ## Args:
    ##   x: The matrix to be inverted
    ##
    ## Returns:
    ##   A list with 4 functions:
    ##        - set: the function to set the matrix to be cached
    ##                Args: y: the matrix
##        - get: returns the cached matrix
##                Args: None
##        - setInverse: Sets the inverse of a matrix to cache
##                Args: The value of the inverse
##        - getInverse: Returns the value of the inverse of a matrix, if exists.
##                Args: None
{
    m <- NULL # Sets the value in the functions enviroment
    set <- function(y) 
    {
        x <<- y
        m <<- NULL # Sets the value in the parent enviroment
    }
    get <- function() x # Returns the matrix value
    setInverse <- function(inverse) m <<- inverse # Sets the inverse matrix
    getInverse <- function() m # Returns the inverse matrix (if exists)
    list(set = set, get = get, # List of functions returned
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...)
    ## Calculates the inverse of a matrix, using the environment from "makeCacheMatrix"
    ## function. If the value was calculated yet, it prints a message and
    ## returns the cached value; if not, it calculates the inverse and stored it.
    ##
    ## Args:
    ##   x: A lists of functions and its environment from "makeCacheMatrix"
    ##    ...: Addicional arguments if needed
    ## Returns:
    ##    An inverted matrix (stored in the "makeCacheMatrix" environment)
{
    m <- x$getInverse() # Try to get the inverse matrix if exists
    if(!is.null(m)) 
    { # If the inverse exists, we can return it without making any calculation
        message("Getting cached data")
        return(m)
    }
    data <- x$get() # If there is no inverse, we get the matrix
    m <- solve(data, ...) # ...and calculate the inverse
    x$setInverse(m) # When we have the inverse, we strored it
    m
}