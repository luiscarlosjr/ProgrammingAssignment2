# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   These functions are able to compute the inverse of a matrix in a efficient  #
#   manner, speccially when multiple calls (in a loop) are realized. By caching #
#   (and recovering from cache), no unecessary time consuming calculations are  #
#   done.                                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

##
# makeCacheMatrix FUNCTION
# USAGE: x_matrix <- makeCacheMatrix(x)
# DESCRIPTION: Creates a list holding a set of four functions aimed at the
#               cached solution of the inverse of 'x'. 
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # Initialize the inverse matrix as 'NULL'
    
    ##
    # SET SUBFUNCTION
    # USAGE: x_matrix$set(new_x)
    # DESCRIPTION: Changes the input and resets the inverse matrix stored in 
    #               the 'makeCacheMatrix' function
    ##
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ##
    # GET SUBFUNCTION
    # USAGE: x_matrix$get()
    # DESCRIPTION: Returns the matrix stored in the 'makeCacheMatrix' function.
    #               No input value is required for this subfunction.
    ##
    get <- function() x
    
    ##
    # SETSOLVE SUBFUNCTION
    # USAGE: x_matrix$setsolve(new_inverse_of_x)
    # DESCRIPTION: Stores the inverse matrix in the 'makeCacheMatrix' function.
    #               *This subfunction should be used with care as it will tamper
    #               the inverse calculation result.*
    ##
    setsolve <- function(solve) m <<- solve
    
    ##
    # GETSOLVE SUBFUNCTION
    # USAGE: inverse_x_matrix <- x_matrix$getsolve()
    # DESCRIPTION: Returns the matrix stored in the 'makeCacheMatrix' function  
    #               as the inverse matrix. No input value is required for this subfunction.
    ##
    getsolve <- function() m
    
    # Returning a list of the subfunctions to the 'x_matrix' object
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

##
# CacheSolve FUNCTION
# USAGE: cacheSolve(x_matrix)
# DESCRIPTION: Returns the inverse of the 'x_matrix' object created with 
#               'makeCacheMatrix'. If the inverse was already calculated, it's
#               cached value will be returned and the unecessary calculation
#               avoided.
##
cacheSolve <- function(x, ...) {
    
    # Testing the existence of a cached solution
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Performing calculation of the inverse and caching the result
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
