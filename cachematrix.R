## Programming Assignment #2
## Chris Croupe
## 01/15/2020

## These functions are used in conjuction to calculate the inverse of a matrix and store that inverse in a 
## cached variable so the inverse calculation does not have to be re-run if the passed matrix inverse has 
## already been calculated - (the same or identical matrix is passed as the last time) 

## makeCacheMatrix generates list of functions that can be called to set the matrix, display the set matrix,
## calculate the inverse of the matrix, display the inverse, and store in inverse in a cached variabe.
## The function also sets a flag if the $set function is called and the inverse must be recalculated

## To test these functions I would pass the sqaure matrix matrix(rexp(100),4,4) but I would change the dimentsions around
## this seemed to work to ensure that the square matrix passed is invertible and made the testing easier since I could just 
##copy the command to make a new matrix since it used random numbers to populate (if I wanted to test with the same matrix
## I would store it in a dummy variable)

makeCacheMatrix <- function(x = numeric) {
        m <- NULL                               ## initialize the variable
        set <- function(y) {                    ## define the "Set" function 
                x <<- y                         ## when $set is called, define the cached inverse matrix as NULL
                m <<- NULL
        }
        get <- function() x                     ## define the "get" function to return the set matrix
        invert <- function(){                   ## define the "invert" function
                z <- M$get()                    ## I had a prolmen running solve on calls with a "$" - so I used this dummy variable
                m <<- solve(z)                  ## inverts the set matrix and stores it in cache
        }
        getInvert <- function() m               ## define the "getInvert" function and returns the inverse of the set matrix
        list(set = set, get = get,              ## names the functions in this function list
        invert = invert, getInvert = getInvert)
}


## cacheSolve checks to see if there is a valid inverse matrix in cache (if "m" is not NULL) and return it if there is
## - else, calculatethe inverse of the new input m

cacheSolve <- function(x, ...) {
        m <- x$getInvert()                      ## Get the cached inverse matrix
        if(!is.null(m)) {                       ## check if the cached inverse matrix is not NULL 
                message("getting cached data")  ## if the inverse matrix is not NULL print a message and return the cached matrix
                return(m)
        }
        data <- x$get()                         ## if the inverse matrix is NULL (a new matrix has been set) 
        m <- solve(data)                        ## get in the inverse of the new matrix
        x$invert()                              ## store the inverse of the new matrix in cache
        x$getInvert()                           ## display the new inverse matrix  
}
