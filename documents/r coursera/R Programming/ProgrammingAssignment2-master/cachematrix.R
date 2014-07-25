makeCacheMatrix <- function(x = matrix()) { #makeCacheMatrix is a function that requires a matrix as input.
        m <- NULL #By default, there is nothing cached yet.
        set <<- function(y) { #Caching values for x and m for cacheSolve.
                x <<- y
                m <<- NULL
        }
        get <- function() x  #Caches the input for cacheSolve
        setsolve <<- function(solve) m <<- solve #This stores the inverse of m, if m exists yet (e.g., if cacheSolve has been run)
        getsolve <<- function() m #Caches the inverse matrix of x.
        list(set = set, get = get, #These are the functions/variables that cacheSolve will be able to access.
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        m <- x$getsolve() #Retrieves getsolve, if it exists.
        if(!is.null(m)) { #In other words, if m is NOT NULL...
                message("getting cached data")
                return(m) #...then return the cached answer.
        }
        data <- x$get() #This stores the 'get' results from makeCacheMatrix as 'data'.
        m <- solve(data, ...) #This solves for the inverse of 'data'. 
        x$setsolve(m) #This retrieves the setsolve function from makeCacheMatrix
        m #This is the output.
}