makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y     #set matrix x to a new matrix y
                m <<- NULL  #resets the inverse to null
        }
        get <- function() x #returns matrix x
        setinverse <- function(solve) m <<- solve #sets the inverse m, to solve
        getinverse <- function() m #returns the inverse, m
        list(set = set, get = get, #returns a special matrix
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) { #calculates the inverse of the special matrix
        m <- x$getinverse()
        if(!is.null(m)) { #checks if inverse has already been calculated
                message("getting cached data") #if already been calculated, gives a message
                return(m) #and return the already-calculated inverse from the cache and skips the computation
        }
        data <- x$get() 
        m <- solve(data, ...) #Otherwise, it calculates the inverse
        x$setinverse(m) #and sets the value of inverse in the cache via the setinverse function
        m
}