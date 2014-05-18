## creates a function containing 4 functions, set sets the value of the original 
##matrix, get retrives value of orginal matrix , getinverse retrives inverse 
##value from cache, setinverse receives inverse value from cachesolve and stores
## it in cache
makeCacheMatrix <- function(x = matrix()) { ##creates matriz
        m <- NULL ## creates empty cache variable m
        set <- function(y) {   ## Set value of matrix
                x <<- y        ## assigns passes value to x
                m <<- NULL     ## clears cache
        }
        get <- function() x    ##gets the value of x
        setinverse <- function(solve) m <<- solve  ##calculates matrix inverse and caches it
        getinverse <- function() m                 ## retrives inverse from cache
        list(set = set, get = get,                 ## creates a list containing each function
             setinverse = setinverse,
             getinverse = getinverse)
}
## calculates the inverse of a matrix and stores it to cache.  Check wether 
## inverse has been calculated before calculating it
cacheSolve <- function(x, ...) {
        m <- x$getinverse()   ## retrives cached value 
        if(!is.null(m)) {     ## if inverse exists retrives from cache and displays it
                message("getting cached data")
                return(m)
        }
        data <- x$get()   ## get matrix from cache
        m <- solve(data)  ## calculates inverse
        x$setinverse(m)   ## passes inverse to cache
        m                 ## displays value of cache
}