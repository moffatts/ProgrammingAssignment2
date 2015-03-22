## Calculating the inverse of a matrix can require a lot of computing power. If a matrix doesen't change
## often it may be better to caculate a matrix's inverse and store it for retrieval by other functions.

## This function creates a matrix object whose inverse we can calculate and store for other functions' use.

CacheMatrix <- function(x = matrix()) {
        
                cache <- NULL
                set <- function(y) {
                        x <<- y
                        cache <<- NULL
                }
                
#list of functions to set and retrieve the cached matrix.               
                get <- function() x
                setcache <- function(solve) cache <<- solve
                getcache <- function() cache
                list(set = set, get = get,
                     setcache  = setcache,
                     getcache = getcache)
        }
        

## This function returns the inverse of the input to the CacheMatrix function above. It checks if the 
## inverse has already been calculated. If the inverse has already been calculated it is returned. If not,
## the function calculate the inverse, stores it and returns it. 

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        cache <- x$getcache()

##checks if an there is already a cached object and returns it if there is.         
        
                if(!is.null(cache)) {
                        message("getting cached data")
                        return(cache)
                }
##if there isn't a cached inverse matrix uses solve function to create  one (n_cache) and stores and
## returns it's value.
                data <- x$get()
                n_cache <- solve(data, ...)
                x$setcache(n_cache)
                return(n_cache)
        }

