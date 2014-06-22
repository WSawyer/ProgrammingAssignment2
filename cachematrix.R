## This will create a custom matrix and give us a list of our new functions


makeCacheMatrix <- function(x = matrix()){
        
        inv <- NULL
        
        set <- function(y){                                 ## setter for the matrix 
                
                x <<- y
                
                inv <<- NULL
        }
        
        get <- function() x                                 ## get matrix
        
        setInverse <- function(inverse) inv<<- inverse      ## set inverse matrix
        
        getInverse <- function() inv                        ## get inverse matrix
        
        list(set = set, get = get,                          ## return our new lists
             
             setInverse = setInverse,          
             getInverse = getInverse)                  
}


## Compute the inverse of our matrix, if the inverse is already cached it will return the cached inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()               ## Query the x matrix's cache
        
        if(!is.null(inv)){                  ## If inv !equal null it has been cached
                
                message("getting cached data")
                
                return(inv)                         ## Return inv 
                
        } else {                                    ## If inv equals null continue
                
                data <- x$get() 
                
                inv <- solve(data, ...)             ## calculate the inverse of the matrix
                
                x$setInverse(inv)                   ## Cache the new inverse
                
                return(inv)                         ## Return inv
                
        }
        