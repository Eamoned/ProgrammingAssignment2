## this function creates an object that calls the makeCacheMatrix to function. The function definition creates 
## the variable X (assigned a matrix) in the object.The internal variable "m" is set to NULL and lists 4 subfunctions (set(), get(), 
## setcache() and getcache() ) that can be called.

makeCacheMatrix <- function(x = matrix()) {  
      
      m <- NULL               ## sets m(cache), or inverse, to NULL. i.e. a placeholder
      set <- function(y) {    ## defines a function to set the matrix, y, and assigns/stores it in the variable "x" 
            x <<- y           
            m <<- NULL        ## resets the inverse to NULL (clears the cache "m") 
      }
      get <- function() x                           ## returns the matrix stored in x
      setcache <- function(inverse) m <<- inverse   ## Takes the inverse matrix passed into it and stores it in "m" (the cache). 
      getcache <- function() m                      ## returns the inverse, m (the cache)
      list(set = set, get = get,                    ## Lists the 4 subfunctions 
           setcache = setcache,                     
           getcache = getcache)
}

cacheSolve <- function(x) {
      m <- x$getcache()   ## gets the inverse matrix (or cache) and assigns it to local variable "m".
      if(!is.null(m)) {   ## checks if the retuned cache has a value. If there's no values (cache is empty) it jumps to 
            message("getting cached data")   ## the subfunction get(). If there is does have a value then it prints the message 
            return(m)                        ## and returns the cached matrix.
      }
      amatrix <- x$get()   ## gets matrix and assigns it to (local) variable "amatrix".
      m <- solve(amatrix)  ## calculates the inverse and assigns it to local variable "m"
      x$setcache(m)        ## store inverse into the cache 
      m                    ## and return the inverse 
}

## Note: "<<-" allows this subfunction to alter the "m" and "x" variable in the containing environment, i.e. the  makeCacheMatrix function.
## If we've calculated the inverse of the matrix once already, it will be stored in the cache. When the cacheSolve()
## function is run again it grabs the cache, getcache(), and the "if" statement is evaluated to "TRUE"retuning the message
## and the inverse matrix m.
