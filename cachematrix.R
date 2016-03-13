## The functions will calculate matrix inversion whenever the result is not cached

## makeCacheMatrix is a list of functions
## which let "save" values on cache
makeCacheMatrix <- function(x = matrix())
{
    ## asigns NULL to inv 
    inv = NULL
    
    ## 1) set: Replaces x for y in the original fx. Assigns null to inv
    set<-function(y)    
    {
        x<<-y
        inv = NULL
    }
    
    ## 2) get: Gets x value
    get<- function() x
    
    ## 3) setinv: Assigns the inveted matrix to inv in the original fx environment
    setinv<- function(inversa)
    {
        inv<<-inversa
    }
    
    ## 4) getinv: gets the inverted matrix cached result
    getinv<- function() inv
    
    list(set=set,get=get,setinv=setinv,getinv=getinv)
    
}


## Search for the inverted matrix in cache if it doesn't find it, then is calculated.

## x is the result cache list from de makeCacheMatrix.R
## ... optional arguments for the solve function
cacheSolve <- function(x,...) {
    
    ## gets the cached result
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        ## If exists just returns the result and end the function
        return(inv)
    }
    ## If it doesnt find it:
    ## Gets the original matrix
    data <- x$get()
    
    ## inverts the matrix
    inv <- solve(data)
    
    ## Saves the result in cache
    x$setinv(inv)
    
    ## prints the result
    inv
}
