## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
            x<<-y
            inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv <<- inverse
        getinverse<-function() inv

        list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()           
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)                 
    }
    
    mat <- x$get()                   
    inv <- solve(mat, ...)           
    x$setinverse(inv)                
    inv
}

## Testing
m  <- matrix(c(1, 2, 3, 4), 2, 2)
cm <- makeCacheMatrix(m)

cacheSolve(cm)   # computes inverse
cacheSolve(cm)  
