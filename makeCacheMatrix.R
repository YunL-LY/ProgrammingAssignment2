makeCacheMarix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y  ## set the value
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}  ## return the frame in matrix

cacheSolve <- function(x, ...){
       inv <- x$getinverse()
       if(!is.null(inv)){
               message("getting cached data")
         return(inv)
       } ## check 
       mat <- x$get()
       inv <- solve(mat, ...)
       inv
}