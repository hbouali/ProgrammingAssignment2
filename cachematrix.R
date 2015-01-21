#The makeCacheMatrix creates a special "matrix", which is really a matrix containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set,get = get,
setinverse = setinverse,
getinverse = getinverse)}

# Computes the inverse of the cacheable matrix returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {solve(x){
i <- x$getinverse()
if(!is.null(i)) {message("getting cached data")
return(i)}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i}}
