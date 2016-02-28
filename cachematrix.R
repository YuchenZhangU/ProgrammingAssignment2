## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  build a function environment to store/cache the data and inverse of a matrix
#  get() getinv() provide interface to get the matrix data and matrix inverse(if it has already been cached)
#  set()  setinv() are used to set the matrix data and inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        
}

## --example
# > x=matrix(c(1,2,2,3),2,2)
# > mat <- makeCacheMatrix(x)
# > mat$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    2    3
# > mat$getinv()
# NULL
# > mat$setinv(solve(mat$get()))
# > mat$getinv()
# [,1] [,2]
# [1,]   -3    2
# [2,]    2   -1



## Write a short comment describing this function
# calculate the inverse of the special "matrix" created with the above function
# if the mean has already been calculated, it gets the mean from the cache and skips the computation.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        inv <- solve(x$get(),...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}


## --example
# > x=matrix(c(1,2,2,3),2,2)
# > mat <- makeCacheMatrix(x)
# > mat$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    2    3
# > mat$getinv()
# NULL
# > cacheSolve(mat)
# [,1] [,2]
# [1,]   -3    2
# [2,]    2   -1
# > mat$getinv()
# [,1] [,2]
# [1,]   -3    2
# [2,]    2   -1