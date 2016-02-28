makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


# > x <- makeVector(x = 1:10)
# > x$get()
# [1]  1  2  3  4  5  6  7  8  9 10
# > x$getmean()
# NULL
# > cachemean(x)
# [1] 5.5
# > x$getmean()
# [1] 5.5