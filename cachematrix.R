## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix and cachesolve

##makeCacheMatrix consists of set get setInv getInv
##library(MASS) is used to calculate inverse for non squared as well as square matrices 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL        #initialising  inverse as NULL 
  set <- function(y){
                    x <<- y
                    inv <<- NULL
                    }
 
   get <- function(){x}    #function to get matrix x
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
#This is used to get each of the data

cacheSolve <- function(x,...) {
                               inv <- x$getInverse()
                               if(!is.null(inv)){
                                 message("getting cached data")
                                 return(inv)
                               }
                               mat <- x$get()
                               inv <- solve(mat,...)
                               x$setInverse(inv)
                               inv
                               
                               
}
