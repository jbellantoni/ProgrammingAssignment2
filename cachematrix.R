## The functions provide a fast way of accessing data, so that to find the inverse of a matrix you do the 
## calculation once and then can load that data every subsequent time. The first function takes a matrix and  
## stores it and an empty object of its inverse, while creating functions to share the data with other 
## environments to calculate and save the inverse of the matrix. The second function performs this calculation 
## and stores the result, so that when you call the function it returns the cached value instead of doing another 
## calculation. 

## This function takes the input of a matrix and initializes inv as null, creating an empty field. It stores the 
## matrix and the inverse matrix, while creating four functions (set, get, setinv, and getinv) to share access to 
## the matrix and inverse matrix with external environments.The result is a list of the four functions. 

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
      x<<-y
      inv<<-NULL
    }
    get<-function() x
    setinv<-function(s)inv <<-s
    getinv<-function() inv
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
    }


## This function retrieves the value of getinv from the parent environment. If the value exists the function will
## return both the message "getting cached data" and the value. If the value does not exist, it will retrieve the
## the matrix value from the get function of makeCacheMatrix, calculate the inverse, send the value of the 
##inverse to the parent environment via the setinv function in makeCacheMatrix, and return the value of the
##inverse matrix.

cacheSolve <- function(p, ...) {
    k<-p$getinv()
    if(!is.null(k)) {
        message("getting cached data")
        return(k)
    }
    data<-p$get()
    k<-solve(data,...)
    p$setinv(k)
    k
        }
        ## Return a matrix that is the inverse of 'x'
