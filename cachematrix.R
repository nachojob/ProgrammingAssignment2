## The following two functions work together to establish a way
## of making the value of a given inverse matrix available, once it is calculated 
## for the first time.


## makeCacheMatrix function transforms the original matrix in an object that both contains
## the original values of the matrix and a variable prepared to later store its 
## calculated inverse matrix.
## Apart from that, the object created expose four functions for getting or setting
## these values mentioned above. 


makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL ## Initialize the variable to later store the inverse matrix.
  
  set <- function(y) { ## Function for assigning the original matrix and the inverse matrix 
                       ## to the variables that store them.
    x <<- y
    mx <<- NULL
  }
  get <- function() x ## Function for providing the original matrix when invoked.
  
  setinv <- function(inv) mx <<- inv ## Function for assigning the calculated inverse matrix 
                                     ## to the variable that stores it.
  
  getinv <- function() mx ## Function for providing the calculated inverse matrix when invoked.
  
  list(set = set, get = get, ## List created and then populated by invoking 
       setinv = setinv,      ## these 4 functions declared above.
       getinv = getinv)
}


## cacheSolve take an object of the type created by makeCacheMatrix and check it to see
## if its inverse matrix has already been calculated. If a cache value is found, the function
## returns this value, otherwise it calculates the inverse matrix and returns it.

cacheSolve <- function(x, ...) { ## x has to be a list of the type created by makeCacheMatrix.
  mx <- x$getinv() ## Get the value of the inverse matrix cache in the x object and stores it in mx.
  if(!is.null(mx)) { ## Inspect mx. If it's NOT null returns its value and cacheSolve ends.
    message("getting cached data")
    return(mx)
  } ## When mx IS null, the inverse matrix has to be calculated for the first time by the next steps:
  data <- x$get()  ## Getting the original matrix from the x object and storing it in data.
  mx <- solve(data, ...) ## Calculating the inverse matrix and storing it in mx.
  x$setinv(mx) ## Storing the inverse matrix (mx) in the object x by calling the setinv function.
  mx ## Returning the inverse matrix.
}
