## makeCacheMatrix creates an object from a matrix. This object has
## 1 property (m_inverse) and 4 methods: set, get, setinverse, getinverse
## cacheSolve is a function that receives a makeCacheMatrix object as input
## checks if it has the inversed matrix stored. If it doesn't, it calculates it
## returns it and then stores it (caches it) in the object so next time it is
## requested it doesn't have to calculate it again.


## makeCacheMatrix property:
## m_inverse: NULL as default, it stores the inversed matrix
## makeCacheMatrix methods:
## (1) set: receives a matrix and saves it in the returning object. Also
## assigns NULL to the inverse matrix as it hasn't been calculated yet.
## (2) get: returns the stored matrix
## (3) setinverse: it receives and inversed matrix and stores it in the object.
## (4) getinverse: returns the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m_inverse <<- solve
  
  getinverse <- function() m_inverse  
  
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve function receives a makeCacheMatrix object and checks
## if it has its inverse matrix already stored or if it's NULL.
## If it is stored it returns it from the value that it has been stored already
## If it's not (receiving a NULL value) then it calculates the inverse matrix
## stores it in the object and returns the inverse matrix.

cacheSolve <- function(x, ...) {
    m_inverse <- x$getinverse()
    if(!is.null(m_inverse)) {
      message("getting cached matrix")
      return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinverse(m_inverse)
    m_inverse

}