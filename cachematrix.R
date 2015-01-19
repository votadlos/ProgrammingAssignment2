##
## This is Programming Assignment 2 in "R Programming" cource on Courcera (class.coursera.org/rprog-010)
##

## Usage example:
# > x <- genInvMatrix(5)
# > x
#            [,1]       [,2]       [,3]      [,4]       [,5]
# [1,]  -11.61753   -2.61608   19.93605 112.18184  133.89640
# [2,] -115.71936  197.42789   36.12571  77.44595   41.48448
# [3,]  -56.25516 -160.40744  -13.49598  51.61160  -11.92324
# [4,]  -80.49301   22.84797 -217.43267 -41.38356  -68.54745
# [5,]   42.03699  -49.34163   11.81965 -15.38977 -139.71111
# > cm <- makeCacheMatrix(x)
# > xinv <- cacheSolve(cm)
# > xinv %*% x
#               [,1]          [,2]          [,3]          [,4]          [,5]
# [1,]  1.000000e+00 -5.551115e-17 -1.387779e-17 -2.359224e-16 -3.330669e-16
# [2,]  0.000000e+00  1.000000e+00  2.081668e-17 -2.775558e-17 -8.326673e-17
# [3,]  0.000000e+00 -8.326673e-17  1.000000e+00 -1.422473e-16 -1.110223e-16
# [4,]  5.551115e-17 -5.551115e-17  1.387779e-17  1.000000e+00  0.000000e+00
# [5,] -5.551115e-17  5.551115e-17 -2.775558e-17  1.387779e-17  1.000000e+00
# > xinv <- cacheSolve(cm)
# cacheSolve: getting cached data
# > 
# > xinv <- cacheSolve(cm)
# cacheSolve: getting cached data
# > cm$delinv()
# > xinv <- cacheSolve(cm)
# > xinv <- cacheSolve(cm)
# cacheSolve: getting cached data
# > 
##


#
# makeCacheMatrix returns a list of functions for matrix comes as input
#
makeCacheMatrix <- function(x = matrix()) {
	INV <- NULL
	set <- function(y) {
		x <<- y
		INV <<- NULL
	}
	get <- function() x
	setinv <- function(inv) INV <<- inv
	getinv <- function() INV
	delinv <- function() INV <<- NULL
	list(set = set, 
		get = get,
		setinv = setinv,
		getinv = getinv,
		delinv = delinv
	)
}

#
# cacheSolve returns a matrix that is the inverse of x. Inverse is calculated only once - 
# when invertible matrix hasn't found yet. When it was successfully found and 'cached' - it's returned
# instead being calculated again
#
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	INV <- x$getinv()
	if(!is.null(INV)) {
		message("cacheSolve: getting cached data")
		return(INV)
	}
	d <- x$get()
	INV <- solve(d, ...)
	x$setinv(INV)
	INV
}

#
# genInvMatrix - generates random invertable matrix (auxiliary function, not graded!)
#
genInvMatrix <- function(size = 10) {
	flag <- F
	x <- y <- NA
	while(!flag){
		#message("generate...")
		x = matrix(rnorm(size*size, sd=100), size, size)
		flag <- class(try(solve(x),silent=T))=="matrix"
	}
	x
}
