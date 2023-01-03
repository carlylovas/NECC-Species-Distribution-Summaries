#####
## Center of Gravity from SDMTools: https://rdrr.io/cran/SDMTools/src/R/COGravity.R
wt.mean <- function(x, wt) {
    s = which(is.finite(x * wt))
    wt = wt[s]
    x = x[s] # remove NA info
    return(sum(wt * x) / sum(wt)) # return the mean
}

wt.var <- function(x, wt) {
    s = which(is.finite(x + wt))
    wt = wt[s]
    x = x[s] # remove NA info
    xbar = wt.mean(x, wt) # get the weighted mean
    return(sum(wt * (x - xbar)^2) * (sum(wt) / (sum(wt)^2 - sum(wt^2)))) # return the variance
}


wt.sd <- function(x, wt) {
    return(sqrt(wt.var(x, wt))) # return the standard deviation
}


COGravity <- function(x,y=NULL,z=NULL,wt=NULL) {
    # check if raster from sp or raster package and convert if necessary
    if (any(class(x) %in% "RasterLayer")) x = asc.from.raster(x)
    if (any(class(x) == "SpatialGridDataFrame")) x = asc.from.sp(x)
    #check if x is vector or matrix
    if (is.vector(x)) { #if the data is a vector...do calculations
    if (is.null(wt)) { # if no weighting supplied, calculate means & standard deviations
        out = c(COGx = mean(x, na.rm = TRUE), COGx.sd = sd(x, na.rm = TRUE))
        if (!is.null(y)) out = c(out, COGy = mean(y, na.rm = TRUE), COGy.sd = sd(y, na.rm = TRUE))
        if (!is.null(z)) out = c(out, COGz = mean(z, na.rm = TRUE), COGz.sd = sd(z, na.rm = TRUE))
    } else { # if weighting supplied, calculate weighted means and variances to get COG
        out = c(COGx = wt.mean(x, wt), COGx.sd = wt.sd(x, wt))
        if (!is.null(y)) out = c(out, COGy = wt.mean(y, wt), COGy.sd = wt.sd(y, wt))
        if (!is.null(z)) out = c(out, COGz = wt.mean(z, wt), COGz.sd = wt.sd(z, wt))
    }
    } else if (any(class(x) == 'asc')) { #if x is of class 'asc'
    if (is.null(wt)) { # if wt is null then assume that values in x are the weights
        pos = as.data.frame(which(is.finite(x), arr.ind = TRUE))
        pos$x = getXYcoords(x)$x[pos$row]
        pos$y = getXYcoords(x)$y[pos$col]
        pos$wt = x[cbind(pos$row, pos$col)]
        out = c(COGx = wt.mean(pos$x, pos$wt), COGx.sd = wt.sd(pos$x, pos$wt), COGy = wt.mean(pos$y, pos$wt), COGy.sd = wt.sd(pos$y, pos$wt))
    } else { # if wt is supplied, it must be of the same dim as x and then the values of x are assumed to be your z
        if (!all(dim(x) == dim(wt))) stop("the grids for x & weights must be of the same dimensions")
        pos = as.data.frame(which(is.finite(x), arr.ind = TRUE))
        pos$x = getXYcoords(x)$x[pos$row]
        pos$y = getXYcoords(x)$y[pos$col]
        pos$z = x[cbind(pos$row, pos$col)]
        pos$wt = wt[cbind(pos$row, pos$col)]
        out = c(COGx = wt.mean(pos$x, pos$wt), COGx.sd = wt.sd(pos$x, pos$wt), COGy = wt.mean(pos$y, pos$wt), COGy.sd = wt.sd(pos$y, pos$wt), COGz = wt.mean(pos$z, pos$wt), COGz.sd = wt.sd(pos$z, pos$wt))
    }
    }
    # return the output
    return(out)
}

"getXYcoords" <- function(w) {
    # check if raster from sp or raster package and convert if necessary
    if (any(class(w) %in% "RasterLayer")) w = asc.from.raster(w)
    if (any(class(w) == "SpatialGridDataFrame")) w = asc.from.sp(w)
    if (!inherits(w, "asc")) stop("must be of class asc")

    # Gets the attributes
    cs <- attr(w, "cellsize")
    xll <- attr(w, "xll")
    yll <- attr(w, "yll")

    ## Computation of the number of rows and columns of the matrix
    nr <- nrow(w)
    nc <- ncol(w)

    ## The results
    x <- xll + c(0:(nr - 1)) * cs
    y <- yll + c(0:(nc - 1)) * cs
    return(list(x = x, y = y))
}
