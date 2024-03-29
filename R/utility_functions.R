#' @importFrom stats complete.cases weighted.mean


###################################
#### Expand Formula (from plm) ####
###################################


expand.formula <- function(x){
  oclass <- class(x)
  if (! any(class(x) == "Formula")) stop("not a Formula object")
  if (length(x)[2] != 2) stop("not a two part formula")
  xs <- structure(x, class = "formula")
  has.response <- attr(terms(xs),"response") == 1
  if (has.response){
    y <- x[[2]]
    rhs <- x[[3]]
  }
  else{
    y <- NULL
    rhs <- x[[2]]
  }
  firstpart <- rhs[[2]]
  secondpart <- rhs[[3]]
  if (has.response){
    one <- do.call("~", list(y,firstpart))
    two <- do.call("~", list(y,secondpart))
  }
  else{
    one <- do.call("~", list(firstpart))
    two <- do.call("~", list(secondpart))
  }
  two <- update(one, two)
  one <- paste(deparse(one), collapse = "")
  two <- paste(deparse(two[[3]]), collapse = "")
  result <- as.formula(paste(one, "|", two, collapse = ""));
  result <- Formula::as.Formula(result)
  #YC  class(result) <- c("pFormula", class(result))
  structure(result, class = oclass)
}



#################################
#### Function Residual maker ####
#################################

resm <- function(x, tol = .Machine$double.eps, ...){
  x <- as.matrix(x)
  r <- diag(1, nrow(x)) - x %*% tcrossprod(solve(crossprod(x), tol = tol), x)
  return(r)
}

##################################
#### Function Predicted maker ####
##################################

hatm <- function(y, x, weights = NULL, checkcol = TRUE, tol = .Machine$double.eps, isw = FALSE, ...){
  x <- as.matrix(x)
  y <- as.matrix(y)

  # Check for perfect collinearity within groups
  if(checkcol == TRUE){
    x.qr <- qr(x)
    if(x.qr$rank < ncol(x)){
      vars <- x.qr$pivot[1:x.qr$rank]
      x <- x[, vars, drop = FALSE]
    }
  }

  if(!isw){
    res <- stats::fitted(stats::lm.fit(x, y, tol = tol))
  }else{
    res <- stats::fitted(stats::lm.wfit(x, y, w = weights, tol = tol))
  }

  return(res)
}



###################################
#### Detrend function for data ####
###################################

#' @title Detrend data by individual slopes
#'
#' @description
#' Detrends the input data by the predicted values based on the slope parameters within each group
#' specified by id. The result is equal to the transformed data used for estimation in
#' \code{\link[feisr]{feis}}.
#'
#' @details
#' \code{detrend} performs within-group "residual maker" transformation on the origin data.
#' Within each group, the predicted values of the columns in data are computed based on the
#' slope columns plus an individual intercept if \code{intercept = TRUE} (the default).
#' Subsequently the predicted values are subtracted from the origin data. The transformed
#' data can, for instance, be used to obtain coefficients of a fixed effects individual slopes
#' estimator via \code{\link[stats]{lm}}
#' \insertCite{Bruderl.2015.387,Ruttenauer.2020,Wooldridge.2010.384}{feisr}.
#'
#' Estimation requires at least \code{q+1} observations per unit, where \code{q} is the number of slope
#' parameters (including a constant).
#' \code{detrend} automatically selects only those groups from the current data set which have
#' at least \code{q+1} observations, and returns NA for all groups with \code{n_i} < \code{q+1}.
#'
#' \code{NA} values in the input data are handled by list-wise deletion based on the data to be
#' detrended and the slopes.
#'
#' @seealso \code{\link[feisr]{feis}}
#'
#' @param data a \code{data.frame}, \code{matrix}, or \code{vector} of data to be detrended. If \code{id}
#'  and / or \code{slopes} are given as character (see below), must contain \code{id} and / or
#'  \code{slopes} as variable(s). Otherwise must be excluded.
#' @param slopes a \code{data.frame}, \code{matrix}, or \code{vector} of slopes to be used for detrending,
#'  not containing an intercept. Optionally, a \code{character} vector of the names of slope variables
#'  in \code{data}. For pure de-meaning use \code{"slopes = 1"}.
#' @param id a \code{vector} of a unique group / person identifier. Optionally, a \code{character}
#'  of the name of the unique group / person identifier in \code{data}. For overall detrending,
#'  use \code{"id = 1"}.
#' @param intercept logical. If \code{TRUE} the slopes will contain an individual
#'  intercept (default is \code{TRUE}). For \code{"id = 1"}, this is an overall intercept.
#'  Ignored if "slopes = 1".
#' @param na.action character, either \code{na.exclude} (default) or \code{na.omit} indicates the use
#'  of \code{NA}s. \code{na.exclude} passes \code{NA}s through to the output (same length as input).
#'  \code{na.omit} drops \code{NA} rows (list-wise).
#' @param tol	the tolerance for detecting linear dependencies in the residual maker transformation
#' (see \code{\link[base]{solve}}).
#' @param predicted logical. If \code{TRUE} returns the predicted values instead of the
#' detrended data (default is \code{FALSE}).
#' @param ...	further arguments.
#'
#' @return An object of class "\code{data.frame}" or "\code{numeric} (if only one data column),
#' containing the detrended data with \code{row.names} equal
#' to the \code{row.names} of the origin data. If input is an unnamed vector, names are 1:length.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data("mwp", package = "feisr")
#'
#' # Detrend entire data.frame
#' mwp_det <- detrend(data = mwp, slopes = c("exp", "expq"), id = "id")
#'
#' # Detrend single variable
#' lnw_det <- detrend(data = mwp$lnw, slopes = mwp[, c("exp", "expq")], id = mwp$id)
#'
#' @export
#'
detrend <- function(data, slopes, id = NULL, intercept = TRUE,
                    na.action = c("na.exlude", "na.omit"),
                    tol = .Machine$double.eps, predicted = FALSE, ...){

  ### Test input
  if(! is.matrix(data) & ! is.data.frame(data) & ! is.vector(data) ){
    stop(paste("data must be data.frame, matrix, or vector"))
  }
  if(is.vector(data)){
    ld <- length(data)
  }else{
    ld <- nrow(data)
  }

  if(is.null(id)){
    stop(paste("id is missing. For overall detrend, use 'id = 1'"))
  }
  if(is.null(slopes)){
    stop(paste("Argument slopes missing. Misspelled?"))
  }
  if(length(slopes) == 1 && slopes == 1){
    dem <- TRUE
  }else{
    dem <- FALSE
  }

  ov <- FALSE
  if(is.vector(id)){
    if(length(id) != ld & length(id) != 1){
      stop(paste("id must have same length as data"))
    }
    if(length(id) == 1){
      if(id == 1){
        id <- rep(1, ld)
        ov <- TRUE
      }
    }
  }
  if(is.character(id) && length(id) != ifelse(is.vector(data), length(data), nrow(data))){
    if(length(id) != 1){
      stop(paste("id must be character of dimension 1"))
    }
    idcol <- which(names(data) == id)
    if(length(idcol) != 1){
      stop(paste("id (as character) must uniquely identify a column in data"))
    }
    idnames <- id
    id <- data[ , idcol]
    data <- data[ , -idcol, drop = FALSE]
    if(any(is.na(id))){
      stop(paste("NAs in id not allowed"))
    }
  }

  # All slopes available
  if(!is.character(slopes) && !dem){
    if(is.vector(slopes)){
      ls <- length(slopes)
    }else{
      ls <- nrow(slopes)
    }
    if(ld != ls){
      stop(paste("slopes must have same length as data"))
    }
  }

  if(dem){
    slopes <- rep(1, ld)
  }

  if(is.character(slopes)){
    slpcol <- which(names(data) %in% slopes)
    if(length(slpcol) != length(slopes)){
      nf <- which(! slopes %in% names(data))
      stop(paste("Slopes not found in data:", paste0(slopes[nf], collapse = ", ")))
    }
    slpnames <- slopes
    slopes <- data[ , slpcol, drop = FALSE]
    data <- data[ , -slpcol, drop = FALSE]
  }


  ### Get original input
  origrn <- rownames(data)
  if(is.null(origrn)){
    data <- data.frame(data)
    origrn <- rownames(data)
  }

  # Complete cases
  cp <- complete.cases(data.frame(data, slopes))
  omitted <- which(cp == FALSE)
  names(omitted) <- row.names(data)[which(cp == FALSE)]
  attr(omitted, "class") <- "omit"

  data <- data.frame(data)[cp, , drop = FALSE]
  slopes <- data.frame(slopes)[cp, , drop = FALSE]
  id <- c(id)[cp]

  # Make model matrix
  if(dem){
    fmz <- formula(paste0("~", "1"))
  }else{
    fmz <- formula(paste0("~", paste0(colnames(slopes), collapse = "+")))
  }

  Z <- model.matrix(fmz, slopes)
  # instead of update fm -1 use [,-1] (to avoid contrasts for intercept)
  if(intercept == FALSE && !dem){
    Z <- Z[, -1, drop = FALSE]
  }

  fmx <- formula(paste0("~", paste0(colnames(data), collapse = "+")))
  # fmx <- update(fmx, ~ . -1)
  X <- model.matrix(fmx, data)
  X <- X[, -1, drop = FALSE]

  ### Subset to obs with N > n_slopes+1
  ns <- ncol(Z)
  pcount <- ave(id, id, FUN = function(x) length(x))

  if(any(pcount < (ns + 1))){
    warning(paste("Detrend needs at least n(slopes)+1 observations per group. \n",
                  "You specified", ns, "slope parameter(s) plus intercept,",
                  "all groups with t <=", ns+1, "dropped", sep=" "), call. = TRUE, immediate. = TRUE)

    # reduce sample
    nn <- which(pcount <= (ns + 1))
    nnrn <- row.names(data)[nn]

    na <- which(origrn %in% nnrn)
    names(na) <- nnrn
    omitted <- sort(c(omitted, na))
    attr(omitted, "class") <- "omit"

    X <- X[-nn, , drop = FALSE]
    Z <- Z[-nn, , drop = FALSE]
    id <- id[-nn]
  }

  ### Check collinearity and variance
  if(qr(Z, tol = tol)$rank < ncol(Z)){
    stop(paste("Perfect collinearity in slope variables. See 'tol' option."))
  }

  ### Detrend

  nx <- ncol(X)
  nz <- ncol(Z)

  df_step1 <- cbind(Z, X)
  rownames(df_step1) <- rownames(X)

  dhat <- by(df_step1, id, FUN = function(u) data.frame(hatm(y = u[, (nz + 1):(nz + nx), drop = FALSE],
                                                             x = u[, 1:nz, drop = FALSE],
                                                            checkcol = TRUE, tol = tol)), simplify = FALSE)

  if(utils::packageVersion("dplyr") >= "1.0.0"){
    dhat <- dplyr::bind_rows(rbind(dhat), .id = NULL) # only for version dplyr >= 1.0.0 keeps rownames
  }else{
    dhat <- do.call(rbind, lapply(dhat, as.matrix)) # use dplyr for more efficiency
  }


  # Keep orig col names
  colnames(dhat) <- colnames(df_step1)[(nz + 1):(nz + nx)]

  # Ensure original order
  dhat <- dhat[match(rownames(X), rownames(dhat)), ]

  # Detrend X (or return predicted)
  if(predicted == FALSE){
    detr <- data.frame(X - dhat)
  }
  if(predicted == TRUE){
    detr <- data.frame(dhat)
    rownames(detr) <- rownames(X)
  }

  ### Add NA rows for omitted
  if(na.action[1] == "na.omit"){
    if(is.null(dim(detr))){
      if(length(omitted) == 0){
        names(detr) <- origrn
      }else{
        names(detr) <- origrn[-omitted]
      }
    }
  }else{
    lo <- length(omitted)
    if(lo != 0){
      nd <- nrow(detr)
      detr[(nd + 1):(nd + lo), ] <- NA
      rownames(detr)[(nrow(detr) - lo + 1): nrow(detr)] <- names(omitted)
      detr <- detr[match(origrn, rownames(detr)), ]
    }
    if(is.null(dim(detr))){
      names(detr) <- origrn
    }
  }




  return(detr)

}



#############################
#### Weighted mean by id ####
#############################


ave_wm <- function(x, i, w, na.rm = TRUE){
  Z <- data.frame(x, i, w)
  res <- ave(Z[, c(1, 3)], Z[, 2], FUN = function(v) weighted.mean(v[, 1], w = v[, 2], na.rm = na.rm))[[1]]
  return(res)
}


############################
#### Clean "AsIs" names ####
############################

cleani <- function(x, ...){
  # x <- gsub(".*I\\(\\s*|\\).*", "", x)
  x <- gsub("^I\\(", "", x)
  x <- gsub("\\(", "_", x)
  x <- gsub("\\)", "_", x)
  x <- gsub("[[:punct:]]", "_", x)
  x <- sub("_$", "", x)
  return(x)
}


#################################
#### Between variance slopes ####
#################################

nowithinvar <- function(x, mf, id, tol = 1e-12, ...){

  within <- lapply(colnames(x), FUN = function(u) mf - apply(mf, 2, FUN = function(z)
    ave(z, x[, u], FUN = function(w) mean(w)))
       - apply(mf, 2, FUN = function(v) ave(v, id, FUN = function(y) mean(y))))

  withinsd <- sapply(within, FUN = function(u) apply(u, 2, FUN = function(z) sd(z)))

  res <- which(apply(withinsd, 1, FUN = function(u) any(u < tol)))
  names(res) <- rownames(withinsd)[res]

  res
}


# betw_slp <- function(x, mf, ...){
#   sn <- colnames(x)
#   sn <- sn[-which(sn=="(Intercept)")]
#   res<-apply(mf, 2, FUN = function(z) ave(z, x[, "(Intercept)"], FUN=function(w) mean(w)))
#   for(i in sn){
#     res <- res - apply(mf, 2, FUN = function(z) ave(z, x[, i], FUN=function(w) mean(w)))
#   }
#   res
# }



##########################
#### Extract response ####
##########################

#' @title A function to extract the model.response
#'
#' @description
#' Returns the de-trended response variable of a \code{feis} object.
#'
#' @details
#' The function provides a convenient way to return the model.response of a \code{feis} object.
#' This is the transformed (de-trended) variable which is used for estimation of the final model.
#'
#' @param x an object of class \code{feis}.
#' @param ...	further arguments.
#'
#' @return A "\code{numeric}" of the transformed response variable of the estimation model.
#'
#'
#' @examples
#' data("mwp", package = "feisr")
#' feis.mod <- feis(lnw ~ marry + enrol | year,
#'                  data = mwp, id = "id")
#' y_tilde <- model.response.feis(feis.mod)
#'
#' @export
#'
model.response.feis <- function(x, ...){
  res <- as.vector(x$response)
  return(res)
}



#################
#### Sum Res ####
#################

sumres <- function(x, ...){
  u <- resid(x)
  nna <- which(!is.na(u))
  w <- x$weights
  if(length(w) > 1){
    sr <- summary(unclass(w[nna] * u[nna]))
  }else{
    sr <- summary(unclass(u[nna]))
  }

  srm <- sr["Mean"]
  if (abs(srm) < 1e-10){
    sr <- sr[c(1:3, 5:6)]
  }
  sr
}

rss.feis <- function(x, ...){
  u <- resid(x)
  nna <- which(!is.na(u))
  w <- x$weights
  if(length(w) > 1){
    w <- w[nna]
  }else if(is.null(w)){
    w <- 1
  }
  rss <- sum(w * u[nna]^2)
  return(rss)
}


#############
#### TSS ####
#############

tss.feis <- function(x, ...){
  u <- resid(x)
  nna <- which(!is.na(u))
  f <- fitted(x)[nna]
  y <- f - u
  N <- length(y)
  w <- x$weights
  if(length(w) > 1){
    w <- w[nna]
  }else if(is.null(w)){
    w <- 1
  }
  tss <- sum(w * (y - mean(y))^2)
  return(tss)
}


##############
#### RMSE ####
##############

rmse.feis <- function(x, ...){
  u <- resid(x)
  df <- df.residual(x)
  nna <- which(!is.na(u))
  w <- x$weights
  if(length(w) > 1){
    w <- w[nna]
  }
  rmse <- sqrt(sum(w * u[nna]^2) / df)
  return(rmse)
}


############################
#### Function R squared ####
############################

r.sq.feis <- function(object, adj=FALSE, df=NULL, intercept=FALSE){
  z <- object
  r <- z$residuals
  nna <- which(!is.na(r))
  r <- r[nna]
  n <- length(r)
  tss <- tss.feis(object)
  f <- z$fitted.values[nna]
  if(is.null(df)){
    rdf <- df.residual(object)
  } else{
    rdf <- df
  }

  w <- object$weights
  if(length(w) > 1){
    w <- w[nna]
  }else if(is.null(w)){
    w <- 1
  }

  if(intercept == TRUE){
    mss <- sum(w * (f - mean(f))^2)
    df.int <- 1L
    rdf <- rdf + 1
  } else{
    mss <- sum(w * f^2)
    df.int <- 0L
  }
  r.squared <- mss/tss
  adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)

  if(adj){
    return(adj.r.squared)
  }else{
    return(r.squared)
  }

}




