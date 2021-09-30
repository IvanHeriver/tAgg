# TODO: 
# implement the 'factor' option where one can choose a custom time interval
# from the default ones e.g. 3 days: the time interval should be 'factor * by'
# TODO: 
# implement keep_offset option : the 'by' argument is only used to get
# the interval; the actual time step starts are whatever is the start of the input
# time series
# TODO: 
# implement time_step option (start, middle, end) where the user can choose whether
# he/she wants the target time steps to be at the start, middle or end with respect
# to the chosen interval (by).
# TODO: 
# for week, add an option to choose what is the sart of the week (e.g. sunday, monday, etc..)
# currently, whatever is the day of the time step is chosen as the start of the week
# FIXME: this part could possibly be further optimized.
#        in particular, using the findInterval() function and use the 
#        the result both here and in the my_approx() function for the
#        actual interpolation of the data could be done here.

#------------------------------------------------------------------------------
#' Time aggregation of vector or matrix/data.frame
#' 
#' Given a time vector (in POSIXct format) and a vector/matrix/data.frame of
#' values, this function computes the time aggregated values at a given target
#' time interval (e.g. "days").
#' 
#' Argument \code{time} should be provided in POSIXct format. If another format
#' is used, the function will attempt to convert to POSIXct with a warning using
#' \code{as.POSIXct(time, tz="UTC")}.
#' 
#' The dimensions of \code{time} and \code{x} must be consistent. If argument
#' \code{time} is of length \code{n}, argument \code{x} can be one of the
#' following: (1) a vector of length \code{n} or (2) a matrix or data.frame with
#' \code{n} rows.
#' 
#' @param time Time vector in POSIXct.
#' @param x A vector, matrix or data.frame with dimensions consistent with
#' argument \code{time} containing the values to be aggregated.
#' @param by Character string specifying the target time interval of the
#' aggregation. Its default value is "days" but it can be one of the following time
#' interval : "secs", "mins", "hours", "days", "weeks", "months", "years". Partial
#' matching is supported.
#' @param keep_sum Logical (default is \code{FALSE}). If \code{TRUE}, the time
#' aggregated values are not divided by the duration of the target time steps.
#' @param na.action Character string specifying how missing values are to be handled.
#' Three options are possible: (1) "propagate" (default) where any missing value is
#' propagated to its target time step, (2) "ignore" where missing values are treated
#' as 0s and (3) "threshold" where the proportion specified in argument \code{na.th}
#' is used as threshold of the maximum proportion (time wise) of accepted missing 
#' values for a given target time step; if below the threshold the "ignore" rule is 
#' followed, if above, the "propagate" rule is followed.
#' @param na.th Numeric value between 0 and 1 specifying the maximum proportion of
#' accepted missing value when using the "threshold" rule with argument \code{na.action}.
#' @param method Character string specifying how aggregation should be done. Two
#' options are possible: (1) "linear" (default) where source values are linearly
#' interpolated and the trapezoidal rule is followed in the aggregation;
#' (2) "constant" where source values are considered constant.
#' @param verbose Logical. Should function progress information be printed in the
#' console.
#' @return A data.frame with a first column \code{time} and one additional column
#' if \code{x} is a vector or \code{m} additional columns if \code{x} was matrix or
#' data.frame with \code{m} columns.
#' @importFrom magrittr "%>%"
#' @author Ivan Horner
#' @name tAgg
#' @export
tAgg <- function(
time,
x,
by = "days",
keep_sum = FALSE,
na.action = "propagate",
na.th = 0.05,
method = "linear",
verbose = FALSE
) {
  if (verbose) { message("Initialization..."); flush.console() }

  # -------------------------------------------------------------------
  # verifications
  if (!identical(class(time), c("POSIXct", "POSIXt"))) {
    warning("'time' should be in POSIXct! Converted to POSIXct.")
    time <- as.POSIXct(time, tz = "UTC")
  }
  if (is.null(dim(x))) x <- data.frame(x) # x needs to be a data.frame/matrix
  
  possible_by <- c("secs", "mins", "hours", "days", "weeks", "months", "years")
  by <- match.arg(arg = by, choices = possible_by)
  
  na.action_possible <- c("propagate", "ignore", "threshold")
  na.action <- match.arg(arg = na.action, choices = na.action_possible)
  
  method_possible <- c("linear", "constant")
  method <- match.arg(arg = method, choices = method_possible)
  
  # -------------------------------------------------------------------
  # create target time vector
  time_range <- range(time)
  time_range <- switch(by,
                       secs   =        format(time_range, format = "%Y-%m-%d %H:%M:%S"),
                       mins   = paste0(format(time_range, format = "%Y-%m-%d %H:%M"), ":00"),
                       hours  = paste0(format(time_range, format = "%Y-%m-%d %H"), ":00:00"),
                       days   = paste0(format(time_range, format = "%Y-%m-%d"), " 00:00:00"),
                       weeks  = paste0(format(time_range, format = "%Y-%m-%d"), " 00:00:00"),
                       months = paste0(format(time_range, format = "%Y-%m"), "-1 00:00:00"),
                       years  = paste0(format(time_range, format = "%Y"), "-1-1 00:00:00"))
  time_range <- as.POSIXct(time_range, tz = attr(time, "tzone"))
  time_range[2L] <- seq(time_range[2L], by = by, length.out = 2L)[2L]
  time_target <- seq(time_range[1L], time_range[2L], by = by)
  
  # -------------------------------------------------------------------
  # combining source/target time steps and compute time intervals 
  
  # convert to numeric
  time_num <- as.numeric(time)
  time_target_num <- as.numeric(time_target)
  
  # new time vector combining target and source time steps
  time_new <- c(time_num, time_target_num)
  
  # compute the mapping between source time steps and target time steps
  # (i.e. wich are the source time steps wich are located within target time steps)
  o <- order(time_new)
  time_indices <- cumsum(o > length(time_num))
  time_indices[time_indices == 0] <- 1 # 0 means it should also be part of the first interval
  time_indices <- as.factor(time_indices[-length(time_indices)]) # convert to factors for use with dplyr
  time_new <- time_new[o]
  d_time_new <- diff(time_new)
  
  # compute target time step duration (necessary to have the "weight" of each source time step)
  if (identical(na.action, "threshold")) {
    # managing missing value with a proportion threshold of accepted missing values
    # here we only retrieve the proportion of each source time step with respect to the target time step
    warning("na.action == 'threshold' is still experimental!") # FIXME: further testing necessary before I remove this warning
    Z <- data.frame(f = time_indices, dt = d_time_new)
    Y <- Z %>% dplyr::group_by(f) %>% dplyr::summarize(dt_target = sum(dt))
    Z2 <- Y %>% merge(Z, by = "f") %>% dplyr::mutate(prop_t = dt / dt_target) # proportion 
  } else {
    Y <- data.frame(f = time_indices, dt = d_time_new) %>% dplyr::group_by(f) %>% dplyr::summarize(dt_target = sum(dt))
  }
  
  # convert to double for maximum precision (--> doesn't change anything so I disabled that step that only slowed down code execution)
  # x <- apply(x, 2, as.double) # very slow if x is large!
  # time_num <- as.double(time_num)
  # time_target_num <- as.double(time_target_num)

  if (identical(na.action, "ignore")) {
    if (verbose) { message("Interpolating missing values..."); flush.console() }
    for (k in 1:ncol(x)) {
      is_na <- is.na(x[, k])
      x[is_na, k] <- my_approx(x = time_num[!is_na],
        y = x[!is_na, k, drop=FALSE],
        xout = time_num[is_na], method = "linear")
    }
  } 

  if (verbose) { message("Interpolating data..."); flush.console() }
  # -------------------------------------------------------------------
  # interpolate data at target time steps 
  interpolated_data <- my_approx(x = time_num, y = x, xout = time_target_num, method = method)          # faster if x has many column
  # interpolated_data <- apply(x, 2, function(e) approx(x = time_num, y = e, xout = time_target_num)$y) # faster if x has only few columns (but does not have the constant option)
  V0 <- rbind(x, interpolated_data)[o, , drop = FALSE] # combine source data and interpolated data

  # -------------------------------------------------------------------
  # compute the area below curve of each source time step before summing them
  # at the scale of the target time steps
  if (verbose) { message("Applying aggregation rule..."); flush.console() }
  if (identical(method, "linear")) V1 <- apply(V0, 2, function(e) RcppRoll::roll_sum(e, 2L) / 2 * d_time_new)                 # trapezoidale rule 
  else if (identical(method, "constant")) V1 <- apply(V0[-nrow(V0), , drop = FALSE], 2, function(e) e * d_time_new) # constant rule
  
  if (identical(na.action, "threshold")) {
    if (verbose) { message("Interpolating missing values..."); flush.console() }
    is_na <- is.na(V1)
    V1[is_na * Z2$prop_t <= na.th & is_na] <- 0 # missing values are set to 0 except when threshold is exceeded
  }
  
  if (verbose) { message("Computing below-curve area of target time steps..."); flush.console() }
  # compute volumes of target time steps
  V2 <- data.frame(f = time_indices, V1)  %>% dplyr::group_by(f) %>% dplyr::summarize_all(sum) %>% dplyr::select(-f) %>% data.frame() 
  # convert source below-curve area to target values by deviding by the target time step duration
  if (keep_sum) y <- V2 else y <- V2 / Y$dt_target  # devide by target time step duration
  
  # -------------------------------------------------------------------
  # return values as a data.frame
  colnames(y) <- colnames(x)
  return(data.frame(time = time_target[-length(time_target)], y))
}


# -------------------------------------------------------------------
# compared to approx(): slower if x is a vector, much faster if x is a matrix/data.frame
# they give slighly different results that I wasn't able to correct but it is likely only
# related to decimal/precision issues
# In addition, this function can be customized to implement other interpolation approaches
# (see argument "method" and the 'constant' option option for example)
my_approx <- function(x, y, xout, method="linear") {
  
  i <- findInterval(xout, x)
  i[i==0 | i >= length(x)] <- NA
  
  if (identical(method, "linear")) {
    x1 <- x[i]
    x2 <- x[i + 1]
    
    y1 <- y[i, , drop = FALSE]
    y2 <- y[i + 1, , drop = FALSE]
    
    A <- (y1 - y2) / (x1 - x2)
    B <- y1 - A * x1
    
    A * xout + B
  } else if (identical(method, "constant")) {
    y[i, , drop = FALSE]
  }
} 
