The ***tAgg*** package contains a single function `tAgg()` to perform time aggregation from a source time series to a time aggregated time series.

Given a time vector and a vector/matrix/data.frame of values, it computes the time aggregated values at a given target time interval (e.g. "days", "month", "years", etc.).

# Getting started

## installation

You can install the package from github using the `remotes` R package: 
```r
remotes::install_github("IvanHeriver/tAgg")
```

## usage

First, load the package:

```r
library(tAgg)
```

To illustrate how the `tAgg` is used, let's create some synthetic data:

```r
test_data <- data.frame(
    time=as.POSIXct(
        c("2010-01-01 00:00:00"),
        c("2010-01-02 00:00:00"),
        c("2010-01-02 12:00:00"),
        c("2010-01-02 13:56:00"),
        c("2010-01-02 16:23:00"),
        c("2010-01-02 20:30:00")),
    data=c(2, 4, 3, NA, 5, 10)
)
```