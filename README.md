The ***tAgg*** package contains a single function `tAgg()` to perform time aggregation.
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
        c("2010-01-01 00:00",
        "2010-01-02 00:00",
        "2010-01-02 12:00",
        "2010-01-02 13:56",
        "2010-01-02 16:23",
        "2010-01-02 20:30",
        "2010-01-03 07:00",
        "2010-01-04 01:15",
        "2010-01-04 19:46",
        "2010-01-05 02:33"), format="%Y-%m-%d %H:%M", tz="UTC"),
    data=c(2, 4, 3, NA, 5, 10, 0, 3, 6, 4)
)
plot(test_data, type="b", lwd=2)
```

Let's aggregate thsi data.frame at hourly and daily time intervals:

```r
# hourly aggregation
hourly_agg_data <- tAgg(
    test_data$time,
    test_data$data,
    by="hours"
)
head(hourly_agg_data)
tail(hourly_agg_data)
summary(hourly_agg_data)

# daily aggregation
daily_agg_data <- tAgg(
    test_data$time,
    test_data$data,
    by="days"
)
print(daily_agg_data)
```

And now, let's have a look at the result:

```r
lines(hourly_agg_data, col="blue", lwd=2, type="s")
lines(daily_agg_data, col="red", lwd=2, type="s")
```
