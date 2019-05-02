Class-7
================
Tyler Bogan
April 23, 2019

Pulling functions from a URL
============================

``` r
source("http://tinyurl.com/rescale-R")
```

Try out last day's rescale() function
=====================================

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

Defining a function to show positions are missing (NA) values in two vectors
----------------------------------------------------------------------------

``` r
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)

is.na(y)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE

Try just combining them
-----------------------

``` r
is.na(x) & is.na(y)
```

    ## [1] FALSE FALSE  TRUE FALSE FALSE

Next, how to count them (recall TRUE = 1):
==========================================

``` r
sum(is.na(x) & is.na(y))
```

    ## [1] 1

Now, we can write this as our first function
--------------------------------------------

``` r
both_na <- function(x,y) {
  sum(is.na(x) & is.na(y))
}
```

``` r
both_na(x, c(NA, 3, NA, 2, NA) )
```

    ## [1] 2
