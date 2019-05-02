Class 6: R Functions
================
Tyler Bogan
April 18, 2019

About
-----

This is my **Class 6** R markdown document with my *codes* and notes for the day.

``` r
plot(1:10, typ="l", col="blue")
```

![](Class_6_files/figure-markdown_github/unnamed-chunk-1-1.png)

Reading files - Practice
------------------------

There are 3 files to read

``` r
read.table("test1.txt", header=TRUE, sep = ",")
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
read.csv("test1.txt")
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
read.table("test2.txt", header = TRUE, sep = "$")
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
read.table("test3.txt")
```

    ##   V1 V2 V3
    ## 1  1  6  a
    ## 2  2  7  b
    ## 3  3  8  c
    ## 4  4  9  d
    ## 5  5 10  e

``` r
read.csv("https://bioboot.github.io/bimm143_S19/class-material/test1.txt")
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

Functions
---------

Our First Stilly function example:

``` r
add <- function(x, y=1) {
  # The Body!!
  x + y
}
```

``` r
add(10)
```

    ## [1] 11

``` r
add(10,10)
```

    ## [1] 20

``` r
add( c(1,2,3) )
```

    ## [1] 2 3 4

``` r
add( c(1,2,3), 4 )
```

    ## [1] 5 6 7

``` r
#add(1,2,3)
```

``` r
rescale <- function(x) {
   rng <-range(x, na.rm = TRUE)
 (x - rng[1]) / (rng[2] - rng[1])
}
```

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

``` r
rescale2 <- function(x, na.rm=TRUE) {
  rng <-range(x, na.rm=na.rm)
  (x-rng[1]) / (rng[2] - rng[1])
}
```

``` r
rescale( c(1,2,NA,3,10) )
```

    ## [1] 0.0000000 0.1111111        NA 0.2222222 1.0000000

``` r
## rescale( c(1,10,"string") )
```

``` r
rescale3 <- function(x, na.rm=TRUE, plot=FALSE) {
 
  rng <-range(x, na.rm=na.rm)
 print("Hello")
 
 answer <- (x - rng[1]) / (rng[2] - rng[1])
 
 print("is it me you are looking for?")
 
 if(plot) {
   plot(answer, typ="b", lwd=4)
   }
 
 print("I can see it in ...")
 
 return(answer)
}
```

``` r
rescale3( c(1,3,NA,10) )
```

    ## [1] "Hello"
    ## [1] "is it me you are looking for?"
    ## [1] "I can see it in ..."

    ## [1] 0.0000000 0.2222222        NA 1.0000000
