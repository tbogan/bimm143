Class-17: Visualization of Biological Networks
================

Set up Cytoscape and R connection
---------------------------------

We will use the **RCy3** and **igraph** package in this class. The first is from bioconductor, and the second is from CRAN.

First, let's do a couple steps to make sure that we have the tools ready to go that we will need for the remainder of this exercise.

``` r
library(RCy3)

# Test the connection to Cytoscape.
cytoscapePing()
```

    ## [1] "You are connected to Cytoscape!"

``` r
# Check the version
cytoscapeVersionInfo()
```

    ##       apiVersion cytoscapeVersion 
    ##             "v1"          "3.7.1"

We can test things further by making a small network (here in igraph format as used by the R igraph package) and sending it to Cytoscape:

``` r
g <- makeSimpleIgraph()
createNetworkFromIgraph(g,"myGraph")
```

    ## Loading data...
    ## Applying default style...
    ## Applying preferred layout...

    ## networkSUID 
    ##        1562

``` r
setVisualStyle("Marquee")
```

    ##                 message 
    ## "Visual Style applied."

Note: Our test was successful! We had the following graph populated into our Cytoscape instance.

``` r
fig <- exportImage(filename="demo_marquee", type="png", height=350)

knitr::include_graphics("./demo.png")
```

![](./demo.png)
