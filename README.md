
<!-- README.md is generated from README.Rmd. Please edit that file -->
grasshopper
===========

The goal of grasshopper is to ...

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(grasshopper)

# Simulating a patch of grass
set.seed(111)
x <- grasshopper:::sim_grass_joint(area = 700)
image(x$grass, col = c("white", "darkgreen"))
```

<img src="man/figures/README-example-1.png" width="75%" />

``` r
set.seed(2)
ans <- grasshopper(
  area = 100, len = 10,
  nsim = 10000, # number of grass simulations
  subsim = 5e3, # number of times that we make the bug jump in each sim
  ring = .99, nchanges = 10
  )
#> The new best has a prob: 0.1032 (iter #2)
#> The new best has a prob: 0.1064 (iter #3)
```

<img src="man/figures/README-example2-1.png" width="75%" />

    #> The new best has a prob: 0.1230 (iter #4)

<img src="man/figures/README-example2-2.png" width="75%" />

    #> The new best has a prob: 0.1254 (iter #9)

<img src="man/figures/README-example2-3.png" width="75%" />

    #> The new best has a prob: 0.1302 (iter #26)

<img src="man/figures/README-example2-4.png" width="75%" />

    #> The new best has a prob: 0.1332 (iter #50)

<img src="man/figures/README-example2-5.png" width="75%" />

    #> The new best has a prob: 0.1376 (iter #52)

<img src="man/figures/README-example2-6.png" width="75%" />

    #> The new best has a prob: 0.1408 (iter #53)

<img src="man/figures/README-example2-7.png" width="75%" />

    #> The new best has a prob: 0.1418 (iter #62)

<img src="man/figures/README-example2-8.png" width="75%" />

    #> The new best has a prob: 0.1436 (iter #76)

<img src="man/figures/README-example2-9.png" width="75%" />

    #> The new best has a prob: 0.1456 (iter #96)

<img src="man/figures/README-example2-10.png" width="75%" />

    #> The new best has a prob: 0.1490 (iter #110)

<img src="man/figures/README-example2-11.png" width="75%" />

    #> The new best has a prob: 0.1554 (iter #196)

<img src="man/figures/README-example2-12.png" width="75%" />

    #> The new best has a prob: 0.1574 (iter #263)

<img src="man/figures/README-example2-13.png" width="75%" />

    #> The new best has a prob: 0.1592 (iter #386)

<img src="man/figures/README-example2-14.png" width="75%" />

    #> The new best has a prob: 0.1604 (iter #2672)

<img src="man/figures/README-example2-15.png" width="75%" />

    #> The new best has a prob: 0.1632 (iter #6068)

<img src="man/figures/README-example2-16.png" width="75%" />

    #> The new best has a prob: 0.1646 (iter #6628)

<img src="man/figures/README-example2-17.png" width="75%" />

    #> The new best has a prob: 0.1666 (iter #6682)

<img src="man/figures/README-example2-18.png" width="75%" />

    #> The new best has a prob: 0.1678 (iter #9962)

<img src="man/figures/README-example2-19.png" width="75%" /><img src="man/figures/README-example2-20.png" width="75%" />

``` r

# Plotting the best and the worse
plot_seq(ans)
```

<img src="man/figures/README-example2-21.png" width="75%" />
