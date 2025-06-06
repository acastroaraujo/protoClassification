
<!-- README.md is generated from README.Rmd. Please edit that file -->

# protoClassification

<!-- badges: start -->

[![R-CMD-check](https://github.com/acastroaraujo/protoClassification/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/acastroaraujo/protoClassification/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

Install the development version of protoClassification from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("acastroaraujo/protoClassification")
```

## Get Started

To simulate a dataset you need to create to decide a couple of things
first.

1.  The number of $K$ dimensions.
2.  The marginal probabilities for each dimension.
3.  A correlation matrix for the dimensions.

``` r
library(protoClassification)
set.seed(1)
K <- 6 # 1st step
marginals <- rbeta(K, 2, 2) # 2nd step
rho <- rlkjcorr(1, K, eta = 1) # 3rd step

nms <- paste0("k", 1:K)
names(marginals) <- nms
dimnames(rho) <- list(nms, nms)
```

**Generate data.**

``` r
set.seed(1)
sim_data <- make_binary_data(marginals, rho, obs = 1e3)
head(sim_data, n = 10)
#>    k1 k2 k3 k4 k5 k6
#> 1   0  0  0  1  1  0
#> 2   1  1  1  1  1  1
#> 3   0  0  1  1  1  0
#> 4   1  1  0  1  1  0
#> 5   0  1  1  1  0  1
#> 6   1  1  0  1  0  0
#> 7   0  0  0  1  1  0
#> 8   1  1  0  1  1  1
#> 9   0  1  1  1  1  0
#> 10  1  1  0  1  1  1
```

The parameters are stored as the `params` attribute in the output.

``` r
attr(sim_data, "params")
#> $marginals
#>        k1        k2        k3        k4        k5        k6 
#> 0.3275025 0.5516990 0.2743131 0.8814780 0.5923401 0.2780523 
#> 
#> $rho
#>             k1           k2          k3         k4           k5          k6
#> k1  1.00000000  0.223148504  0.28872627  0.1545050 -0.017382124  0.35530360
#> k2  0.22314850  1.000000000 -0.08484711 -0.2055278 -0.009983913  0.37405720
#> k3  0.28872627 -0.084847106  1.00000000 -0.7472276  0.341004033  0.08352309
#> k4  0.15450496 -0.205527752 -0.74722765  1.0000000 -0.200619041 -0.33781820
#> k5 -0.01738212 -0.009983913  0.34100403 -0.2006190  1.000000000 -0.03222531
#> k6  0.35530360  0.374057201  0.08352309 -0.3378182 -0.032225308  1.00000000
```

Verify that the column means *roughly* correspond to the marginal
probabilities.

``` r
colMeans(sim_data)
#>    k1    k2    k3    k4    k5    k6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
```

In order to verify that the data follows the correlation structure in
`rho` you would have to calculate a “[tetrachoric
correlation](https://en.wikipedia.org/wiki/Polychoric_correlation).”

``` r
psych::tetrachoric(sim_data)$rho
#>             k1          k2          k3         k4          k5          k6
#> k1  1.00000000  0.23375492  0.28118697  0.1442114 -0.01192531  0.40365755
#> k2  0.23375492  1.00000000 -0.13315127 -0.1448709  0.03404136  0.38467926
#> k3  0.28118697 -0.13315127  1.00000000 -0.7612102  0.40485490  0.07792155
#> k4  0.14421140 -0.14487091 -0.76121019  1.0000000 -0.31759978 -0.34664697
#> k5 -0.01192531  0.03404136  0.40485490 -0.3175998  1.00000000 -0.08953172
#> k6  0.40365755  0.38467926  0.07792155 -0.3466470 -0.08953172  1.00000000
```

Additional stuff for Prototype Classification Model:

- `w` a vector of attention weights for each k

- `P` a list of prototypes, one per category.

- `g` (gamma) sensitivity parameter.

``` r
set.seed(1)
w <- runif(K)
w <- w / sum(w)
g <- 10
```

Calculate distance and similarity for one prototype at a time:

``` r
d <- calculateDistSim(
  P = rep(1, K), 
  w = w, 
  data = sim_data, 
  g = g
)

str(d)
#> 'data.frame':    1000 obs. of  2 variables:
#>  $ distance  : num  0.655 0 0.477 0.457 0.145 ...
#>  $ similarity: num  0.00143 1 0.00846 0.01035 0.23423 ...
```

Calculate distance, similarity, and probabilities for multiple
prototypes at the same time:

``` r
prototypes <- list(
  P1 = rep(1, K),
  P2 = rep(0, K),
  P3 = rep(1:0, K / 2)
)

g <- rep(10, 3)

out <- compute(prototypes, w, sim_data, g = g, r = 1)
out
#> 
#> ── Overview ──
#> 
#> ── Output:
#>  $ distance     :1000 obs. of  3 variables:
#>  $ similarity   :1000 obs. of  3 variables:
#>  $ probabilities:1000 obs. of  3 variables:
#>  $ data         :1000 obs. of  6 variables:
#> 
#> ── Prototypes:
#>  $ P1: num [1:6] 1 1 1 1 1 1
#>  $ P2: num [1:6] 0 0 0 0 0 0
#>  $ P3: int [1:6] 1 0 1 0 1 0
#> 
#> ── Distance:
#> Manhattan (r = 1)
#> 
#> ── Sensitivity:
#> g1 g2 g3 
#> 10 10 10
#> 
#> ── Attention Weights:
#>    w1    w2    w3    w4    w5    w6 
#> 0.082 0.116 0.178 0.282 0.063 0.279
#> 
#> ── Marginal Probabilities, or `colMeans(.$data)`
#>    k1    k2    k3    k4    k5    k6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
```

`consolidate()` the previous output into a single data frame for easier
visualization.

``` r
d <- consolidate(out)
str(d)
#> 'data.frame':    1000 obs. of  15 variables:
#>  $ prob1: num  0.0379 0.9988 0.212 0.5783 0.998 ...
#>  $ prob2: num  8.45e-01 4.53e-05 1.34e-01 2.45e-01 8.26e-04 ...
#>  $ prob3: num  0.11692 0.00115 0.65354 0.17654 0.00115 ...
#>  $ sim1 : num  0.00143 1 0.00846 0.01035 0.23423 ...
#>  $ sim2 : num  3.18e-02 4.54e-05 5.36e-03 4.39e-03 1.94e-04 ...
#>  $ sim3 : num  0.0044 0.001149 0.026083 0.003159 0.000269 ...
#>  $ dist1: num  0.655 0 0.477 0.457 0.145 ...
#>  $ dist2: num  0.345 1 0.523 0.543 0.855 ...
#>  $ dist3: num  0.543 0.677 0.365 0.576 0.822 ...
#>  $ k1   : int  0 1 0 1 0 1 0 1 0 1 ...
#>  $ k2   : int  0 1 0 1 1 1 0 1 1 1 ...
#>  $ k3   : int  0 1 1 0 1 0 0 0 1 0 ...
#>  $ k4   : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ k5   : int  1 1 1 1 0 0 1 1 1 1 ...
#>  $ k6   : int  0 1 0 0 1 0 0 1 0 1 ...
```

## Compositional Effects

The more relevant piece of information coming from the `compute()`
function is the `.$probabilities` object.

``` r
out$probabilities |> 
  head(n = 10)
#>            P1           P2          P3
#> 1  0.03793516 8.451456e-01 0.116919205
#> 2  0.99880696 4.534577e-05 0.001147689
#> 3  0.21204400 1.344193e-01 0.653536676
#> 4  0.57832896 2.451337e-01 0.176537365
#> 5  0.99802732 8.258837e-04 0.001146794
#> 6  0.35850294 5.320625e-01 0.109434541
#> 7  0.03793516 8.451456e-01 0.116919205
#> 8  0.99726291 1.591178e-03 0.001145915
#> 9  0.73097337 4.589391e-02 0.223132720
#> 10 0.99726291 1.591178e-03 0.001145915
```

With this you can classify each row in the simulated dataset and then
get conditional probabilities for each $K$ feature.

There are two functions to calculate the compositional effects of one of
these simulations.

- `conditionalProbsWhichMax()`

- `conditionalProbsSample()`

Alternatively, it’s easier to use the `summary()` function.

``` r
summary(out)
#> 
#> ── Category Prevalence, or `colMeans(object$probabilities)`
#>    P1    P2    P3 
#> 0.388 0.436 0.176
#> 
#> ── Conditional Probabilities, or `lapply(conditionalProbsSample(object), colMeans)`
#> $`1`
#>    k1    k2    k3    k4    k5    k6 
#> 0.523 0.780 0.328 0.933 0.617 0.626 
#> 
#> $`2`
#>    k1    k2    k3    k4    k5    k6 
#> 0.130 0.430 0.058 0.947 0.482 0.038 
#> 
#> $`3`
#>    k1    k2    k3    k4    k5    k6 
#> 0.377 0.362 0.690 0.705 0.786 0.088
```

The point is to compare different probabilities across different
parameters values (i.e., compositional effects).

For example:

``` r
set.seed(1)

w_unif <- temperature(w, 5) # make weights more uniform
w_unif
#> [1] 0.1484224 0.1587893 0.1730981 0.1898107 0.1404808 0.1893986

out <- compute(prototypes, w_unif, sim_data, g, r = 1) 
summary(out)
#> 
#> ── Category Prevalence, or `colMeans(object$probabilities)`
#>    P1    P2    P3 
#> 0.376 0.449 0.175
#> 
#> ── Conditional Probabilities, or `lapply(conditionalProbsSample(object), colMeans)`
#> $`1`
#>    k1    k2    k3    k4    k5    k6 
#> 0.555 0.847 0.335 0.920 0.683 0.569 
#> 
#> $`2`
#>    k1    k2    k3    k4    k5    k6 
#> 0.094 0.406 0.073 0.944 0.403 0.098 
#> 
#> $`3`
#>    k1    k2    k3    k4    k5    k6 
#> 0.429 0.302 0.659 0.739 0.857 0.096

w2 <- vector("double", length(w)) # all attention on dimension 2
w2[[2]] <- 1
w2
#> [1] 0 1 0 0 0 0

out <- compute(prototypes, w2, sim_data, g, r = 1)
summary(out)
#> 
#> ── Category Prevalence, or `colMeans(object$probabilities)`
#>    P1    P2    P3 
#> 0.554 0.223 0.223
#> 
#> ── Conditional Probabilities, or `lapply(conditionalProbsSample(object), colMeans)`
#> $`1`
#>    k1    k2    k3    k4    k5    k6 
#> 0.386 1.000 0.242 0.881 0.597 0.366 
#> 
#> $`2`
#>    k1    k2    k3    k4    k5    k6 
#> 0.252 0.000 0.317 0.921 0.577 0.162 
#> 
#> $`3`
#>    k1    k2    k3    k4    k5    k6 
#> 0.250 0.000 0.311 0.922 0.576 0.161
```

------------------------------------------------------------------------

To do:

- Figure out when “r = 1” or “r = 2” matters. It seems that Manhattan
  distance is used when the underlying data is binary. Since I’m only
  using binary data in these simulations, it seems that this doesn’t
  matter, right?
- Figure out a best way to measure compositional effects (e.g., relative
  risk ratio, difference in probabilities)
