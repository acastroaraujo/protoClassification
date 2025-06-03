
<!-- README.md is generated from README.Rmd. Please edit that file -->

# protoClassification

<!-- badges: start -->

[![R-CMD-check](https://github.com/acastroaraujo/protoClassification/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/acastroaraujo/protoClassification/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

You can install the development version of protoClassification from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("acastroaraujo/protoClassification")
```

<img src="overview.png" width="100%" />

### Get Started

To simulate a dataset like the one in the Figure you need to create to
decide a couple of things first.

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

round(rho, 2)
#>       k1    k2    k3    k4    k5    k6
#> k1  1.00  0.22  0.29  0.15 -0.02  0.36
#> k2  0.22  1.00 -0.08 -0.21 -0.01  0.37
#> k3  0.29 -0.08  1.00 -0.75  0.34  0.08
#> k4  0.15 -0.21 -0.75  1.00 -0.20 -0.34
#> k5 -0.02 -0.01  0.34 -0.20  1.00 -0.03
#> k6  0.36  0.37  0.08 -0.34 -0.03  1.00
round(marginals, 2)
#>   k1   k2   k3   k4   k5   k6 
#> 0.33 0.55 0.27 0.88 0.59 0.28
```

**Generate data.**

``` r
set.seed(1)
X <- make_binary_data(marginals, rho, obs = 1e3)
head(X, n = 10)
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

Verify that the column means *roughly* correspond to the marginal
probabilities.

``` r
colMeans(X) |> round(2)
#>   k1   k2   k3   k4   k5   k6 
#> 0.33 0.55 0.27 0.90 0.59 0.28
```

In order to verify that the data follows the correlation structure in
`rho` you would have to calculate a “[tetrachoric
correlation](https://en.wikipedia.org/wiki/Polychoric_correlation).”

``` r
psych::tetrachoric(X)$rho |> round(2)
#>       k1    k2    k3    k4    k5    k6
#> k1  1.00  0.23  0.28  0.14 -0.01  0.40
#> k2  0.23  1.00 -0.13 -0.14  0.03  0.38
#> k3  0.28 -0.13  1.00 -0.76  0.40  0.08
#> k4  0.14 -0.14 -0.76  1.00 -0.32 -0.35
#> k5 -0.01  0.03  0.40 -0.32  1.00 -0.09
#> k6  0.40  0.38  0.08 -0.35 -0.09  1.00
```

Additional stuff for Prototype Classification Model:

- `g` (gamma) sensitivity parameter
- `w` a vector of attention weights for each k
- `P` a list of prototypes

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
  data = X, 
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

out <- compute(prototypes, w, X, g = 10, r = 1)
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
#> ── Attention Weights:
#>    w1    w2    w3    w4    w5    w6 
#> 0.082 0.116 0.178 0.282 0.063 0.279
#> 
#> ── Other Parameters:
#>  g  r 
#> 10  1
#> 
#> ── Marginal Probabilities (From Data):
#>    k1    k2    k3    k4    k5    k6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
```

`consolidate()` the previous output into a single data frame:

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
