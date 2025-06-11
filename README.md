
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
```

**Generate data.**

``` r
set.seed(1)
sim_data <- make_binary_data(marginals, rho, obs = 1e3)
sim_data
#> 
#> ── Data ──
#> 
#> 1000 obs. of  6 variables:
#>  $ x1: int  0 1 0 1 0 1 0 1 0 1 ...
#>  $ x2: int  0 1 0 1 1 1 0 1 1 1 ...
#>  $ x3: int  0 1 1 0 1 0 0 0 1 0 ...
#>  $ x4: int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ x5: int  1 1 1 1 0 0 1 1 1 1 ...
#>  $ x6: int  0 1 0 0 1 0 0 1 0 1 ...
#> 
#> ── Parameters ──
#> 
#> ── Marginal Probabilities:
#>   x1   x2   x3   x4   x5   x6 
#> 0.33 0.55 0.27 0.88 0.59 0.28
#> 
#> 
#> ── Correlation Matrix:
#>       x1    x2    x3    x4    x5    x6
#> x1  1.00  0.22  0.29  0.15 -0.02  0.36
#> x2  0.22  1.00 -0.08 -0.21 -0.01  0.37
#> x3  0.29 -0.08  1.00 -0.75  0.34  0.08
#> x4  0.15 -0.21 -0.75  1.00 -0.20 -0.34
#> x5 -0.02 -0.01  0.34 -0.20  1.00 -0.03
#> x6  0.36  0.37  0.08 -0.34 -0.03  1.00
```

*Note. The parameters are stored as the `params` attribute in the
output.*

We can verify that the column means *roughly* correspond to the marginal
probabilities.

``` r
colMeans(sim_data)
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
```

In order to verify that the data follows the correlation structure in
`rho` you would have to calculate a “[tetrachoric
correlation](https://en.wikipedia.org/wiki/Polychoric_correlation).”

``` r
psych::tetrachoric(sim_data)$rho
#>             x1          x2          x3         x4          x5          x6
#> x1  1.00000000  0.23375492  0.28118697  0.1442114 -0.01192531  0.40365755
#> x2  0.23375492  1.00000000 -0.13315127 -0.1448709  0.03404136  0.38467926
#> x3  0.28118697 -0.13315127  1.00000000 -0.7612102  0.40485490  0.07792155
#> x4  0.14421140 -0.14487091 -0.76121019  1.0000000 -0.31759978 -0.34664697
#> x5 -0.01192531  0.03404136  0.40485490 -0.3175998  1.00000000 -0.08953172
#> x6  0.40365755  0.38467926  0.07792155 -0.3466470 -0.08953172  1.00000000
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
  data = sim_data,
  P = rep(1, K),
  w = w,
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

out <- compute(sim_data, prototypes, w, g)
out
#> 
#> ── Output ──
#> 
#>  $ distance      1000 obs. of  3 variables
#>  $ similarity    1000 obs. of  3 variables
#>  $ probabilities 1000 obs. of  3 variables
#>  $ data          1000 obs. of  6 variables
#> 
#> ── Prototypes ──
#> 
#>  $ P1: num [1:6] 1 1 1 1 1 1
#>  $ P2: num [1:6] 0 0 0 0 0 0
#>  $ P3: int [1:6] 1 0 1 0 1 0
#> 
#> ── Distance ──
#> 
#> Manhattan (r = 1)
#> 
#> ── Sensitivity ──
#> 
#> g1 g2 g3 
#> 10 10 10
#> 
#> ── Attention Weights ──
#> 
#>    w1    w2    w3    w4    w5    w6 
#> 0.082 0.116 0.178 0.282 0.063 0.279
#> 
#> ── Marginal Probabilities ──
#> 
#> ── `colMeans(.$data)`
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
#> 
#> ── `colMeans(.$probabilities)`
#>        C1        C2        C3 
#> 0.3883449 0.4360664 0.1755887
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
#>  $ x1   : int  0 1 0 1 0 1 0 1 0 1 ...
#>  $ x2   : int  0 1 0 1 1 1 0 1 1 1 ...
#>  $ x3   : int  0 1 1 0 1 0 0 0 1 0 ...
#>  $ x4   : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ x5   : int  1 1 1 1 0 0 1 1 1 1 ...
#>  $ x6   : int  0 1 0 0 1 0 0 1 0 1 ...
```

*Note. Since only binary data is implemented, there is no difference
between Manhattan and Euclidean distance!*

## Marginal and Conditional Probabilities

So far, a single simulation requires the marginal probabilities for each
element of $\mathbf{x}$ to be specified at the outset.

``` r
colMeans(out$data) # cf. `marginals` argument in `make_binary_data()`
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
```

The more relevant piece of information we get from the `compute()`
function is the `.$probabilities` object, which calculates the
probability that any given individual in our simulated dataset will
belong to each of the prototype categories.

This allows us to calculate the marginal probabilities for each
category.

``` r
colMeans(out$probabilities)
#>        C1        C2        C3 
#> 0.3883449 0.4360664 0.1755887
```

With some ingenuity, we can use this information to get *conditional
probabilities* too.

$$
\Pr(X_k = 1 \mid C = c)
$$

``` r
conditionalProbs(out, "features")
#>           x1        x2         x3        x4        x5         x6
#> C1 0.5226571 0.7802353 0.32826487 0.9332301 0.6171392 0.62588499
#> C2 0.1302299 0.4297247 0.05791891 0.9465871 0.4820181 0.03806980
#> C3 0.3770770 0.3621551 0.69020964 0.7052639 0.7862111 0.08786438
```

$$
\Pr(X_k = 0 \mid C = c)
$$

``` r
1 - conditionalProbs(out, "features")
#>           x1        x2        x3         x4        x5        x6
#> C1 0.4769570 0.2200126 0.6714007 0.06717440 0.3829206 0.3734904
#> C2 0.8703417 0.5706125 0.9425701 0.05316343 0.5189671 0.9620408
#> C3 0.6227137 0.6362298 0.3103042 0.29413893 0.2117976 0.9130266
```

$$
\Pr(C = c \mid X_k)
$$

``` r
conditionalProbs(out, type = "categories")
#> $`Xk=0`
#>           C1        C2         C3
#> x1 0.2743383 0.5632908 0.16237092
#> x2 0.1926099 0.5577848 0.24960538
#> x3 0.3591157 0.5664215 0.07446281
#> x4 0.2566337 0.2286733 0.51469307
#> x5 0.3608689 0.5489563 0.09017476
#> x6 0.2001655 0.5788414 0.22099310
#> 
#> $`Xk=1`
#>           C1         C2         C3
#> x1 0.6236135 0.17344785 0.20293865
#> x2 0.5456643 0.33832130 0.11601444
#> x3 0.4652701 0.09116788 0.44356204
#> x4 0.4029833 0.45951724 0.13749944
#> x5 0.4073537 0.35719728 0.23544898
#> x6 0.8839345 0.06015273 0.05591273
```

Alternatively, it’s easier to use the `summary()` function to extract
all conditional and marginal probabilities.

``` r
probs <- summary(out)
probs
#> 
#> ── Categories ──
#> 
#> ── Marginals:
#>    C1    C2    C3 
#> 0.388 0.436 0.176
#> 
#> ── Conditionals:
#> $`Xk=0`
#>       C1    C2    C3
#> x1 0.276 0.562 0.162
#> x2 0.193 0.558 0.249
#> x3 0.360 0.565 0.075
#> x4 0.258 0.231 0.511
#> x5 0.362 0.548 0.091
#> x6 0.202 0.578 0.220
#> 
#> $`Xk=1`
#>       C1    C2    C3
#> x1 0.624 0.173 0.202
#> x2 0.548 0.337 0.116
#> x3 0.467 0.092 0.441
#> x4 0.404 0.458 0.137
#> x5 0.409 0.357 0.234
#> x6 0.884 0.060 0.056
#> 
#> ── Features ──
#> 
#> ── Marginals:
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
#> 
#> ── Conditionals:
#>       x1    x2    x3    x4    x5    x6
#> C1 0.523 0.779 0.329 0.933 0.618 0.625
#> C2 0.129 0.429 0.057 0.946 0.480 0.038
#> C3 0.378 0.364 0.691 0.705 0.788 0.088
```
