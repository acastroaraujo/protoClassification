
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
#> ── `colMeans(.$data)`:
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
#> 
#> ── `colMeans(.$probabilities)`:
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

## Conditional Probabilities

**YOU ARE HERE**

The more relevant piece of information coming from the `compute()`
function is the `.$probabilities` object.

``` r
out$probabilities |> 
  head(n = 10)
#>            C1           C2          C3
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
get posterior conditional probabilities.

$$
\Pr(X_k = x \mid C = c)
$$

``` r
conditionalProbsSample(out, type = "features", s = 3)
#> $C1
#>             x1        x2        x3        x4        x5        x6
#> [1,] 0.5250660 0.7783641 0.3350923 0.9419525 0.6279683 0.6332454
#> [2,] 0.5219638 0.7700258 0.3385013 0.9431525 0.6072351 0.6253230
#> [3,] 0.5248042 0.7624021 0.3394256 0.9399478 0.6240209 0.6266319
#> 
#> $C2
#>             x1        x2         x3        x4        x5         x6
#> [1,] 0.1360544 0.4126984 0.05668934 0.9523810 0.4739229 0.03628118
#> [2,] 0.1270208 0.4364896 0.06004619 0.9445727 0.4942263 0.03464203
#> [3,] 0.1293303 0.4411085 0.04618938 0.9584296 0.4618938 0.03695150
#> 
#> $C3
#>             x1        x2        x3        x4        x5        x6
#> [1,] 0.3722222 0.4277778 0.6777778 0.6777778 0.7833333 0.1055556
#> [2,] 0.3833333 0.3722222 0.6500000 0.6944444 0.7722222 0.1000000
#> [3,] 0.3750000 0.3858696 0.6739130 0.6739130 0.8097826 0.1032609
```

$$
\Pr(C = c \mid X_k = x)
$$

``` r
conditionalProbsSample(out, type = "categories", s = 2)
#> $x1
#> , , x1=0
#> 
#>          C1        C2        C3
#> 1 0.2670623 0.5652819 0.1676558
#> 2 0.2922849 0.5563798 0.1513353
#> 
#> , , x1=1
#> 
#>          C1        C2        C3
#> 1 0.6319018 0.1533742 0.2147239
#> 2 0.6073620 0.1993865 0.1932515
#> 
#> 
#> $x2
#> , , x2=0
#> 
#>          C1        C2        C3
#> 1 0.1928251 0.5448430 0.2623318
#> 2 0.1995516 0.5695067 0.2309417
#> 
#> , , x2=1
#> 
#>          C1        C2        C3
#> 1 0.5415162 0.3393502 0.1191336
#> 2 0.5523466 0.3357401 0.1119134
#> 
#> 
#> $x3
#> , , x3=0
#> 
#>          C1        C2         C3
#> 1 0.3539945 0.5592287 0.08677686
#> 2 0.3553719 0.5730028 0.07162534
#> 
#> , , x3=1
#> 
#>          C1         C2        C3
#> 1 0.4708029 0.09124088 0.4379562
#> 2 0.5000000 0.08759124 0.4124088
#> 
#> 
#> $x4
#> , , x4=0
#> 
#>          C1        C2        C3
#> 1 0.2475248 0.2475248 0.5049505
#> 2 0.2970297 0.2178218 0.4851485
#> 
#> , , x4=1
#> 
#>          C1        C2        C3
#> 1 0.4015573 0.4516129 0.1468298
#> 2 0.4060067 0.4649611 0.1290323
#> 
#> 
#> $x5
#> , , x5=0
#> 
#>          C1        C2         C3
#> 1 0.3640777 0.5364078 0.09951456
#> 2 0.3859223 0.5436893 0.07038835
#> 
#> , , x5=1
#> 
#>          C1        C2        C3
#> 1 0.4013605 0.3571429 0.2414966
#> 2 0.4013605 0.3673469 0.2312925
#> 
#> 
#> $x6
#> , , x6=0
#> 
#>          C1        C2        C3
#> 1 0.2013793 0.5696552 0.2289655
#> 2 0.2068966 0.5820690 0.2110345
#> 
#> , , x6=1
#> 
#>          C1         C2         C3
#> 1 0.8727273 0.06545455 0.06181818
#> 2 0.8909091 0.06545455 0.04363636
```

Instead of working directly with the posterior draws, you can average
over them using the `conditionalProbs()` function:

``` r
conditionalProbs(out, type = "features", s = 300)
#>           x1        x2         x3        x4        x5         x6
#> C1 0.5214711 0.7798585 0.32929595 0.9326957 0.6174720 0.62664146
#> C2 0.1305170 0.4284553 0.05827153 0.9466424 0.4813426 0.03733254
#> C3 0.3784600 0.3652807 0.68685163 0.7061498 0.7870929 0.08690270
conditionalProbs(out, type = "categories", s = 300)
#> $`xk=0`
#>           C1        C2         C3
#> x1 0.2750198 0.5629080 0.16207221
#> x2 0.1923393 0.5585277 0.24913303
#> x3 0.3598990 0.5657576 0.07434343
#> x4 0.2573597 0.2310231 0.51161716
#> x5 0.3609304 0.5491667 0.08990291
#> x6 0.2012644 0.5784782 0.22025747
#> 
#> $`xk=1`
#>           C1         C2         C3
#> x1 0.6245297 0.17356851 0.20190184
#> x2 0.5472503 0.33732852 0.11542118
#> x3 0.4659611 0.09212895 0.44190998
#> x4 0.4037449 0.45901001 0.13724509
#> x5 0.4085998 0.35667800 0.23472222
#> x6 0.8837939 0.06031515 0.05589091
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
#> $`xk=0`
#>       C1    C2    C3
#> x1 0.275 0.563 0.163
#> x2 0.193 0.558 0.249
#> x3 0.359 0.566 0.075
#> x4 0.255 0.232 0.513
#> x5 0.361 0.550 0.090
#> x6 0.200 0.579 0.221
#> 
#> $`xk=1`
#>       C1    C2    C3
#> x1 0.623 0.174 0.203
#> x2 0.546 0.338 0.117
#> x3 0.465 0.092 0.443
#> x4 0.403 0.459 0.138
#> x5 0.408 0.356 0.236
#> x6 0.883 0.060 0.057
#> 
#> ── Features ──
#> 
#> ── Marginals:
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
#> 
#> ── Conditionals:
#>       x1    x2    x3    x4    x5    x6
#> C1 0.523 0.780 0.328 0.933 0.617 0.627
#> C2 0.130 0.430 0.058 0.946 0.482 0.038
#> C3 0.378 0.364 0.693 0.706 0.788 0.088
```

## Compositional Effects

YOU ARE HERE… EXPLAIN WHAT THEY ARE

$$
\Pr(\mathbf{x} \mid C_1)
$$

Bayes

$$
\Pr(x_1 \mid C_1) = \frac{\Pr(C_1 \mid x_1 = 1) \Pr(x_1 = 1)}{\Pr(C_1 \mid x_1 = 1) \Pr(x_1 = 1) + \Pr(C_1 \mid x_1 = 0) \Pr(x_1 = 0)}
$$

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
#> ── Categories ──
#> 
#> ── Marginals:
#>    C1    C2    C3 
#> 0.376 0.449 0.175
#> 
#> ── Conditionals:
#> $`xk=0`
#>       C1    C2    C3
#> x1 0.248 0.603 0.148
#> x2 0.129 0.597 0.274
#> x3 0.344 0.573 0.082
#> x4 0.299 0.248 0.452
#> x5 0.289 0.650 0.061
#> x6 0.223 0.558 0.218
#> 
#> $`xk=1`
#>       C1    C2    C3
#> x1 0.640 0.130 0.230
#> x2 0.575 0.330 0.096
#> x3 0.460 0.120 0.420
#> x4 0.385 0.472 0.144
#> x5 0.437 0.308 0.255
#> x6 0.778 0.160 0.061
#> 
#> ── Features ──
#> 
#> ── Marginals:
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
#> 
#> ── Conditionals:
#>       x1    x2    x3    x4    x5    x6
#> C1 0.555 0.847 0.335 0.920 0.683 0.570
#> C2 0.094 0.406 0.073 0.944 0.403 0.099
#> C3 0.429 0.304 0.658 0.739 0.859 0.095

w2 <- vector("double", length(w)) # all attention on dimension 2
w2[[2]] <- 1
w2
#> [1] 0 1 0 0 0 0

out <- compute(prototypes, w2, sim_data, g, r = 1)
summary(out)
#> 
#> ── Categories ──
#> 
#> ── Marginals:
#>    C1    C2    C3 
#> 0.554 0.223 0.223
#> 
#> ── Conditionals:
#> $`xk=0`
#>       C1    C2    C3
#> x1 0.504 0.249 0.247
#> x2 0.000 0.501 0.499
#> x3 0.578 0.211 0.210
#> x4 0.653 0.173 0.174
#> x5 0.541 0.230 0.229
#> x6 0.484 0.259 0.257
#> 
#> $`xk=1`
#>       C1    C2    C3
#> x1 0.656 0.171 0.172
#> x2 1.000 0.000 0.000
#> x3 0.489 0.256 0.255
#> x4 0.543 0.229 0.228
#> x5 0.563 0.219 0.218
#> x6 0.738 0.131 0.131
#> 
#> ── Features ──
#> 
#> ── Marginals:
#>    x1    x2    x3    x4    x5    x6 
#> 0.326 0.554 0.274 0.899 0.588 0.275
#> 
#> ── Conditionals:
#>       x1 x2    x3    x4    x5    x6
#> C1 0.386  1 0.242 0.881 0.597 0.366
#> C2 0.252  0 0.314 0.921 0.578 0.163
#> C3 0.251  0 0.313 0.922 0.575 0.160
```

------------------------------------------------------------------------

To do:

- Figure out when “r = 1” or “r = 2” matters. It seems that Manhattan
  distance is used when the underlying data is binary. Since I’m only
  using binary data in these simulations, it seems that this doesn’t
  matter, right?
- Figure out a best way to measure compositional effects (e.g., relative
  risk ratio, difference in probabilities)

Change P to C when necessary.
