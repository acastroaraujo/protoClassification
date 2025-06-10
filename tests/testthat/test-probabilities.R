
set.seed(1)
K <- 8
marginals <- rbeta(K, 2, 2)
rho <- rlkjcorr(1, K, eta = 1)
sim_data <- make_binary_data(marginals, rho, obs = 4e3)
w <- runif(K)
w <- w / sum(w)

prototypes <- list(
  P1 = rep(1, K),
  P2 = rep(0, K),
  P2 = rep(0:1, K/2)
)

g <- rep(10, length(prototypes))

out <- compute(sim_data, prototypes, w, g)
probs <- summary(out, s = 4e3)

test_that("Law of Total Probability", {

  testthat::skip_on_cran()

  a <- apply(probs$conditional$features, 2, function(x) {
    sum(x * probs$marginal$categories)
  })

  b <- probs$marginal$features

  expect_true(all((a - b) <  1e-4))

  xk0 <- probs$conditional$categories[["Xk=0"]]
  xk1 <- probs$conditional$categories[["Xk=1"]]
  xk0 <- apply(xk0, 2, \(x) x * (1 - probs$marginal$features))
  xk1 <- apply(xk1, 2, \(x) x * probs$marginal$features)

  a <- xk0 + xk1
  b <- probs$marginal$categories

  output <- apply(a, 1, \(x) x - b)
  expect_true(all(output < 1e-3))

})

test_that("Bayes Theorem", {

  testthat::skip_on_cran()

  x_marginals <- probs$marginal$features
  cat_conditionals <- probs$conditional$categories

  xOFF <- sweep(cat_conditionals[[1]], (1 - x_marginals), MARGIN = 1, FUN = "*")
  xOn <- sweep(cat_conditionals[[2]], x_marginals, MARGIN = 1, FUN = "*")

  output <- t(xOn / (xOn + xOFF))

  expect_true(all(output - probs$conditional$features < 1e-3))

})

