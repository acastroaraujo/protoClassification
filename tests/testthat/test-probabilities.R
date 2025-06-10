
set.seed(1)
K <- 5
marginals <- rbeta(K, 2, 2)
rho <- rlkjcorr(1, K, eta = 1)
sim_data <- make_binary_data(marginals, rho, obs = 4e3)
w <- runif(K)
w <- w / sum(w)

prototypes <- list(
  P1 = rep(1, K),
  P2 = rep(0, K)
)

g <- rep(10, length(prototypes))

out <- compute(prototypes, w, sim_data, g = g, r = 1)
probs <- summary(out, s = 4e3)

test_that("Law of Total Probability", {

  a <- apply(probs$conditionals$features, 2, function(x) {
    sum(x * probs$marginals$categories)
  })

  b <- probs$marginals$features

  expect_true(all((a - b) <  1e-4))

  xk0 <- probs$conditionals$categories[["xk=0"]]
  xk1 <- probs$conditionals$categories[["xk=1"]]
  xk0 <- apply(xk0, 2, \(x) x * (1 - probs$marginals$features))
  xk1 <- apply(xk1, 2, \(x) x * probs$marginals$features)

  a <- xk0 + xk1
  b <- probs$marginals$categories

  output <- apply(a, 1, \(x) x - b)
  expect_true(all(output < 1e-4))

})

test_that("Bayes Theorem", {

  x_marginals <- probs$marginals$features
  cat_conditionals <- probs$conditionals$categories

  xOFF <- sweep(cat_conditionals[[1]], (1 - x_marginals), MARGIN = 1, FUN = "*")
  xOn <- sweep(cat_conditionals[[2]], x_marginals, MARGIN = 1, FUN = "*")

  output <- t(xOn / (xOn + xOFF))

  expect_true(all(output - probs$conditionals$features < 1e-4))

})

