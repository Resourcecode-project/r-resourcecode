test_that("meridional conversion works", {
  u <- c(1, 0, -1, 0)
  v <- c(0, 1, 0, -1)
  m <- cbind(u, v)
  res <- data.frame(wspd = 1, wdir = c(270, 180, 90, 0))

  expect_equal(zmcomp2metconv(m), res)
  expect_equal(zmcomp2metconv(u, v), res)
})

test_that("%nin% helper works", {
  expect_false(1 %nin% c(1, 3, 5, 9))
  expect_true(1 %nin% c(3, 5, 9))
})

test_that("Node selection works", {
  point_test <- c(-4.700, 48.302)
  node_test <- 134938
  spec_test <- 2124
  expect_equal(closest_point_FIELD(point_test)$point, node_test)
  expect_equal(closest_point_FIELD(point_test[1], point_test[2])$point, node_test)
  expect_equal(closest_point_SPEC(point_test)$point, 2124)
  expect_equal(closest_point_SPEC(point_test[1], point_test[2])$point, 2124)
})

test_that("Fast trapz works", {
  n <- 101
  x <- seq(0, pi, length.out = n)
  Y1 <- sin(matrix(x, ncol = n, nrow = n, byrow = FALSE))
  Y2 <- sin(matrix(x, ncol = n, nrow = n, byrow = TRUE))
  expect_equal(fastTrapz(x, Y1, 1), t(fastTrapz(x, Y2, 2)))
})

test_that("JONSWAP computation works", {
  expect_vector(jonswap())
  expect_vector(jonswap(hs = 1, tp = 15))
  expect_snapshot_output(jonswap())
  expect_snapshot_output(jonswap(fmax = 0.95, df = 0.003))
  expect_error(jonswap(hs = 4, tp = 15, fmax = 0.95))
})
