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
  skip_if_offline()
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))
  point_test <- c(-4.700, 48.302)
  node_test <- 134938
  spec_test <- 2124
  expect_equal(closest_point_field(point_test)$point, node_test)
  expect_equal(
    closest_point_field(point_test[1], point_test[2])$point,
    node_test
  )
  expect_equal(closest_point_spec(point_test)$point, 2124)
  expect_equal(closest_point_spec(point_test[1], point_test[2])$point, 2124)
})

test_that("Fast trapz works", {
  n <- 101
  x <- seq(0, pi, length.out = n)
  y1 <- sin(matrix(x, ncol = n, nrow = n, byrow = FALSE))
  y2 <- sin(matrix(x, ncol = n, nrow = n, byrow = TRUE))
  expect_equal(fastTrapz(x, y1, 1), t(fastTrapz(x, y2, 2)))
})

test_that("JONSWAP computation works", {
  expect_vector(jonswap())
  expect_vector(jonswap(hs = 1, tp = 15))
  expect_snapshot_output(jonswap())
  expect_snapshot_output(jonswap(fmax = 0.95, df = 0.003))
  expect_error(jonswap(hs = 4, tp = 15, fmax = 0.95))
})

test_that("Directional means are accurately computed", {
  expect_error(mean_direction())
  expect_equal(mean_direction(rep(0, 10)), 0)
  expect_equal(mean_direction(rep(0, 10)), mean_direction(rep(360, 10)))
})

test_that("Weighted directional means are accurately computed", {
  expect_warning(mean_direction(c(0, 90), weights = c(-1, 1)))
  expect_error(mean_direction(c(0, 90), weights = c(1)))
  expect_equal(mean_direction(c(0, 90), weights = c(50, 50)), 45)
  expect_equal(mean_direction(c(0, 90), weights = c(0, 1)), 90)
})

test_that("Directional binning works", {
  expect_equal(
    cut_directions(c(10, 80, 170, 280), n_bins = 4),
    factor(c("N", "E", "S", "W"), levels = c("N", "E", "S", "W"))
  )
  expect_equal(
    cut_directions(c(10, 80, 170, 280), n_bins = 8),
    factor(
      c("N", "E", "S", "W"),
      levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    )
  )
  expect_equal(
    cut_directions(c(10, 80, 170, 280), n_bins = 16),
    factor(
      c("N", "E", "S", "W"),
      levels = c(
        "N",
        "NNE",
        "NE",
        "ENE",
        "E",
        "ESE",
        "SE",
        "SSE",
        "S",
        "SSW",
        "SW",
        "WSW",
        "W",
        "WNW",
        "NW",
        "NNW"
      )
    )
  )
})
