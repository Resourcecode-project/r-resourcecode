test_that("meridional conversion works", {
  u <- c(1, 0, -1, 0)
  v <- c(0, 1, 0, -1)
  m <- cbind(u, v)
  res <- data.frame(wspd = 1, wdir = c(270, 180, 90, 0))

  expect_equal(zmcomp2metconv(m), res)
  expect_equal(zmcomp2metconv(u, v), res)
})


test_that("metconv2zmcomp returns correct components for cardinal winds", {
  # North wind (10 m/s) → blows southward → u = 0, v = -10
  res <- metconv2zmcomp(10, 0)
  expect_equal(res$u, 0, tolerance = 1e-12)
  expect_equal(res$v, -10, tolerance = 1e-12)

  # East wind (5 m/s) → blows westward → u = -5, v = 0
  res <- metconv2zmcomp(5, 90)
  expect_equal(res$u, -5, tolerance = 1e-12)
  expect_equal(res$v, 0, tolerance = 1e-12)

  # South wind (8 m/s) → blows northward → u = 0, v = +8
  res <- metconv2zmcomp(8, 180)
  expect_equal(res$u, 0, tolerance = 1e-12)
  expect_equal(res$v, 8, tolerance = 1e-12)

  # West wind (12 m/s) → blows eastward → u = +12, v = 0
  res <- metconv2zmcomp(12, 270)
  expect_equal(res$u, 12, tolerance = 1e-12)
  expect_equal(res$v, 0, tolerance = 1e-12)
})

test_that("metconv2zmcomp works with vector inputs", {
  spd <- c(10, 5, 8, 12)
  dir <- c(0, 90, 180, 270)
  res <- metconv2zmcomp(spd, dir)

  expect_equal(nrow(res), 4)
  expect_equal(res$u, c(0, -5, 0, 12), tolerance = 1e-12)
  expect_equal(res$v, c(-10, 0, 8, 0), tolerance = 1e-12)
})

test_that("%nin% helper works", {
  expect_false(1 %nin% c(1, 3, 5, 9))
  expect_true(1 %nin% c(3, 5, 9))
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
  expect_error(jonswap(hs = 4, tp = 15, fmax = 0.95))
  expect_error(jonswap(gam = 0.5))
})

test_that("JONSWAP default are not changed", {
  # nolint start
  true_spec = tibble::tribble(
    ~freq             , ~spec                 ,
    0.0339            ,  0                    ,
    0.03729           ,  1.78912458747991e-05 ,
    0.041019          ,  0.0152443362934968   ,
    0.0451209         ,  0.361780824645876    ,
    0.04963299        ,  2.48937306355604     ,
    0.054596289       ,  8.3487239124918      ,
    0.0600559179      , 19.0193973615578      ,
    0.06606150969     , 51.0681971008098      ,
    0.072667660659000 , 61.4298599997782      ,
    0.079934426724900 , 23.0790154205524      ,
    0.087927869397390 , 15.0852082849038      ,
    0.096720656337129 , 10.7789730474366      ,
    0.106392721970842 ,  7.32188794399809     ,
    0.117031994167926 ,  4.81071104041718     ,
    0.128735193584719 ,  3.08518113899385     ,
    0.141608712943191 ,  1.95686309248167     ,
    0.15576958423751  ,  1.22683045725158     ,
    0.171346542661261 ,  0.766163735497102    ,
    0.188481196927387 ,  0.475995782202428    ,
    0.207329316620126 ,  0.294907527414745    ,
    0.228062248282138 ,  0.182596088371805    ,
    0.250868473110352 ,  0.112880437673245    ,
    0.275955320421387 ,  0.0698400710094801   ,
    0.303550852463526 ,  0.0431793187224093   ,
    0.333905937709879 ,  0.0266975644741862   ,
    0.367296531480867 ,  0.0165092697144078   ,
    0.404026184628953 ,  0.0102108753204184   ,
    0.444428803091849 ,  0.00631725412559007  ,
    0.488871683401034 ,  0.00390981800223497  ,
    0.537758851741137 ,  0.00242039217856272  ,
    0.591534736915251 ,  0.00149866668912047  ,
    0.650688210606776 ,  0.000928085964857926 ,
    0.715757031667453 ,  0.000574934487590613 ,
    0.787332734834199 ,  0.000356233212602035 ,
    0.866066008317619 ,  0.000220764277203686 ,
    0.952672609149381 ,  0.000136827317172514
  )
  # nolint end
  attr(true_spec, "Note") <- "JONSWAP Spectrum, Hs=5, Tp=15, gamma=3.3"
  expect_equal(jonswap(), true_spec)
})

test_that("Directional means are accurately computed", {
  expect_error(mean_direction())
  expect_equal(mean_direction(rep(0, 10)), 0)
  expect_equal(mean_direction(rep(0, 10)), mean_direction(rep(360, 10)))
  expect_error(mean_direction("one"), "'directions' must be numeric")
  expect_error(
    mean_direction(1:2, c("one", "two")),
    "'weights' must be numeric"
  )
})

test_that("Weighted directional means are accurately computed", {
  expect_warning(mean_direction(c(0, 90), weights = c(-1, 1)))
  expect_error(mean_direction(c(0, 90), weights = c(1)))
  expect_equal(mean_direction(c(0, 90), weights = c(50, 50)), 45)
  expect_equal(mean_direction(c(0, 90), weights = c(0, 1)), 90)
  expect_equal(mean_direction(directions = numeric(0)), numeric(0))
})

test_that("Directional binning works", {
  expect_error(cut_directions("one"), "'directions' must be numeric")
  expect_error(cut_directions(1, 1), "'n_bins' must be at least 2")
  expect_equal(cut_directions(numeric()), numeric())
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
