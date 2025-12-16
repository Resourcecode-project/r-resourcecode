test_that("obital speed computation works", {
  spec <- t(vapply(
    1:10,
    \(h) jonswap(h, tp = 12)$spec,
    FUN.VALUE = numeric(36)
  ))
  expected_orb_speed_bottom = matrix(
    c(
      0.06908343,
      0,
      0.13816685,
      0,
      0.20725028,
      0,
      0.27633370,
      0,
      0.34541713,
      0,
      0.41450055,
      0,
      0.48358398,
      0,
      0.55266741,
      0,
      0.62175083,
      0,
      0.69083426,
      0
    ),
    byrow = TRUE,
    nrow = 10,
    ncol = 2
  )
  expected_orb_speed_half = matrix(
    c(
      0.09500958,
      0.065225,
      0.19001916,
      0.130450,
      0.28502873,
      0.195675,
      0.38003831,
      0.260900,
      0.47504789,
      0.326125,
      0.57005747,
      0.391350,
      0.66506704,
      0.456575,
      0.76007662,
      0.521800,
      0.85508620,
      0.587025,
      0.95009578,
      0.652250
    ),
    byrow = TRUE,
    nrow = 10,
    ncol = 2
  )

  expected_orb_speed_top = matrix(
    c(
      0.2561765,
      0.2466858,
      0.5123530,
      0.4933716,
      0.7685295,
      0.7400574,
      1.0247060,
      0.9867432,
      1.2808825,
      1.2334290,
      1.5370590,
      1.4801148,
      1.7932355,
      1.7268005,
      2.0494120,
      1.9734863,
      2.3055885,
      2.2201721,
      2.5617650,
      2.4668579
    ),
    byrow = TRUE,
    nrow = 10,
    ncol = 2
  )

  orb_speed_bottom <- compute_orbital_speeds(spec, rscd_freq, depth = 50, z = 0)
  orb_speed_half <- compute_orbital_speeds(spec, rscd_freq, depth = 50, z = 25)
  orb_speed_top <- compute_orbital_speeds(spec, rscd_freq, depth = 50, z = 50)
  orb_speed_bottom_spec <- compute_orbital_speeds(
    spec,
    rscd_freq,
    depth = 50,
    z = 0,
    output_speeds = TRUE
  )
  expect_equal(dim(orb_speed_bottom), c(10, 2))
  expect_equal(dim(orb_speed_bottom_spec), c(10, 36, 2))
  expect_equal(orb_speed_bottom[, 2], rep(0, 10))
  # nolint next
  expect_equal(orb_speed_bottom_spec[,, 2], matrix(0, ncol = 36, nrow = 10))
  expect_equal(orb_speed_bottom, expected_orb_speed_bottom, tolerance = 1e-7)
  expect_equal(orb_speed_half, expected_orb_speed_half, tolerance = 1e-7)
  expect_equal(orb_speed_top, expected_orb_speed_top, tolerance = 1e-7)
})
