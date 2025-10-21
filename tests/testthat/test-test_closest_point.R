# ==============================================================================
# Test Suite 1: Input Validation
# ==============================================================================


test_that("Input validation works correctly", {
  # Test ill-formed coordinates input
  expect_error(
    closest_point_field(c(1, 2), 1),
    "If 'lat' is provided, it sould be the same length as 'x'."
  )

  expect_error(
    closest_point_field(c(1, 2), closest = "one"),
    "'closest' must be an integer greater than 1."
  )
  expect_error(
    closest_point_field(c(1, 2), closest = 0),
    "'closest' must be an integer greater than 1."
  )

  # Test ill-formed coordinates input
  expect_error(
    closest_point_spec(c(1, 2), 1),
    "If 'lat' is provided, it sould be the same length as 'x'."
  )

  expect_error(
    closest_point_spec(c(1, 2), closest = "one"),
    "'closest' must be an integer greater than 1."
  )
  expect_error(
    closest_point_spec(c(1, 2), closest = 0),
    "'closest' must be an integer greater than 1."
  )
})

# ==============================================================================
# Test Suite 2: Node selection: one location provided
# ==============================================================================

test_that("Unique node selection works", {
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))

  point_test <- c(-4.700, 48.302)
  node_test <- matrix(134938)
  spec_test <- matrix(2124)

  expect_equal(closest_point_field(point_test)$point, node_test)
  expect_equal(
    closest_point_field(point_test[1], point_test[2])$point,
    node_test
  )
  expect_equal(closest_point_spec(point_test)$point, spec_test)
  expect_equal(closest_point_spec(point_test[1], point_test[2])$point, spec_test)
})

# ==============================================================================
# Test Suite 3: Multiple nodes selection
# ==============================================================================


test_that("Multiple nodes selection works", {
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))

  point_test <- matrix(
    c(
      resourcecodedata::rscd_field[c(100, 1000), 2],
      resourcecodedata::rscd_field[c(100, 1000), 3]
    ),
    ncol = 2
  )
  node_test <- matrix(c(100, 1000), ncol = 2)
  spec_test <- matrix(c(23045, 22274), ncol = 2)


  output <- closest_point_field(point_test)
  expect_named(output, c("points", "distances"))
  expect_identical(dim(output$points), c(1L, 2L))
  expect_identical(dim(output$distances), c(2L, 1L))
  expect_equal(output$point, node_test)
  expect_true(all(output$distance == 0))

  output2 <- closest_point_field(point_test[, 1], point_test[, 2])
  expect_equal(output2, output)

  output_spec <- closest_point_spec(point_test)
  expect_named(output_spec, c("points", "distances"))
  expect_identical(dim(output_spec$points), c(1L, 2L))
  expect_identical(dim(output_spec$distances), c(2L, 1L))
  expect_equal(output_spec$point, spec_test)

  output2_spec <- closest_point_spec(point_test[, 1], point_test[, 2])
  expect_equal(output2_spec, output_spec)
})

# ==============================================================================
# Test Suite 4: Several closest points selection
# ==============================================================================


test_that("List of closest nodes selection works", {
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))

  point_test <- matrix(
    c(
      resourcecodedata::rscd_field[c(100, 1000), 2],
      resourcecodedata::rscd_field[c(100, 1000), 3]
    ),
    ncol = 2
  )
  node_test <- matrix(c(100, 101, 99, 1000, 1001, 999), ncol = 3, byrow = TRUE)
  closest <- 3

  output <- closest_point_field(point_test, closest = closest)
  expect_named(output, c("points", "distances"))
  expect_identical(dim(output$points), c(2L, 3L))
  expect_identical(dim(output$distances), c(2L, 3L))
  expect_equal(output$point, node_test)
  expect_true(all(output$distance[, 1] == 0))

  output2 <- closest_point_field(point_test[, 1], point_test[, 2], closest = closest)
  expect_equal(output2, output)

  output_spec <- closest_point_spec(point_test, closest = 3)
  expect_named(output_spec, c("points", "distances"))
  expect_identical(dim(output_spec$points), c(2L, 3L))
  expect_identical(dim(output_spec$distances), c(2L, 3L))

  output2_spec <- closest_point_spec(point_test[, 1], point_test[, 2], closest = 3)
  expect_equal(output2_spec, output_spec)
})
