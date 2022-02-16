################################################################################

test_that("file sizes are okay", {
  tmp <- file_create(tempfile(), 10)
  expect_equal(file.size(tmp), 10)
  file_resize(tmp, 5)
  expect_equal(file.size(tmp), 5)
  file_resize_off(tmp, 10)
  expect_equal(file.size(tmp), 15)
})

################################################################################

test_that("file_resize works with large files", {

  skip_on_cran()

  tmp <- file_create(tempfile(), 1e9)
  expect_equal(file.size(tmp), 1e9)
  file_resize(tmp, 3e9)
  expect_equal(file.size(tmp), 3e9)
  file_resize_off(tmp, 2e9)
  expect_equal(file.size(tmp), 5e9)
  file_resize_off(tmp, 2e9)
  expect_equal(file.size(tmp), 7e9)
})

################################################################################
