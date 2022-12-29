
test_that("has_internet() works", {
  skip_if_offline()
  expect_true(has_internet())
})
