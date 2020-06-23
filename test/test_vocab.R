library("testthat")


test_that( "in", {
  expect_true( 3L %in% 1:4)
  expect_true( 3 %in% 1:4)
  expect_true( "3" %in% 1:4)
  expect_false( 0 %in% 1:4)
  expect_false( 5 %in% 1:4)
  expect_false( 3.5 %in% 1:4)
} )


test_that( "match", {
  expect_identical( 3L, !!match(4, 2:5))
  expect_identical( 3L, !!match(4L, 2:5))
  expect_identical( 3L, !!match("4", 2:5))
  expect_identical( NA_integer_, !!match(1, 2:5))
  expect_identical( NA_integer_, !!match(6, 2:5))
  expect_identical( NA_integer_, !!match(3.5, 2:5))

} )
