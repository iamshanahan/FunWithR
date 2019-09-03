library("testthat")

test_that( "NULL behaves", {
  x <- c(2.1, 4.2, 3.3, 5.4)
  expect_identical(c(3.3, 4.2), x[c(3,2)])
  expect_identical(c(4.2, 3.3), x[c(F,T,T,F)])
  expect_identical(c(4.2, 3.3), x[c(-1,-4)])
  expect_identical(c(4.2, 3.3), x[-c(1,4)])
  expect_identical(c(3.3, 3.3, 3.3), x[c(3,3,3)])
  expect_identical(c(3.3, 3.3, 3.3), x[c(0,3,3,3)])
  expect_identical(x, x[])
  expect_identical(numeric(0), x[0])

  (y <- setNames(x, letters[1:4]))
  expect_equivalent( c(3.3, 4.2, 3.3), y[c("c","b","c")]) #equivalent ignores labels
  expect_equivalent( c(NA, 3.3, NA), y[c("f","c","f")])

} )
