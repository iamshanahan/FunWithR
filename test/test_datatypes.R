test_that( "data structures", {

  dbl_var <- c(1, 2.5, 4.5)
  exp
  ambi_var <- c(1,2,3)

  # With the L suffix, you get an integer rather than a double
  int_var <- c(1L, 6L, 10L)
  # Use TRUE and FALSE (or T and F) to create logical vectors
  log_var <- c(TRUE, FALSE, T, F)
  chr_var <- c("these are", "some strings")


} )

test_that( "datatypes behave", {

  assertthat::are_equal(3,3)
  expect_equal(sin(pi / 4), 1 / sqrt(2))
  expect_equal(cos(pi / 4), 1 / sqrt(2))
  expect_equal(tan(pi / 4), 1)

} )
