library("testthat")

test_that( "vector subsetting", {
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
  expect_equivalent( c(3.3, 4.2, 3.3), y[c(3,2,3)])
  expect_equivalent( c(NA, NA, 3.3), y[c(3L, 2L, "c")]) # coercion to all chars
  expect_equivalent( c(NA, 3.3, NA), y[c("f","c","f")])

} )


test_that( "matix subsetting", {
  a <- matrix(1:12, nrow = 4)
  colnames(a) <- c("A", "B", "C")
  expect_identical( 12L, a[12])
  expect_equivalent( 12L, a[4,3]) #ignore column name
  expect_identical( "C", attr(a[4,3],"names"))
  expect_equivalent( c(2L,6L,10L), a[2,])
  expect_identical( c("A","B","C"), !!attr(a[2,],"names"))
  expect_identical( c(5L,6L,7L,8L),a[,2])
  expect_identical( c(5L,6L,7L,8L),a[,"B"])
  expect_identical( c(5L,6L,7L,8L),a[,c(F,T,F)])
  expect_identical( c(5L,6L,7L,8L),a[,c(-1,-3)])
} )

test_that( "dataframe subsetting", {
  df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
  expect_type( df[1],"list" )
  expect_identical( "x", attributes(df[1])[["names"]] )
  expect_identical( 1:3, df[[1]])
  expect_identical( "x", attributes(df["x"])[["names"]] )
  expect_identical( 1:3, df[["x"]])

  expect_identical( 1:3, df[,1])
  expect_identical( 1:3, df[,"x"])

  expect_identical( 1:3, df$x)
} )
