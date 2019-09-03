library("testthat")

test_that( "NULL behaves", {
  expect_null( NULL )
  expect_false( is.vector(NULL) ) # TODO types of null
  expect_equal( 0, length(NULL)) # TODO move
} )

test_that( "Primitive vectors", {
  expect_type( 1.5, "double" )
  expect_true(is.vector(1.5))

  explicit_double <- c(1, 2.5, 4.5)
  expect_type( explicit_double, "double" )
  expect_true(is.vector(explicit_double))

  implicit_double <- c(1,2,3)
  expect_type( implicit_double, "double" )
  expect_true(is.vector(implicit_double))

  # With the L suffix, you get an integer rather than a double
  explicit_int <- c(1L, 6L, 10L)
  expect_type( explicit_int, "integer" )
  expect_true( is.vector(explicit_int))

  # Use TRUE and FALSE (or T and F) to create logical vectors
  expect_equal( TRUE, T )
  expect_equal( FALSE, F )
  explicit_logical <- c(TRUE, FALSE, T, F)
  expect_equal( typeof( explicit_logical), "logical")
  expect_true(is.vector(explicit_logical))

  explicit_string <- c("these are", "some strings")
  expect_equal( typeof( explicit_string), "character" )
  expect_true( is.vector(explicit_string))
} )

test_that( "NA equality", {
  expect_true( is.logical(NA))
  expect_true( is.double(NA_real_))
  expect_true( is.character(NA_character_))
  expect_true( is.integer(NA_integer_))

  #equality
  # there is some nuance around the relationship between all.equal, isTRUE, and expect_equal
  expect_false( isTRUE( all.equal( NA, NA_character_ ) ) )
  expect_equal( NA_real_, NA_integer_ ) # etc
  expect_false( identical(NA, NA_integer_) ) # etc
} )

test_that( "NA coercion", {

  # coercion
  expect_true( is.logical(c(NA,T)[1]))
  expect_identical( NA, c(NA, T)[1])

  expect_true( is.double(c(NA,1)[1]))
  expect_identical( NA_real_, c(NA, 1)[1])

  expect_true( is.integer( c(NA, 1L)))
  expect_identical( NA_integer_, c(NA, 1L)[1])

  expect_true( is.character( c(NA, "a string")[1]))
  expect_identical( NA_character_, c(NA, "a string")[1])

} )

test_that( "type id", {

  expect_true( is.atomic(T) )
  expect_true( is.atomic(1) )
  expect_true( is.atomic(1L) )
  expect_true( is.atomic("a string") )
  expect_true( is.atomic(c(1,2,3)))

  expect_false( is.numeric(T) )
  expect_true( is.numeric(1) )
  expect_true( is.numeric(1L) )
  expect_false( is.numeric("a string") )
  expect_true( is.numeric(c(1,2,3)))
} )

test_that( "coercion", {

  expect_identical( 1, as.numeric(T)) # identical to as.double
  expect_identical( 1, as.double(T))
  expect_identical( 1L, as.integer(T))
  expect_identical( T, as.logical(T))

  expect_identical( 3, as.numeric(3))
  expect_identical( 3, as.double(3))
  expect_identical( 3L, as.integer(3.99))
  expect_identical( T, as.logical(3.5))

  expect_identical( 5, as.numeric(5L))
  expect_identical( 5, as.double(5L))
  expect_identical( 5L, as.integer(5))
  expect_identical( T, as.logical(5))

  expect_identical( 1, as.numeric(T))
  expect_identical( 1, as.double(TRUE))
  expect_identical( 1L, as.integer(T))
  expect_identical( T, as.logical(T))

  expect_identical( 0, as.numeric(FALSE))
  expect_identical( 0, as.double(F))
  expect_identical( 0L, as.integer(F))
  expect_identical( F, as.logical(F))

  expect_identical( NA_real_, as.numeric("TRUE"))
  expect_identical( NA_real_, as.double("T"))
  expect_identical( NA_integer_, as.integer("three"))
  expect_identical( NA, as.logical("three"))

  #expect_equal( 3.141592606, pi )
  #expect_false( all.equal( 3, pi ))
  #
  # special cases
  expect_identical( TRUE, as.logical("TRUE"))
  expect_identical( FALSE, as.logical("F"))

  expect_identical( "3.5", as.character(3.5))
  expect_identical( "3", as.character(3))
  expect_identical( "3", as.character(3L))
  expect_identical( "TRUE", as.character(T))
  expect_identical( "FALSE", as.character(FALSE))
  expect_identical( "a string", as.character("a string"))

  # todo more character as.other cases
  expect_identical( 3.5, as.numeric("3.5"))
} )

test_that( "lists", {
  x <- list(1:3, c("a","bc"), c(TRUE, FALSE, TRUE), c(2.3, 5.9))
  expect_type( x, "list")
  expect_false( is.atomic(x) )
  expect_true( is.recursive(x) )
  expect_true( is.vector(x))
  expect_equal( 4, length(x) )

  expect_type( x[2], "list")
  expect_type( x[[2]], "character" )

  y <- list(1:3, c("a","bc"), c(TRUE, FALSE, TRUE), c(2.3, 5.9))
  expect_type( c(x,y), "list" )
  expect_equal( 8, length( c(x,y) ) )

  expect_type( c(x,"y"), "list" )
  expect_equal( 5, length( c(x,"y") ) )

  x <- list(list(1, 2), c(3, 4))
  expect_equal( 2, length(x) )
  y <- c(list(1, 2), c(3, 4))
  expect_equal(4, length(y) )

  expect_type(unlist(list(1,2)), "double")
  expect_type(unlist(list(1,"2")), "character")
} )

test_that( "attributes", {
  y <- 1:10
  expect_null(attributes(y))
  expect_null(attr(y,"an_attribute"))
  attr(y,"an_attribute") <- "assign an attribute to a vector"
  expect_type( attributes(y),"list")
  expect_length(attributes(y),1)
  expect_equal( attr( y, "an_attribute" ), "assign an attribute to a vector" )
  z <- structure(y, second_attribute="another value")
  expect_equal( attr( z, "an_attribute" ), "assign an attribute to a vector" )
  z <- structure(y, an_attribute="another value")
  expect_equal( attr( z, "an_attribute" ), "another value" )
  expect_identical( 'a', c('a'))
  print("Success!")
} )

  #TODO raw, complex maybe

test_that( "factors behave", {
  x <- factor( c("it", "she", "he") )
  expect_type(x, "integer")
  expect_identical( c("he","it","she"), levels(x) )
  expect_false( is.ordered(x) )
  expect_warning( x[1] < x[2] )
  expect_identical( NA, x[1] < x[2] )
  expect_identical( 2L, as.integer(x[1]))

  x <- ordered( c("it", "she", "he") )
  expect_type( x, "integer")
  expect_identical( c("he","it","she"), levels(x) )
  expect_true( is.ordered(x) )
  expect_silent( x[1] < x[2] )
  expect_true( x[1] < x[2] )
  expect_identical( 2L, as.integer(x[1]))

  specified_ordered <- ordered( c("it", "she", "he"), levels = c("he", "she", "it") )
  expect_type(specified_ordered, "integer")
  expect_identical( c("he","she","it"), levels(specified_ordered) )
  expect_true( is.ordered(specified_ordered) )
  expect_silent( specified_ordered[1] < specified_ordered[2] )
  expect_false( specified_ordered[1] < specified_ordered[2] )
  expect_identical( 3L, as.integer(specified_ordered[1]))


  expect_equal(1L, as.integer( specified_ordered[3] ) )
  expect_equal("he", as.character( specified_ordered[3] ) )

  levels(specified_ordered) <- c("a", "quick", "brown" )
  expect_identical( c("a","quick","brown"), levels(specified_ordered) )
  expect_equal("a", as.character( specified_ordered[3] ) )
} )

test_that( "vector to matrix and array", {
  some_vec <- 1:30
  expect_type( some_vec, "integer" )
  expect_equal( "integer", class(some_vec) )
  expect_null( dim( some_vec ) )

  dim( some_vec ) <- NULL
  expect_type( some_vec, "integer" )
  expect_equal( "integer", class(some_vec) )

  dim( some_vec ) <- c(10,3)
  expect_equal( c(10,3), dim( some_vec))
  expect_equal( "matrix", class(some_vec) )

  dim( some_vec ) <- c(2,3,5)
  expect_equal( c(2,3,5), dim( some_vec))
  expect_equal( "array", class(some_vec) )

  expect_error( { dim(some_vec) <- c(3,5) } )
  expect_error( { dim(some_vec) <- c(6,10) } )
} )

test_that( "matrix to array and vector", {
  some_vec <- matrix( 1:30 )
  expect_equal( c(30,1), dim(some_vec) )
  expect_equal( "matrix", class(some_vec) )

  dim( some_vec ) <- NULL
  expect_type( some_vec, "integer" )
  expect_equal( "integer", class(some_vec) )

  dim( some_vec ) <- c(10,3)
  expect_equal( c(10,3), dim( some_vec))
  expect_equal( "matrix", class(some_vec) )

  dim( some_vec ) <- c(2,3,5)
  expect_equal( c(2,3,5), dim( some_vec))
  expect_equal( "array", class(some_vec) )
} )

test_that( "array to vector and matrix", {
    some_vec <- array( 1:30 )
    expect_equal( 30, !!dim(some_vec) ) # one-dim by default
    expect_equal( "array", class(some_vec) )

    dim( some_vec ) <- NULL
    expect_type( some_vec, "integer" )
    expect_equal( "integer", class(some_vec) )

    dim( some_vec ) <- c(10,3)
    expect_equal( c(10,3), dim( some_vec))
    expect_equal( "matrix", class(some_vec) )

    dim( some_vec ) <- c(2,3,5)
    expect_equal( c(2,3,5), dim( some_vec))
    expect_equal( "array", class(some_vec) )
} )

test_that( "array to vector and matrix", {
  some_vec <- array( 1:30 )
  expect_equal( 30, !!dim(some_vec) ) # one-dim by default
  expect_equal( "array", class(some_vec) )

  dim( some_vec ) <- NULL
  expect_type( some_vec, "integer" )
  expect_equal( "integer", class(some_vec) )

  dim( some_vec ) <- c(10,3)
  expect_equal( c(10,3), dim( some_vec))
  expect_equal( "matrix", class(some_vec) )
  expect_true( is.array( some_vec))

  dim( some_vec ) <- c(2,3,5)
  expect_equal( c(2,3,5), dim( some_vec))
  expect_equal( "array", class(some_vec) )

} )

test_that("I", {
  x <- list( 1:3,c("a","b","c") )
  expect_identical( "AsIs", class(I(x)) )
} )

test_that("data frames", {
  df <- data.frame(x = 1:4, y = c("a", "b", "c", "d"), banana=c("b","an","an","as") )
  expect_type(df,"list")
  expect_s3_class(df,"data.frame")
  expect_identical( df[1], data.frame(x=1:4) )
  expect_identical( df[[1]], 1:4 )
  expect_s3_class( df[["y"]], "factor" )
  expect_type(df[["y"]], "integer")
  expect_true( is.data.frame( df ) )

  df <- data.frame(x = 1:4, y = c("a", "b", "c", "d"), banana=c("b","an","an","as"), stringsAsFactors = F )
  expect_type(df[["y"]], "character")

  expect_null(df[["foo"]])
  expect_error( data.frame( 1:4, c("a","b","c") ) )


  ef <- data.frame(x = 1:4, y = c("a", "b", "c", "d"), banana=c("b","an","an","as") )

  ff <- rbind(df,ef)

  gg <- cbind(ef,df)
  print(ff)

  dfm <- data.frame(x = 1:3, y = matrix(1:9, ncol = 3) )
  expect_identical(c(3L,2L),!!dim(dfm))
  expect_identical( I(matrix(1:9, ncol = 3)), dfm[[2]] )

})

test_that( "names", {
  # TODO
})
