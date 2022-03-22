
test_that("connection to db", {
  
  res <- init_connection()
  testthat::expect_equal(length(res), 1)
  end_connection(res)

})

