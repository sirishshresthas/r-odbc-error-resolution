
test_that("Validate table name", {
  conn <- init_connection()
  exists <- table_exists(conn=conn, table_name="BMK2012_View_All")
  expect_identical(exists, TRUE)
  end_connection(conn)
})


test_that("Bad table name", {
  conn <- init_connection()
  exists <- table_exists(conn=conn, table_name='test')
  expect_identical(exists, FALSE)
  end_connection(conn)
})

