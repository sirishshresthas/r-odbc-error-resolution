
test_that("Test query creating", {
  
  conn <- init_connection()
  
  query <- create_query_with_column_names(conn, query=NULL, table_name='DSI_360BDView_Items')
  expect_match(query, "select|SELECT.*")
  end_connection(conn)
  
})


