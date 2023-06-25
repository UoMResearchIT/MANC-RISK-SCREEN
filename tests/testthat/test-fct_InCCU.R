test_that("Inc. Cost-Util. table & plot don't crash", {

  expect_no_error({
      output_df <- get_incCU_table(c("foo","bar","baz"), c(1200,1350,1400), c(10.01, 10.02, 10.03))
      pretty_incCU_table(output_df,20e3)
      print(plot_ce_table(output_df))
  })
})


test_that("seq_through behaves", {
  expect_null(seq_through(1,2,3,1))
  expect_null(seq_through(3,1,2,1))
  expect_equal(seq_through(1,10,4,3), c(1,4,7,10))
  expect_error(seq_through(1,10,4,-3))
})
