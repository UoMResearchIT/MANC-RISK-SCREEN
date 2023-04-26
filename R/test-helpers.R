#' Custom expectation, throws error if setdiff(a,b) is not empty
#' See https://testthat.r-lib.org/articles/custom-expectation.html
#'
#' @noRd
expect_subset_of <- function(obj_a, obj_b) {
  # 1. Capture object and label
  act_a <- testthat::quasi_label(rlang::enquo(obj_a), arg = "obj_a")
  act_b <- testthat::quasi_label(rlang::enquo(obj_b), arg = "obj_b")

  conflicts <- setdiff(act_a$val, act_b$val)
  testthat::expect(
    rlang::is_empty(conflicts),
    paste("Elements of",act_a$lab,"not in", act_b$lab,":",
          stringr::str_flatten_comma(conflicts))
  )

  invisible(conflicts)
}
