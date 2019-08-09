context("test-metreport")

test_that("int{Min,Max} works", {

  intMin <- function(x){
    round(min(x,na.rm = TRUE), digits = 1)
  }

  intMax <- function(x){
    round(max(x,na.rm = TRUE), digits = 1)
  }

  expect_equal(intMin(seq(0.01,1,by = 0.01)), 0)
  expect_equal(intMax(seq(0.01,1,by = 0.01)), 1)
})


