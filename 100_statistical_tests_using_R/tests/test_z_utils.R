source("../z_utils.R", chdir = TRUE)
library(testthat)
context("z utils")

cv_alpha = c(0.1,   0.05,  0.025, 0.01,  0.005)
cv_zval_lu = c(1.282, 1.645, 1.960, 2.326, 2.576) # upper/lower tail zval
cv_zval_t  = c(1.645, 1.960, 2.241, 2.576, 2.807) # two tail zval


test_that("z.get_critical_values", {
  # couple of sanity checks
  if (length(cv_alpha) != length(cv_zval_lu)) {
    test_that("test data", fail("length(cv_alpha) != length(cv_zval_lu)"))
  }
  if (length(cv_alpha) != length(cv_zval_t)) {
    test_that("test data", fail("length(cv_alpha) != length(cv_zval_t)"))
  }
  
  for (i in 1:length(cv_alpha)) {
    for (type in hypothesis.valid_types) {
      cv = z.get_critical_values(cv_alpha[i], rounding=3, type=type)
      if (hypothesis.isTwoTail(type)) {
        expect_equal(unname(!!cv['lower']), !!-cv_zval_t[i])
        expect_equal(unname(!!cv['upper']), !!cv_zval_t[i])
      } else if (hypothesis.isLowerTail(type)) {
        expect_equal(unname(!!cv['lower']), !!-cv_zval_lu[i])
      } else if (hypothesis.isUpperTail(type)) {
        expect_equal(unname(!!cv['upper']), !!cv_zval_lu[i])
      }
    }
  }

  # argument checks
  expect_error(z.get_critical_values(0),"Invalid.*")
  expect_error(z.get_critical_values(1),"Invalid.*")
  expect_error(z.get_critical_values(0.05, 0),"Invalid.*")  # rounding check
  expect_error(z.get_critical_values(0.05, type='invalid'),"Invalid.*")  # type check
  expect_error(z.get_critical_values(0.05, type=NA),"Invalid.*")  # type check
})



