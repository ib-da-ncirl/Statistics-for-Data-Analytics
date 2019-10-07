source("../t_utils.R", chdir = TRUE)
library(testthat)
context("t utils")

# Return the numeric value of 'val' for the specified alpha 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param alpha: alpha value to look up
# :param val: name of value to return
# :return numeric value
get_for_alpha = function(type, df, alpha, val) {
  return(unname(t.calc_alt(type, df, alpha)[val]))
} 

# Return the numeric value of 'alpha' for the specified alpha 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param alpha: alpha value to look up
# :return alpha value
get_alpha = function(type, df, alpha) {
  return(get_for_alpha(type, df, alpha, 'alpha'))
} 

# Return the numeric value of 'level' for the specified alpha 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param alpha: alpha value to look up
# :return level value
get_level_for_alpha = function(type, df, alpha) {
  return(get_for_alpha(type, df, alpha, 'level'))
} 

# Return the numeric value of 'tval' for the specified alpha 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param alpha: alpha value to look up
# :return tval value
get_tval_for_alpha = function(type, df, alpha) {
  return(get_for_alpha(type, df, alpha, 'tval'))
} 

# Return the numeric value of 'val' for the specified level
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param level: level value to look up
# :param val: name of value to return
# :return numeric value
get_for_level = function(type, df, level, val) {
  return(unname(t.calc_alt(type, df, level_req=level)[val]))
} 

# Return the numeric value of 'level' for the specified level 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param level: level value to look up
# :return level value
get_level = function(type, df, level) {
  return(get_for_level(type, df, level, 'level'))
} 

# Return the numeric value of 'alpha' for the specified level 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param level: level value to look up
# :return alpha value
get_alpha_for_level = function(type, df, level) {
  return(get_for_level(type, df, level, 'alpha'))
} 

# Return the numeric value of 'tval' for the specified level 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param level: level value to look up
# :return tval value
get_tval_for_level = function(type, df, level) {
  return(get_for_level(type, df, level, 'tval'))
} 



# test data
alpha = c(0.025, 0.01, 0.005)
level = c(97.5,  99,   99.5)
df    = c(9,     19,   29)
type  = c(hypothesis.upperTail, hypothesis.lowerTail, hypothesis.twoTail)
tval  = c(2.26,  -2.54, round(2.756*2,2))

test_that("t.calc_alt", {
  # couple of sanity checks
  if (length(alpha) != length(level)) {
    test_that("test data", fail("length(alpha) != length(level)"))
  }
  if (length(alpha) != length(df)) {
    test_that("test data", fail("length(alpha) != length(df)"))
  }
  if (length(alpha) != length(type)) {
    test_that("test data", fail("length(alpha) != length(type)"))
  }
  if (length(alpha) != length(tval)) {
    test_that("test data", fail("length(alpha) != length(tval)"))
  }

  for (i in 1:length(alpha)) {
    # alpha
    expect_equal(get_alpha(type[i], df[i], alpha[i]), alpha[i])
    expect_equal(get_level_for_alpha(type[i], df[i], alpha[i]), level[i])
    expect_equal(get_tval_for_alpha(type[i], df[i], alpha[i]), tval[i])
    # level
    expect_equal(get_level(type[i], df[i], level[i]), level[i])
    expect_equal(get_alpha_for_level(type[i], df[i], level[i]), alpha[i])
    expect_equal(get_tval_for_level(type[i], df[i], level[i]), tval[i])
  }
  
  # argument checks
  expect_error(t.calc_alt(hypothesis.upperTail, 8, alpha_req=0),"Invalid.*")
  expect_error(t.calc_alt(hypothesis.upperTail, 8, alpha_req=1),"Invalid.*")
  expect_error(t.calc_alt(hypothesis.upperTail, 8, level_req=0),"Invalid.*")
  expect_error(t.calc_alt(hypothesis.upperTail, 8, level_req=100),"Invalid.*")
})




  
cv_alpha      = c(0.1,   0.05,  0.025, 0.01,  0.005)
# upper tail value
cv_tval_10_lu = c(1.372, 1.812, 2.228, 2.764, 3.169)  # upper/lower tail tvals for 10 df 
cv_tval_29_lu = c(1.311, 1.699, 2.045, 2.462, 2.756)  # upper/lower tail tvals for 29 df
# two tail value
cv_tval_10_t  = c(1.812, 2.228, 2.634, 3.169, 3.581)  # two tail tvals for 10 df
cv_tval_29_t  = c(1.699, 2.045, 2.364, 2.756, 3.038)  # two tail tvals for 29 df
cv_df = c(10, 29)

test_that("t.get_critical_values", {
  # couple of sanity checks
  if (length(cv_alpha) != length(cv_tval_10_lu)) {
    test_that("test data", fail("length(cv_alpha) != length(cv_tval_10_lu)"))
  }
  if (length(cv_alpha) != length(cv_tval_10_t)) {
    test_that("test data", fail("length(cv_alpha) != length(cv_tval_10_t)"))
  }
  if (length(cv_alpha) != length(cv_tval_29_lu)) {
    test_that("test data", fail("length(cv_alpha) != length(cv_tval_29_lu)"))
  }
  if (length(cv_alpha) != length(cv_tval_29_t)) {
    test_that("test data", fail("length(cv_alpha) != length(cv_tval_29_t)"))
  }
  
  for (df in cv_df) {
    if (df == 10) {
      tvals_lu = cv_tval_10_lu
      tvals_t = cv_tval_10_t
    } else if (df == 29) {
      tvals_lu = cv_tval_29_lu
      tvals_t = cv_tval_29_t
    } else {
      test_that("test data", fail(paste0(c("unknown df", df,  collapse=" "))))
    }
    for (i in 1:length(cv_alpha)) {
      for (type in hypothesis.valid_types) {
        cv = t.get_critical_values(cv_alpha[i], df, rounding=3, type=type)
        if (hypothesis.isTwoTail(type)) {
          expect_equal(unname(!!cv['lower']), !!-tvals_t[i])
          expect_equal(unname(!!cv['upper']), !!tvals_t[i])
        } else if (hypothesis.isLowerTail(type)) {
          expect_equal(unname(!!cv['lower']), !!-tvals_lu[i])
        } else if (hypothesis.isUpperTail(type)) {
          expect_equal(unname(!!cv['upper']), !!tvals_lu[i])
        }
      }
    }
  }
  
  # argument checks
  expect_error(t.get_critical_values(0, 8),"Invalid.*")
  expect_error(t.get_critical_values(1, 8),"Invalid.*")
  expect_error(t.get_critical_values(0.05, 0),"Invalid.*")     # df check
  expect_error(t.get_critical_values(0.05, 3, 0),"Invalid.*")  # rounding check
  expect_error(t.get_critical_values(0.05, 3, type='invalid'),"Invalid.*")  # type check
  expect_error(t.get_critical_values(0.05, 3, type=NA),"Invalid.*")  # type check
})

