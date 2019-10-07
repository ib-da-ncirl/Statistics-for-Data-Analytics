source("../007_t-test_for_population_mean_variance_unknown.R", chdir = TRUE)
library(testthat)
context("007-t-test")

# http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-mean-unknown-variance
rtutor_2tail = c(xbar=14.6, n=35, mu0=15.4, sigma=2.5, alpha=0.05)

# http://www.r-tutor.com/elementary-statistics/hypothesis-testing/upper-tail-test-population-mean-unknown-variance
rtutor_utail = c(xbar=2.1, n=35, mu0=2, sigma=0.3, alpha=0.05)

# http://www.r-tutor.com/elementary-statistics/hypothesis-testing/lower-tail-test-population-mean-unknown-variance
rtutor_ltail = c(xbar=9900, n=30, mu0=10000, sigma=125, alpha=0.05)


test_that("R-tutor Two-tail", {
  
  reject = t.hypthosis_test(type=hypothesis.twoTail, xbar=rtutor_2tail['xbar'], n=rtutor_2tail['n'], 
                          mu0=rtutor_2tail['mu0'], sigma2=NA, sigma=rtutor_2tail['sigma'], alpha=rtutor_2tail['alpha'])
  
  # result vector is all strings
  expect_match(reject['reject'], "FALSE")
})

test_that("R-tutor Upper-tail", {
  
  reject = t.hypthosis_test(type=hypothesis.upperTail, xbar=rtutor_utail['xbar'], n=rtutor_utail['n'], 
                          mu0=rtutor_utail['mu0'], sigma2=NA, sigma=rtutor_utail['sigma'], alpha=rtutor_utail['alpha'])
  
  # result vector is all strings
  expect_match(reject['reject'], "TRUE")
})

test_that("R-tutor Lower-tail", {
  
  reject = t.hypthosis_test(type=hypothesis.lowerTail, xbar=rtutor_ltail['xbar'], n=rtutor_ltail['n'], 
                          mu0=rtutor_ltail['mu0'], sigma2=NA, sigma=rtutor_ltail['sigma'], alpha=rtutor_ltail['alpha'])
  
  # result vector is all strings
  expect_match(reject['reject'], "TRUE")
})
