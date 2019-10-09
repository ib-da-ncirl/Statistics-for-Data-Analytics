source("../001_z-test_for_population_mean_variance_known.R", chdir = TRUE)
library(testthat)
context("001-z-test")

test_that("z.calculate", {
  
  # argument checks
  expect_error(z.calculate(5, 1, datadef(n=-1)),"Invalid.*")
  expect_error(z.calculate(5, 1, datadef(n=1.1)),"Invalid.*")
  
  # '100 Statistical Tests' pg 22
  z = z.calculate(mu0=4.0, sigma2=1.0**2, sampledef=datadef(mean=4.6, n=9))
  expect_equal(!!round(unname(z), 2), 1.8)
})

# http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-mean-known-variance
rtutor_2tail = c(xbar=14.6, n=35, mu0=15.4, sigma2=2.5**2, alpha=0.05)

# http://www.r-tutor.com/elementary-statistics/hypothesis-testing/upper-tail-test-population-mean-known-variance
rtutor_utail = c(xbar=2.1, n=35, mu0=2, sigma2=0.25**2, alpha=0.05)

# http://www.r-tutor.com/elementary-statistics/hypothesis-testing/lower-tail-test-population-mean-known-variance
rtutor_ltail = c(xbar=9900, n=30, mu0=10000, sigma2=120**2, alpha=0.05)


test_that("R-tutor Two-tail", {

  sampledef = datadef(mean=rtutor_2tail['xbar'], n=rtutor_2tail['n'])
  reject = z.hypthosis_test(type=hypothesis.twoTail, mu0=rtutor_2tail['mu0'], sigma2=rtutor_2tail['sigma2'],  
                            sampledef=sampledef, alpha=rtutor_2tail['alpha'])
  
  # result vector is all strings
  expect_match(reject['reject'], "FALSE")
})

test_that("R-tutor Upper-tail", {
  
  sampledef = datadef(mean=rtutor_utail['xbar'], n=rtutor_utail['n'])
  reject = z.hypthosis_test(type=hypothesis.upperTail, mu0=rtutor_utail['mu0'], sigma2=rtutor_utail['sigma2'], 
                            sampledef=sampledef, alpha=rtutor_utail['alpha'])
  
  # result vector is all strings
  expect_match(reject['reject'], "TRUE")
})

test_that("R-tutor Lower-tail", {
  
  sampledef = datadef(mean=rtutor_ltail['xbar'], n=rtutor_ltail['n'])
  reject = z.hypthosis_test(type=hypothesis.lowerTail, mu0=rtutor_ltail['mu0'], sigma2=rtutor_ltail['sigma2'], 
                            sampledef=sampledef, alpha=rtutor_ltail['alpha'])
  
  # result vector is all strings
  expect_match(reject['reject'], "TRUE")
})
