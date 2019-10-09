source("../003_z-test_for_two_population_mean_variance_known.R", chdir = TRUE)
library(testthat)
context("003-z-test")

test_that("z.calculate2sam", {
  
  # argument checks
  valid_mean_var = datadef(mean=0, var=1)
  invalid_mean_var = datadef(mean=0, var=-1)
  valid_mean_n = datadef(mean=0, n=10)
  invalid_mean_n1 = datadef(mean=0, n=-10)
  invalid_mean_n2 = datadef(mean=0, n=10.1)
  
  expect_error(z.calculate2sam(popdef1=invalid_mean_var, popdef2=valid_mean_var, 
                               sampledef1=valid_mean_n, sampledef2=valid_mean_n), "Invalid.*popdef1.*")

  expect_error(z.calculate2sam(popdef1=valid_mean_var, popdef2=invalid_mean_var, 
                               sampledef1=valid_mean_n, sampledef2=valid_mean_n), "Invalid.*popdef2.*")
  
  for (ddef in c(invalid_mean_n1, invalid_mean_n2)) {
    expect_error(z.calculate2sam(popdef1=valid_mean_var, popdef2=valid_mean_var, 
                                 sampledef1=ddef, sampledef2=valid_mean_n), "Invalid.*sampledef1.*")
    expect_error(z.calculate2sam(popdef1=valid_mean_var, popdef2=valid_mean_var, 
                                 sampledef1=valid_mean_n, sampledef2=ddef), "Invalid.*sampledef2.*")
  }

  # '100 Statistical Tests' pg 25
  z = z.calculate2sam(popdef1=datadef(mean=0, var=0.000576), popdef2=datadef(mean=0, var=0.001089), 
                      sampledef1=datadef(mean=80.02, n=13), sampledef2=datadef(mean=79.98, n=8))
  expect_equal(!!round(unname(z), 2), 2.98)
})

# 'Statistics for dummies" pg 246 Stats-absorbent v Sponge-0-matic example
paper_towels = c(xbar1=3, sigma1=0.9, n1=50, xbar2=3.5, sigma2=1.2, n2=50)

test_that("Stats for dummies[paper towels] Two-tail", {
  
  reject = z.2sam_hypthosis_test(type=hypothesis.twoTail, popdef1=datadef(var=paper_towels['sigma1']**2), 
                                 popdef2=datadef(var=paper_towels['sigma2']**2), 
                                 sampledef1=datadef(mean=paper_towels['xbar1'], n=paper_towels['n1']), 
                                 sampledef2=datadef(mean=paper_towels['xbar2'], n=paper_towels['n2']), 
                                 alpha=0.05)

  # result vector is all strings
  expect_match(reject['reject'], "TRUE")
})




  


