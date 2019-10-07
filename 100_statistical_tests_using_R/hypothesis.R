source("utils.R")

# Test if H0 should be rejected
# :param alpha: significance level, e.g. 0.05 == 95% confidence level
# :param pvalue: p-value to check
# :return TRUE if H0 should be rejected
p.rejectH0 = function(alpha, pvalue) {
  # TODO add more significance levels
  # guideline values based on 'Statistics for Dummies', but of course its subjective
  level            = c(0.05)  # significance level
  very_significant = c(0.01)  # very significant if less than
  significant      = c(0.049) # significant if less than
  marginal         = c(0.051) # marginal if less than
  significance_vals = data.frame(level, very_significant, significant, marginal)
  
  test_vals = significance_vals[which(significance_vals$level == alpha),]
  
  reject = c(reject=NA, strength=NA)
  if (is.list(test_vals) & (nrow(test_vals) == 1)) {
    if (pvalue <= test_vals['very_significant']) {
      reject['reject'] = TRUE
      reject['strength'] = 'very significant'
    } else if (pvalue < test_vals['significant']) {
      reject['reject'] = TRUE
      reject['strength'] = 'significant'
    } else if ((pvalue >= test_vals['significant']) & (pvalue <= test_vals['marginal'])) {
      reject['reject'] = TRUE
      reject['strength'] = 'marginal'
    } else {
      reject['reject'] = FALSE
      reject['strength'] = 'insufficient'
    }
  }
  return(reject)
}

hypthosis_test.type = 0x01 
hypthosis_test.xbar = 0x02
hypthosis_test.n    = 0x04
hypthosis_test.mu0  = 0x08
hypthosis_test.sigma2 = 0x10
hypthosis_test.sigma  = 0x20
hypthosis_test.alpha  = 0x40
hypthosis_test.ip.all = c(hypthosis_test.type, hypthosis_test.xbar, hypthosis_test.n,
                          hypthosis_test.mu0, hypthosis_test.sigma2, hypthosis_test.sigma,
                          hypthosis_test.alpha)
hypthosis_test.z.mask = c(hypthosis_test.type, hypthosis_test.xbar, hypthosis_test.n,
                          hypthosis_test.mu0, hypthosis_test.sigma2, hypthosis_test.alpha)
hypthosis_test.t.mask = c(hypthosis_test.type, hypthosis_test.xbar, hypthosis_test.n,
                          hypthosis_test.mu0, hypthosis_test.sigma, hypthosis_test.alpha)


# Get input for a hypothesis test
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param xbar: sample mean
# :param n: sample size
# :param mu0: population mean
# :param sigma2: population variance
# :param sigma: population standard deviation
# :param alpha: significance level
hypthosis_test_input = function(required, type=NA, xbar=NA, n=NA, mu0=NA, sigma2=NA, sigma=NA, alpha=NA) {
  
  quit = FALSE
  if (hypthosis_test.type %in% required) {
    type = hypothesis.get_test_type(type)
  }
  if (hypthosis_test.xbar %in% required) {
    if (is.na(xbar)) {
      xbar = get_input_num('Sample mean (x-bar)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(xbar) == 'q')
    }
  }
  if (!quit & hypthosis_test.n %in% required) {
    if (is.na(n)) {
      n = get_input_num('Sample size (n)', min_val=-Inf, max_val=Inf, int_only=TRUE, quit='q')
      quit = (tolower(n) == 'q')
    }
  }
  if (!quit & hypthosis_test.mu0 %in% required) {
    if (is.na(mu0)) {
      mu0 = get_input_num('Population mean (mu0)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(mu0) == 'q')
    }
  }
  if (!quit & hypthosis_test.sigma2 %in% required) {
    if (is.na(sigma2)) {
      sigma2 = get_input_num('Population variance (sigma2)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(sigma2) == 'q')
    }
  }
  if (!quit & hypthosis_test.sigma %in% required) {
    if (is.na(sigma)) {
      sigma = get_input_num('Population standard deviation (sigma)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(sigma) == 'q')
    }
  }
  if (!quit & hypthosis_test.alpha %in% required) {
    if (is.na(alpha)) {
      # TODO add more significance levels
      alpha = get_input_num('Significance level (alpha)', min_val=0.05, max_val=0.05, int_only=FALSE, quit='q')
      quit = (tolower(alpha) == 'q')
    }
  }

  if (!quit) {
    result = c(NA, NA, NA, NA, NA, NA, NA)
    names(result) = c('type', 'xbar', 'n', 'mu0', 'sigma2', 'sigma', 'alpha')
    result['type'] = utf8ToInt(tolower(type)) # need to convert to numeric as vector is all same type
    result['xbar'] = xbar
    result['n'] = n
    result['df'] = n - 1
    result['mu0'] = mu0
    result['sigma2'] = sigma2
    result['sigma'] = sigma
    result['alpha'] = alpha
  } else {
    result = NA
  }
  return(result)
}

