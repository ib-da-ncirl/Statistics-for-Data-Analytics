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

# Do a hypothesis test
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param pvalue: p-value
# :param alpha: significance level
# :param display: Display result; default is TRUE
# :param h0: null hypothesis elements in vector form, e.g. c('mu', 'mu0')
hypthosis_test.do = function(type, pvalue, alpha, display=TRUE, h0=c('mu', 'mu0')) {

  reject = p.rejectH0(alpha, pvalue)

  if (display) {
    h0_phrase = paste0("h0_", h0[1], "==", h0[2])
    
    if (hypothesis.isTwoTail(type)) {
      # Ha : sample mean != population mean
      h1_phrase = paste0("h1_", h0[1], "!=", h0[2])
      name = "Two tail"
      
    } else if (hypothesis.isLowerTail(type)) {
      # Ha : sample mean < population mean
      h1_phrase = paste0("h1_", h0[1], "<", h0[2])
      name = "Lower tail"
      
    } else if (hypothesis.isUpperTail(type)) {
      # Ha : sample mean > population mean
      h1_phrase = paste0("h1_", h0[1], ">", h0[2])
      name = "Upper tail"
      
    }
    # H0 : sample mean == population mean
    print(gen_phrase(h0_phrase))
    print(gen_phrase(h1_phrase))
    
    print(paste(name, "p-value result", pvalue, sep = " "))
    print(paste("Probability of", unicode_chars("h0"), "=", paste(round((pvalue*100), 3), "%", sep=""), sep = " "))
    conclusion = paste("Result is statistically ", reject['strength'], ",", sep = "")
    alpha_str = paste("at", gen_phrase("alpha==", alpha), sep = " ")
    if (reject['reject']) {
      h0_str = paste(unicode_chars("h0"), "rejected", sep = " ")
    } else {
      h0_str = paste(unicode_chars("h0"), "not rejected", sep = " ")
    }
    print(paste(conclusion, h0_str, alpha_str, sep = " "))
  }

  return(reject)
}


hypthosis_test.type   = 1 
hypthosis_test.xbar   = 2  # sample mean
hypthosis_test.s2     = 3  # sample variance
hypthosis_test.s      = 4  # sample standard deviation
hypthosis_test.n      = 5  # sample size
hypthosis_test.mu0    = 6  # population mean
hypthosis_test.sigma2 = 7  # population variance
hypthosis_test.sigma  = 8  # population standard deviation
hypthosis_test.alpha  = 9
hypthosis_test.ip.all = c(hypthosis_test.type, 
                          hypthosis_test.xbar, hypthosis_test.s2, hypthosis_test.s,
                          hypthosis_test.n,
                          hypthosis_test.mu0, hypthosis_test.sigma2, hypthosis_test.sigma,
                          hypthosis_test.alpha)
hypthosis_test.z.mask = c(hypthosis_test.type, hypthosis_test.xbar, hypthosis_test.n,
                          hypthosis_test.mu0, hypthosis_test.sigma2, hypthosis_test.alpha)
hypthosis_test.t.mask = c(hypthosis_test.type, hypthosis_test.xbar, hypthosis_test.n,
                          hypthosis_test.mu0, hypthosis_test.sigma, hypthosis_test.alpha)
hypthosis_test.type_alpha.mask = c(hypthosis_test.type, hypthosis_test.alpha)
hypthosis_test.pop_mean_var.mask = c(hypthosis_test.mu0, hypthosis_test.sigma2)
hypthosis_test.sam_mean_n.mask = c(hypthosis_test.xbar, hypthosis_test.n)


# Get input for a hypothesis test
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param xbar: sample mean
# :param s2: sample variance
# :param s: sample standard deviation
# :param n: sample size
# :param mu0: population mean
# :param sigma2: population variance
# :param sigma: population standard deviation
# :param alpha: significance level
# :param heading: Heading string to display
hypthosis_test_input = function(required, type=NA, 
                                xbar=NA, s2=NA, s=NA, n=NA, 
                                mu0=NA, sigma2=NA, sigma=NA, 
                                alpha=NA, heading=NA) {
  if (!is.na(heading)) {
    print(heading)
  }
  
  quit = FALSE
  if (hypthosis_test.type %in% required) {
    type = hypothesis.get_test_type(type)
  }
  if (hypthosis_test.xbar %in% required) {
    if (is.na(xbar)) {
      xbar = get_input_num('Sample mean (xbar)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(xbar) == 'q')
    }
  } else {
    xbar = NA
  }
  if (hypthosis_test.s2 %in% required) {
    if (is.na(s2)) {
      s2 = get_input_num('Sample variance (s2)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(s2) == 'q')
    }
  } else {
    s2 = NA
  }
  if (hypthosis_test.s %in% required) {
    if (is.na(s)) {
      s = get_input_num('Sample standard deviation (s)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(s) == 'q')
    }
  } else {
    s = NA
  }
  if (!quit & hypthosis_test.n %in% required) {
    if (is.na(n)) {
      n = get_input_num('Sample size (n)', min_val=-Inf, max_val=Inf, int_only=TRUE, quit='q')
      quit = (tolower(n) == 'q')
    }
  } else {
    n = NA
  }
  if (!quit & hypthosis_test.mu0 %in% required) {
    if (is.na(mu0)) {
      mu0 = get_input_num('Population mean (mu0)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(mu0) == 'q')
    }
  } else {
    mu0 = NA
  }
  if (!quit & hypthosis_test.sigma2 %in% required) {
    if (is.na(sigma2)) {
      sigma2 = get_input_num('Population variance (sigma2)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(sigma2) == 'q')
    }
  } else {
    sigma2 = NA
  }
  if (!quit & hypthosis_test.sigma %in% required) {
    if (is.na(sigma)) {
      sigma = get_input_num('Population standard deviation (sigma)', min_val=-Inf, max_val=Inf, int_only=FALSE, quit='q')
      quit = (tolower(sigma) == 'q')
    }
  } else {
    sigma = NA
  }
  if (!quit & hypthosis_test.alpha %in% required) {
    if (is.na(alpha)) {
      # TODO add more significance levels
      alpha = get_input_num('Significance level (alpha)', min_val=0.05, max_val=0.05, int_only=FALSE, quit='q')
      quit = (tolower(alpha) == 'q')
    }
  } else {
    alpha = NA
  }

  if (!quit) {
    result = c(type=NA, xbar=NA, s2=NA, s=NA, n=NA, mu0=NA, sigma2=NA, sigma=NA, alpha=NA)
    result['type'] = utf8ToInt(tolower(type)) # need to convert to numeric as vector is all same type
    result['xbar'] = xbar
    result['s2'] = s2
    result['s'] = s
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

