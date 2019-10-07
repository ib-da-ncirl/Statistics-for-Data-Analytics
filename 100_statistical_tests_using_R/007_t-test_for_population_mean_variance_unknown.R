source("t_utils.R")
source('hypothesis.R')

# Test 7 in '100 Statistical Tests'  (pg 7 & 29)
# t-test for a population mean (variance unknown)

# Calculate t-value
# :param data: vector of sample data
# :param mu0: population mean
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
t.t.test = function(data, mu0, type=NA) {

  type = hypothesis.get_test_type(type)
  if (hypothesis.isTwoTail(type)) {
    alternative = "two.sided"
  } else if (hypothesis.isLowerTail(type)) {
    alternative = "less"
  } else if (hypothesis.isUpperTail(type)) {
    alternative = "greater"
  } else {
    alternative = NA
  }
  
  if (!is.na(alternative)) {
    print(t.test(data, mu=mu0, alternative=alternative))
  }
}

# Calculate t-value
# :param mu0: population mean
# :param data: vector of sample data
# :param xbar: sample mean
# :param n: sample size
# :param sigma: sample standard deviation
# :return t-value
t.calculate = function(mu0, data=NA, xbar=NA, n=NA, sigma=NA) {
  
  fargs = check_arguments(data=data, xbar=xbar, n=n, mu0=mu0, sigma2=NA, sigma=sigma, df=NA)
  xbar = fargs['xbar']
  n = fargs['n']
  sigma = fargs['sigma']
  
  isv = is.vector(data, mode='numeric')
  isx_n_s = is.numeric(xbar) & is.numeric(n) & is.numeric(sigma)
  if (!(isv & !isx_n_s) & !(!isv & isx_n_s) & !(isv & isx_n_s)) {
    stop(paste0(c("Invalid arguments: either 'data', or 'xbar'/'n'/'sigma' combination is required"), collapse = " "))
  }
  
  # t-value = (xbar - mu0) / (sigma / sqrt(n))
  tvalue = (xbar - mu0) / (sigma / sqrt(n))
  names(tvalue) = c("t-value")  # give zvalue its correct name
  dbg_print("t-value", unname(tvalue))
  return (tvalue)
}

# Calculate p-value
# This function provides the same functionality as t.test (https://www.rdocumentation.org/packages/stats/versions/3.6.1/topics/t.test)
#
# A vector argument of the test data, or the xbar & n arguments is required
# :param mu0: population mean
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param data: vector of sample data
# :param xbar: sample mean
# :param n: sample size
# :param sigma: sample standard deviation
# :param df: degrees of freedom
# :param alpha: significance level
# :return p-value or Inf if error
t.pvalue = function(mu0, type=NA, data=NA, xbar=NA, n=NA, sigma=NA, df=NA, alpha=NA) {
  
  type = hypothesis.get_test_type(type)
  
  if (is.na(type)) {
    pvalue = Inf
  } else {
    
    fargs = check_arguments(data=data, xbar=xbar, n=n, mu0=mu0, sigma2=NA, sigma=sigma, df=df)
    xbar = fargs['xbar']
    n = fargs['n']
    mu0 = fargs['mu0']
    sigma = fargs['sigma']
    df = fargs['df']

    tvalue = t.calculate(mu0, data=data, xbar=xbar, n=n, sigma=sigma)

    if (hypothesis.isTwoTail(type)) {
      # H0 : sample mean == population mean
      # Ha : sample mean != population mean
      pvalue = 2*(pt(tvalue, df, lower.tail=TRUE))
      
    } else if (hypothesis.isLowerTail(type)) {
      # H0 : sample mean == population mean
      # Ha : sample mean < population mean
      pvalue = pt(tvalue, df, lower.tail=TRUE)
      
    } else {
      # H0 : sample mean == population mean
      # Ha : sample mean > population mean)
      pvalue = pt(tvalue, df, lower.tail=FALSE)
      
    }
    names(pvalue) = c("p-value")  # give pvalue its correct name
  }
  dbg_print("p-value", unname(pvalue))

  return (pvalue)
}

# Do a hypothesis test
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param xbar: sample mean
# :param n: sample size
# :param mu0: population mean
# :param sigma2: population variance
# :param sigma: sample standard deviation
# :param alpha: significance level
t.hypthosis_test = function(type=NA, xbar=NA, n=NA, mu0=NA, sigma2=NA, sigma=NA, alpha=NA) {
  reject = NA

  input = hypthosis_test_input(hypthosis_test.t.mask, type, xbar, n, mu0, sigma2, sigma, alpha)
  if (is.vector(input)) {
    type = hypothesis.get_test_type(input['type'])
    if (!is.na(type)) {
      xbar = input['xbar']
      n = input['n']
      df = input['df']
      mu0 = input['mu0']
      sigma2 = input['sigma2']
      sigma = input['sigma']
      alpha = input['alpha']
  
      if (hypothesis.isTwoTail(type)) {
        # Ha : sample mean != population mean
        h1_phrase = "h1_mu!=mu0"
        name = "Two tail"
  
      } else if (hypothesis.isLowerTail(type)) {
        # Ha : sample mean < population mean
        h1_phrase = "h1_mu<mu0"
        name = "Lower tail"
        
      } else if (hypothesis.isUpperTail(type)) {
        # Ha : sample mean > population mean
        h1_phrase = "h1_mu>mu0"
        name = "Upper tail"
        
      }
      # H0 : sample mean == population mean
      print(gen_phrase("ho_mu==mu0"))
      print(gen_phrase(h1_phrase))
      
      pvalue = t.pvalue(mu0, sigma=sigma, type=type, data=NA, xbar=xbar, n=n)
      
      reject = p.rejectH0(alpha, pvalue)
      
      print(paste(name, "t-value result", pvalue, sep = " "))
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
  }
  return(reject)
}

# Do a hypothesis test
# :param data: vector of data to do test on
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param mu0: population mean
# :param sigma2: population variance
# :param sigma: sample standard deviation
# :param alpha: significance level
t.hypthosis_test_sample = function(data, type=NA, mu0=NA, sigma2=NA, sigma=NA, alpha=NA) {
  t.hypthosis_test(type, xbar=mean(data), n=length(data), mu0=mu0, sigma2=sigma2, sigma=sigma, alpha=alpha)
}

# Main function to run tests
t_test_007 = function() {
  # class example
  class_data = c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)
  class_mu0 = 75
  class_sigma2 = 18
  
  # http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-mean-unknown-variance
  rtutor_2tail = c(xbar=14.6, n=35, mu0=15.4, sigma=2.5, alpha=0.05)

  # http://www.r-tutor.com/elementary-statistics/hypothesis-testing/upper-tail-test-population-mean-unknown-variance
  rtutor_utail = c(xbar=2.1, n=35, mu0=2, sigma=0.3, alpha=0.05)

  # http://www.r-tutor.com/elementary-statistics/hypothesis-testing/lower-tail-test-population-mean-unknown-variance
  rtutor_ltail = c(xbar=9900, n=30, mu0=10000, sigma=125, alpha=0.05)
  
  menu_sels = c()
  # options 1-3
  menu_builtin = paste0(c("data vector", unicode_chars("mu0"), "=", class_mu0), collapse = " ")
  for (type in hypothesis.valid_types) {
    menu_sels = append(menu_sels, paste0(c("Builtin:", hypothesis.typeToStr(type), menu_builtin), collapse = " "))
  }
  # options 4-6
  menu_class = paste0(c("data vector", unicode_chars("mu0"), "=", class_mu0), collapse = " ")
  for (type in hypothesis.valid_types) {
    menu_sels = append(menu_sels, paste0(c("Class example:", hypothesis.typeToStr(type), menu_class), collapse = " "))
  }
  # options 7-9
  for (type in hypothesis.valid_types) {
    if (hypothesis.isTwoTail(type)) {
      arguments = rtutor_2tail
    } else if (hypothesis.isUpperTail(type)) {
      arguments = rtutor_utail
    } else {
      arguments = rtutor_ltail
    }
    menu_sels = append(menu_sels, paste0(c("R-tutor:", hypothesis.typeToStr(type),
                                           "xbar", "=", arguments['xbar'],
                                           "n", "=", arguments['n'],
                                           unicode_chars("mu0"), "=", arguments['mu0'],
                                           unicode_chars("sigma"), "=", arguments['sigma'],
                                           unicode_chars("alpha"), "=", arguments['alpha']),
                                         collapse = " "))
  }
  # options 10-11
  menu_sels = append(menu_sels, c("Manually enter", "Toggle debug"))
  
  # query enable debug
  #dbg_enable()
  dbg_enabled <- TRUE
  
  sel = -1
  while (sel != 0) {
    sel = menu(menu_sels, graphics = FALSE, title = '\nSelect Test (0 to exit)')
    
    if ((sel >= 1) & (sel <= 3)) {
      for (sel_type in hypothesis.valid_types) {
        if (grepl(hypothesis.typeToStr(sel_type), menu_sels[sel])) {
          type = sel_type
          break
        }
      }
      t.t.test(class_data, mu0=class_mu0, type=type)
    } else if ((sel >= 4) & (sel <= 6)) {
      for (sel_type in hypothesis.valid_types) {
        if (grepl(hypothesis.typeToStr(sel_type), menu_sels[sel])) {
          type = sel_type
          break
        }
      }
      t.hypthosis_test_sample(class_data, type=type, mu0=class_mu0, sigma=sd(class_data), alpha=0.05)
    } else if ((sel >= 7) & (sel <= 9)) {
      for (sel_type in hypothesis.valid_types) {
        if (grepl(hypothesis.typeToStr(sel_type), menu_sels[sel])) {
          type = sel_type
          break
        }
      }
      if (hypothesis.isTwoTail(type)) {
        arguments = rtutor_2tail
      } else if (hypothesis.isUpperTail(type)) {
        arguments = rtutor_utail
      } else {
        arguments = rtutor_ltail
      }
      t.hypthosis_test(type=type, xbar=arguments['xbar'], n=arguments['n'],
                       mu0=arguments['mu0'], sigma2=NA, sigma=arguments['sigma'], alpha=arguments['alpha'])
    } else if (sel == 10) {
      t.hypthosis_test()
    } else if (sel == 11) {
      dbg_enabled <<- !dbg_enabled
    }
  }  
}

# if test in progress conditionally display menu
if (!exists("test_in_progress"))  {
  show_menu = TRUE
} else {
  show_menu = !test_in_progress
}
if (show_menu) {
  t_test_007() 
}


