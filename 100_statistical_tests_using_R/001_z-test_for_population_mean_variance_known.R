source("z_utils.R")
source('hypothesis.R')

# Test 1 in '100 Statistical Tests'  (pg 5 & 21)
# Z-test for a population mean (variance known)

# Calculate z-value
# A vector argument of the test data, or the xbar & n arguments is required
# :param mu0: population mean
# :param sigma2: population variance
# :param data: vector of sample data
# :param xbar: sample mean
# :param n: sample size
# :return z-value
z.calculate = function(mu0, sigma2, data=NA, xbar=NA, n=NA) {
  
  fargs = check_arguments(data=data, xbar=xbar, n=n, mu0=mu0, sigma2=sigma2, sigma=NA)
  xbar = fargs['xbar']
  n = fargs['n']

  isv = is.vector(data, mode='numeric')
  isx_n = is.numeric(xbar) & is.numeric(n)
  if (!(isv & !isx_n) & !(!isv & isx_n) & !(isv & isx_n)) {
    stop(paste0(c("Invalid arguments: either 'data', or 'xbar'/'n' combination is required"), collapse = " "))
  }
  # z-value = (xbar - mu0) / sqrt(variance / n)
  zvalue = (xbar - mu0) / (sqrt(sigma2 / n))
  names(zvalue) = c("z-value")  # give zvalue its correct name, (inherits 'xbar' ??)
  dbg_print("z-value", unname(zvalue))
  return (zvalue)
}

# Calculate p-value
# A vector argument of the test data, or the xbar & n arguments is required
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param mu0: population mean
# :param sigma2: population variance
# :param data: sample data
# :param xbar: sample mean
# :param n: sample size
# :return p-value or Inf if error
z.pvalue = function(mu0, sigma2, type=NA, data=NA, xbar=NA, n=NA) {

  type = hypothesis.get_test_type(type)
  
  if (is.na(type)) {
    pvalue = Inf
  } else {
    
    fargs = check_arguments(data=data, xbar=xbar, n=n, mu0=mu0, sigma2=sigma2, sigma=NA)
    xbar = fargs['xbar']
    n = fargs['n']
    mu0 = fargs['mu0']
    sigma2 = fargs['sigma2']
    df = fargs['df']

    zvalue = z.calculate(mu0, sigma2, data=data, xbar=xbar, n=n)

    if (hypothesis.isTwoTail(type)) {
      # H0 : sample mean == population mean
      # Ha : sample mean != population mean
      
      # doing it this way (i.e. using 'abs(z)') means we don't have to worry about the sign of z
      # pnorm(abs(z)) : probability of z-value, i.e. negative infinity to z-value
      # 1-pnorm(abs(z)) : tail we're looking for, i.e z-value to infinity 
      # 2*(1-pnorm(abs(z))) : two tails, since it a mirror image
      pvalue = 2*(1-pnorm(abs(zvalue), lower.tail=TRUE))
      
    } else if (hypothesis.isLowerTail(type)) {
      # H0 : sample mean == population mean
      # Ha : sample mean < population mean
      pvalue = pnorm(zvalue, lower.tail=TRUE)
      
    } else {
      # H0 : sample mean == population mean
      # Ha : sample mean > population mean)
      pvalue = pnorm(zvalue, lower.tail=FALSE)
      
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
# :param sigma: population standard deviation
# :param alpha: significance level
z.hypthosis_test = function(type=NA, xbar=NA, n=NA, mu0=NA, sigma2=NA, sigma=NA, alpha=NA) {
  reject = NA

  input = hypthosis_test_input(hypthosis_test.z.mask, type, xbar, n, mu0, sigma2, sigma, alpha)
  if (is.vector(input)) {
    type = hypothesis.get_test_type(input['type'])
    if (!is.na(type)) {
      xbar = input['xbar']
      n = input['n']
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
      
      pvalue = z.pvalue(mu0, sigma2, type=type, data=NA, xbar=xbar, n=n)
      
      reject = p.rejectH0(alpha, pvalue)
      
      print(paste(name, "z-value result", pvalue, sep = " "))
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
# :param alpha: significance level
z.hypthosis_test_sample = function(data, type=NA, mu0=NA, sigma2=NA, alpha=NA) {
  z.hypthosis_test(type, xbar=mean(data), n=length(data), mu0=mu0, sigma2=sigma2, alpha=alpha)
}

# Main function to run tests
z_test_001 = function() {
  # class example
  class_data = c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)
  class_mu0 = 75
  class_sigma2 = 18
  
  # http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-mean-known-variance
  rtutor_2tail = c(xbar=14.6, n=35, mu0=15.4, sigma2=2.5**2, alpha=0.05)
  
  # http://www.r-tutor.com/elementary-statistics/hypothesis-testing/upper-tail-test-population-mean-known-variance
  rtutor_utail = c(xbar=2.1, n=35, mu0=2, sigma2=0.25**2, alpha=0.05)
  
  # http://www.r-tutor.com/elementary-statistics/hypothesis-testing/lower-tail-test-population-mean-known-variance
  rtutor_ltail = c(xbar=9900, n=30, mu0=10000, sigma2=120**2, alpha=0.05)

  menu_sels = c()
  # options 1-3
  menu_class = paste0(c("data vector", unicode_chars("mu0"), "=", class_mu0, unicode_chars("sigma2"), "=", class_sigma2), collapse = " ")
  for (type in hypothesis.valid_types) {
    menu_sels = append(menu_sels, paste0(c("Class example:", hypothesis.typeToStr(type), menu_class), collapse = " "))
  }
  # options 4-6
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
                     unicode_chars("sigma2"), "=", arguments['sigma2'],
                     unicode_chars("alpha"), "=", arguments['alpha']),
                   collapse = " "))
  }
  # options 7-8
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
      z.hypthosis_test_sample(class_data, type=type, mu0=class_mu0, sigma2=class_sigma2, alpha=0.05)
      
    } else if ((sel >= 4) & (sel <= 6)) {
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
      z.hypthosis_test(type=type, xbar=arguments['xbar'], n=arguments['n'],
                     mu0=arguments['mu0'], sigma2=arguments['sigma2'], sigma=NA, alpha=arguments['alpha'])
      
    } else if (sel == 7) {
      z.hypthosis_test()
    } else if (sel == 8) {
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
  z_test_001()
}


