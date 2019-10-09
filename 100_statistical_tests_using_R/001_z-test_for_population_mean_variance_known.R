source("z_utils.R")
source('hypothesis.R')
source('datadef.R')

# Test 1 in '100 Statistical Tests'  (pg 5 & 21)
# Z-test for a population mean (variance known)

# Calculate z-value
# A vector argument of the test data, or the xbar & n arguments is required
# :param mu0: population mean
# :param sigma2: population variance
# :param sampledef: sample data
# :return z-value
z.calculate = function(mu0, sigma2, sampledef) {
  
  if (!sampledef$isvalid_mean_n()) {
    stop(paste0(c("Invalid 'sampledef' argument: ", sampledef$toString()), collapse = " "))
  }

  # z-value = (xbar - mu0) / sqrt(variance / n)
  zvalue = (sampledef$mean - mu0) / (sqrt(sigma2 / sampledef$n))
  names(zvalue) = c("z-value")  # give zvalue its correct name
  dbg_print("z-value", unname(zvalue))
  return (zvalue)
}

# Calculate p-value
# A vector argument of the test data, or the xbar & n arguments is required
# :param mu0: population mean
# :param sigma2: population variance
# :param sampledef: sample data
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :return p-value or Inf if error
z.pvalue = function(mu0, sigma2, sampledef, type=NA) {
  
  type = hypothesis.get_test_type(type)
  
  if (is.na(type)) {
    pvalue = Inf
  } else {
    
    if (!sampledef$isvalid_mean_n()) {
      stop(paste0(c("Invalid 'sampledef' argument: ", sampledef$toString()), collapse = " "))
    }
    
    zvalue = z.calculate(mu0, sigma2, sampledef)
    
    pvalue = z.pvalue_from_z(zvalue, type)
  }
  dbg_print("p-value", unname(pvalue))
  
  return (pvalue)
}

# Do a hypothesis test
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param mu0: population mean
# :param sigma2: population variance
# :param sampledef: sample data
# :param alpha: significance level
z.hypthosis_test = function(type=NA, mu0=NA, sigma2=NA, sampledef=NULL, alpha=NA) {
  reject = NA

  if (is.null(sampledef)) {
    xbar=NA
    s2=NA
    s=NA
    n=NA
  } else {
    xbar=sampledef$mean
    s2=sampledef$var
    s=sampledef$stddev
    n=sampledef$n
  }
  input = hypthosis_test_input(hypthosis_test.z.mask, type, 
                               xbar, s2, s, n, 
                               mu0, sigma2, sigma, alpha)
  if (is.vector(input)) {
    type = hypothesis.get_test_type(input['type'])
    if (!is.na(type)) {
      xbar = input['xbar']
      n = input['n']
      mu0 = input['mu0']
      sigma2 = input['sigma2']
      sigma = input['sigma']
      alpha = input['alpha']
      
      sample = datadef(mean=xbar, n=n)
      pvalue = z.pvalue(mu0, sigma2, sample, type=type)
      
      reject = hypthosis_test.do(type, pvalue, alpha, h0=c('mu', 'mu0'))
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
  sampledef = datadef(data=data)
  z.hypthosis_test(type, mu0=mu0, sigma2=sigma2, sampledef, alpha=alpha)
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
      sampledef = datadef(mean=arguments['xbar'], n=arguments['n'])
      z.hypthosis_test(type=type, mu0=arguments['mu0'], sigma2=arguments['sigma2'], 
                       sampledef=sampledef, alpha=arguments['alpha'])
      
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


