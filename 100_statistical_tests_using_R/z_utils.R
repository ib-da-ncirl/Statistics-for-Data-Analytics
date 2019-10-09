source("utils.R")

# Calculate the alpha, confidence level & z-value corresponding to the specified argument 
# E.g. to request the Two-tail values for an alpha (significance level) of 0.05
#     values = z.calc_alz(type='t', alpha_req=0.05)
#     print(unname(values['alpha']))  # prints 0.05 
#     print(unname(values['level']))  # prints 95
#     print(unname(values['zval']))  # prints 1.96 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param alpha_req: alpha value to request
# :param level_req: confidence level to request
# :return named vector with values corresponding to request
z.calc_alz = function(type, alpha_req=NA, level_req=NA) {
  
  if (is.na(hypothesis.validate_test_type(type))) {
    stop(paste0(c("Invalid 'type' argument, must be ':", hypothesis.valid_types, "':", type), collapse = " "))
  }

  # calc the values
  if (!is.na(alpha_req)) {
    # lookup using alpha
    if ((alpha_req <= 0) | (alpha_req >= 1)) {
      stop(paste0(c("Invalid 'alpha_req' argument, must be '0 < alpha_req < 1':", alpha_req), collapse = " "))
    }

    level_req = (1-alpha_req)*100
  } else if (!is.na(level_req)) {
    if ((level_req <= 0) | (level_req >= 100)) {
      stop(paste0(c("Invalid 'level_req' argument, must be '0 < level_req < 100':", level_req), collapse = " "))
    }
    
    alpha_req = (100-level_req)/100
  } else {
    stop(paste0(c("Invalid arguments, either 'alpha_req' or 'level_req' is required:", level_req), collapse = " "))
  }

  # z-table shows the left-tail probability
  divisor = 1
  if (hypothesis.isTwoTail(type)) {
    divisor = 2 # split rejection region, half upper tail & half lower tail
  } else if (hypothesis.isLowerTail(type)) {
    divisor = 2 # lower tail => negative z-value
  }
  
  # can calc z-values using quantile function qnorm(alpha)
  result = c(alpha_req, level_req, round(qnorm(1-alpha_req/divisor),2))
  names(result) <- c('alpha','level','zval')

  return(result)
}

# Get the critical values for the specified alpha (significance level)
# :param alpha: alpha value to request CV for
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u', Two-tail by default
# :param rounding: Number of decimal places to round result to; 3 by default
# :return named vector with values corresponding to request
z.get_critical_values = function(alpha, type=hypothesis.twoTail, rounding=3) {
  
  arg_errors = cv_arguments_check(alpha, type, rounding)
  if (length(arg_errors) > 0) {
    msg = ''
    for (m in arg_errors) {
      msg = paste0(c(msg, m), collapse = "\n")
    }
    stop(msg)
  }

  divisor = 1
  ltail = FALSE
  if (hypothesis.isTwoTail(type)) {
    divisor = 2 # split rejection region, half upper tail & half lower tail
    titles = c('lower', 'upper')
  } else if (hypothesis.isLowerTail(type)) {
    titles = c('lower')
    ltail = TRUE
  } else {
    titles = c('upper')
  }
  
  # z-table shows left-tail values so can also use, 
  # z.alpha = qnorm(1-(alpha/divisor)), and leave the lower.tail argument at the default TRUE
  z.alpha = qnorm((alpha/divisor), lower.tail=ltail)
  if (is.numeric(rounding)) {
    z.alpha = round(z.alpha, rounding)
  }
  if (hypothesis.isTwoTail(type)) {
    cv = c(-z.alpha, z.alpha)   # symmetrical so lower tail value is negative of upper tail
  } else if (hypothesis.isLowerTail(type)) {
    cv = c(z.alpha)
  } else {
    cv = c(z.alpha)
  }
  names(cv) = titles
  dbg_print("critical values for", unicode_chars('alpha'), '=', alpha, ", type =", type, ':', cv)
  return(cv)
}  

# Calculate p-value
# :param zvalue: z-value
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :return p-value or Inf if error
z.pvalue_from_z = function(zvalue, type=NA) {
  
  type = hypothesis.get_test_type(type)
  
  if (is.na(type)) {
    pvalue = Inf
  } else {
    
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

