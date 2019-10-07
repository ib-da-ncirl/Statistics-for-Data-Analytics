source("utils.R")

# Get the alpha, confidence level & t-value corresponding to the specified argument 
# E.g. to request the Two-tail values for an alpha (significance level) of 0.05 with 8 degrees of freedom
#     values = t.calc_alt('t', 8, alpha_req=0.05)
#     print(unname(values['alpha']))  # prints 0.05 
#     print(unname(values['level']))  # prints 95
#     print(unname(values['tval']))  # prints 3.72 
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param df: degrees of freedom
# :param alpha_req: alpha value to request
# :param level_req: confidence level to request
# :return named vector with values corresponding to request
t.calc_alt = function(type, df, alpha_req=NA, level_req=NA) {
  
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
  
  # t-table shows right-tail probabilities
  multiplier = 1
  ltail = FALSE
  if (hypothesis.isTwoTail(type)) {
    multiplier = 2 # double value to account for both right tail & left tail
  } else if (hypothesis.isLowerTail(type)) {
    ltail = TRUE
  }
  
  result = c(alpha_req, level_req, round((multiplier*qt(alpha_req, df, lower.tail=ltail)),2))
  names(result) <- c('alpha','level','tval')

  return(result)
}

# Get the critical values for the specified alpha (significance level)
# :param alpha: alpha value to request CV for
# :param df: alpha value to request CV for
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u', Two-tail by default
# :param rounding: Number of decimal places to round result to; 3 by default
# :return named vector with values corresponding to request
t.get_critical_values = function(alpha, df, type=hypothesis.twoTail, rounding=3) {
  
  arg_errors = cv_arguments_check(alpha, type, rounding)
  if (length(arg_errors) > 0) {
    msg = ''
    for (m in arg_errors) {
      msg = paste0(c(msg, m), collapse = "\n")
    }
    stop(msg)
  }
  if (df <= 0) {
    stop(paste0(c("Invalid 'df' argument, must be >= 1:", df), collapse = " "))
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
  
  t.alpha = qt(alpha/divisor, df, lower.tail=ltail)
  if (is.numeric(rounding)) {
    t.alpha = round(t.alpha, rounding)
  }
  if (hypothesis.isTwoTail(type)) {
    cv = c(-t.alpha, t.alpha)   # symmetrical so lower tail value is negative of upper tail
  } else if (hypothesis.isLowerTail(type)) {
    cv = c(t.alpha)
  } else {
    cv = c(t.alpha)
  }
  names(cv) = titles
  dbg_print("critical values for", unicode_chars('alpha'), '=', alpha, ", df =" , df, ", type =", type, ':', cv)
  return(cv)
}  

