dbg_enabled <<- TRUE # note: <<- needed for global variable


# Get user input
# :param prompt: prompt message to display
# :param allowed_values: vector of allowed values
# :return user input
get_input = function(prompt, allowed_values) {
  result = ''
  while (!(result %in% allowed_values)) {
    allow = paste(allowed_values, sep='', collapse = ",")
    allow_str = paste('[', allow, ']: ', sep='')
    dprompt = paste(prompt, allow_str)
    result <- readline(prompt=dprompt)
  }
  return (result)
}

# Get user input
# :param prompt: prompt message to display
# :param min_val: min allowed value
# :param max_val: max allowed value
# :param int_only: integer only flag
# :param quit: optional abort input value
# :return user input
get_input_num = function(prompt, min_val, max_val, int_only=FALSE, quit=NA) {
  valid = FALSE
  result = 0
  while (!valid) {
    allow = paste(min_val, max_val, sep='|')
    allow_str = paste('[', allow, ']: ', sep='')
    dprompt = paste(prompt, allow_str)
    result <- readline(prompt=dprompt)
    
    if (!is.na(quit) & (tolower(result) == tolower(quit))) {
      break
    }

    if (!is.na(as.numeric(result))) {
      dval = as.numeric(result)
      if (int_only) {
        ival = as.integer(result)
        if (ival != dval) {
          next
        }
      }
      if (int_only) {
        result = ival
      } else {
        result = dval
      }
      valid = (result >= min_val) & (result <= max_val)
    }
  }
  return (result)
}

# Query user if debug output is required
dbg_enable = function() {
  dbg_sel <- get_input('Enable debug', c('y','n'))
  dbg_enabled <<- (tolower(dbg_sel) == 'y') # note: <<- needed for global variable
}

# Print debug output
# :param ...: arguments to print
dbg_print = function(...) {
  if (exists("dbg_enabled")) {
    if (dbg_enabled) {
      arguments <- list(...)
      print(paste0(c(">> ", arguments), collapse = " "))
    }
  }
}

hypothesis.twoTail <- 't'
hypothesis.upperTail <- 'u'
hypothesis.lowerTail <- 'l'
hypothesis.valid_types = c(hypothesis.twoTail, hypothesis.upperTail, hypothesis.lowerTail)

# Check if type is two-tail
hypothesis.isTwoTail = function(type) {
  return(tolower(type) == hypothesis.twoTail)
}
# Check if type is lower-tail
hypothesis.isLowerTail = function(type) {
  return(tolower(type) == hypothesis.lowerTail)
}
# Check if type is upper-tail
hypothesis.isUpperTail = function(type) {
  return(tolower(type) == hypothesis.upperTail)
}

# Check if type is upper-tail
hypothesis.typeToStr = function(type) {
  string = switch(tolower(type),
                  't' = "Two-tail",
                  'l' = "Lower-tail",
                  'u' = "Upper-tail"
  )
}

# Validate test type
# :param type: test type argument
# :return standard type, or NA if invalid
hypothesis.validate_test_type = function(type) {
  if (!(tolower(type) %in% hypothesis.valid_types)) {
    result = NA
  } else {
    result = tolower(type)
  }
  return(result)
}

# Request test type input if argument is NA, or return standard type
# :param type: test type argument
# :return standard type, or NA if invalid
hypothesis.get_test_type = function(type=NA) {

  if (is.na(type)) {
    type = get_input('Test type - Two/Lower/Upper', hypothesis.valid_types)
  } else if (is.numeric(type)) {
    # convert from numeric
    type = hypothesis.validate_test_type(tolower(intToUtf8(type)))
  }
  return (type)
}


# Return common statistical characters
# :param name: Name of character to return
# :return Named vector for character or NA vector
unicode_chars = function(name) {
  chars = c(mu="\u03bc", mu0="\u03bc\u2080", mu1="\u03bc\u2081", 
            h0="H\u2080", h1="H\u2081", notequal="\u2260", alpha="\u237a", 
            sigma="\u03c3", sigma2="\u03c3\u00b2",
            sub0="\u2080", sub1="\u2081", sub2="\u2082")
  return(chars[name])
}

# Build common statistical phrases
# :param name: Name of phrase to return
# :param value: Optional, value to include in phrase
# :return String for phrase or NULL
gen_phrase = function(name, value=NA) {
  phrase = switch(name,
    "h0_mu==mu0" = c(unicode_chars("h0"), ": ", unicode_chars("mu"), " = ", unicode_chars("mu0")),
    "h1_mu!=mu0" = c(unicode_chars("h1"), ": ", unicode_chars("mu"), " ", unicode_chars("notequal"), " ", unicode_chars("mu0")),
    "h1_mu>mu0" = c(unicode_chars("h1"), ": ", unicode_chars("mu"), " > ", unicode_chars("mu0")),
    "h1_mu<mu0" = c(unicode_chars("h1"), ": ", unicode_chars("mu"), " < ", unicode_chars("mu0")),
    "alpha==" = c(unicode_chars("alpha"), " = ", value),
    "h0_mu0==mu1" = c(unicode_chars("h0"), ": ", unicode_chars("mu0"), " = ", unicode_chars("mu1")),
    "h1_mu0!=mu1" = c(unicode_chars("h1"), ": ", unicode_chars("mu0"), " ", unicode_chars("notequal"), " ", unicode_chars("mu1")),
    "h1_mu0>mu1" = c(unicode_chars("h1"), ": ", unicode_chars("mu0"), " > ", unicode_chars("mu1")),
    "h1_mu0<mu1" = c(unicode_chars("h1"), ": ", unicode_chars("mu0"), " < ", unicode_chars("mu1"))
  )
  
  if (is.vector(phrase)) {
    phrase = paste(unname(phrase), collapse = "")
  }
  return(phrase)
}

# Concatinate the elements with no spaces
# :param elements: a vextor of the elements to concatinate
# :return contatinated string
paste_ns = function(elements) {
  return(paste0(elements, collapse = ""))
}


# Check common arguments, and calculate missing arguments where possible
# :param data: vector of sample data
# :param xbar: sample mean
# :param n: sample size
# :param mu0: population mean
# :param sigma2: population variance
# :param sigma: sample standard deviation
# :param df: degrees of freedom
check_arguments = function(data=NA, xbar=NA, n=NA, mu0=NA, sigma2=NA, sigma=NA, df=NA) {
  isv = is.vector(data, mode = "numeric")
  # these are primarily to avoid an unused arguments error
  isx = is.numeric(xbar)
  isn = is.numeric(n)
  ism = is.numeric(mu0)
  iss2 = is.numeric(sigma2)
  iss = is.numeric(sigma)
  isdf = is.numeric(df)

  if (isv) {
    # if there is data, use it to calc everything, except mu0
    xbar = mean(data)
    n = length(data)
    sigma2 = var(data)
    sigma = sd(data)
    df = n - 1
  } else {
    if (iss & !iss2) {
      sigma2 = sigma**2
    } else if (!iss & iss2) {
      sigma = sigma2**0.5
    } 
    if (!isn & isdf) {
      n = df + 1
    } else if (isn & !isdf) {
      df = n - 1
    }
  }
  
  # need to unname arguments to avoid name concatination, e.g. c(xbar.xbar = 11)
  args = c(xbar=unname(xbar), n=unname(n), mu0=unname(mu0), sigma2=unname(sigma2), 
           sigma=unname(sigma), df=unname(df))
  dbg_print("args", args)
  return (args)
}


# Check the arguments for critical values calculations
# :param alpha: alpha value to request CV for
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param rounding: Number of decimal places to round result to
# :return string vector of ant errors
cv_arguments_check = function(alpha, type, rounding) {
  cv_errors = list()
  if ((alpha <= 0) | (alpha >= 1)) {
    msg = paste0(c("Invalid 'alpha' argument, must be '0 < alpha < 1':", alpha), collapse = " ")
    cv_errors[length(cv_errors)+1] = msg
  }
  if (is.numeric(rounding) & (rounding < 1)) {
    msg = paste0(c("Invalid 'rounding' argument, must be >= 1:", rounding), collapse = " ")
    cv_errors[length(cv_errors)+1] = msg
  }
  if (is.na(hypothesis.validate_test_type(type))) {
    msg = paste0(c("Invalid 'type' argument, must be:", 
                        paste0(hypothesis.valid_types, collapse = "|"), ":", type), collapse = " ")
    cv_errors[length(cv_errors)+1] = msg
  }
  
  return(cv_errors)
}  
