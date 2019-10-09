source("z_utils.R")
source('hypothesis.R')
source('datadef.R')

# Test 3 in '100 Statistical Tests'  (pg 6 & 25)
# Z-test for two population means (variances known and unequal)

# Calculate z-value
# :param popdef1: population 1 data (mean & variance)
# :param popdef2: population 2 data (mean & variance)
# :param sampledef1: sample 1 data (mean & n)
# :param sampledef2: sample 2cdata (mean & n)
# :return z-value
z.calculate2sam = function(popdef1, popdef2, sampledef1, sampledef2) {
  
  idx = 1
  targs = c(popdef1=popdef1, popdef2=popdef2)
  for (ddef in targs) {
    if (!ddef$isvalid_mean_var()) {
      stop(paste0(c("Invalid '", names(targs)[idx], "' argument (mean/variance):", ddef$toString()), collapse = " "))
    }
    idx = idx + 1
  }
  idx = 1
  targs = c(sampledef1=sampledef1, sampledef2=sampledef2)
  for (ddef in targs) {
    if (!ddef$isvalid_mean_n()) {
      stop(paste0(c("Invalid '", names(targs)[idx], "' argument (mean/n): ", ddef$toString())))
    }
    idx = idx + 1
  }
  
  # z-value = ((xbar1 - xbar2) - (mu0_1 - mu0_2)) / sqrt((variance1 / n1) + (variance2 / n2))
  zvalue = (((sampledef1$mean - sampledef2$mean) - (popdef1$mean - popdef2$mean)) 
            / ((popdef1$var / sampledef1$n) + (popdef2$var / sampledef2$n))**0.5)
  names(zvalue) = c("z-value")  # give zvalue its correct name
  dbg_print("z-value", unname(zvalue))
  return (zvalue)
}

# Calculate p-value
# :param popdef1: population 1 data (mean & variance)
# :param popdef2: population 2 data (mean & variance)
# :param sampledef1: sample 1 data (mean & n)
# :param sampledef2: sample 2 data (mean & n)
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :return p-value or Inf if error
z.pvalue2sam = function(popdef1, popdef2, sampledef1, sampledef2, type=NA) {

  type = hypothesis.get_test_type(type)
  
  if (is.na(type)) {
    pvalue = Inf
  } else {
    
    zvalue = z.calculate2sam(popdef1, popdef2, sampledef1, sampledef2)

    pvalue = z.pvalue_from_z(zvalue, type)
  }
  dbg_print("p-value", unname(pvalue))

  return (pvalue)
}

# Do a hypothesis test
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param popdef1: population 1 data (mean & variance)
# :param popdef2: population 2 data (mean & variance)
# :param sampledef1: sample 1 data (mean & n)
# :param sampledef2: sample 2 data (mean & n)
# :param alpha: significance level
z.2sam_hypthosis_test = function(type=NA, popdef1=NULL, popdef2=NULL, 
                                 sampledef1=NULL, sampledef2=NULL, alpha=NA) {
  reject = NA

  # type & alpha input
  input = hypthosis_test_input(hypthosis_test.type_alpha.mask, type=type, alpha=alpha)
  if (is.vector(input)) {
    type = hypothesis.get_test_type(input['type'])
    if (!is.na(type)) {
      alpha = input['alpha']

      # population input      
      idx = 1
      quit = FALSE
      pd1 = NULL
      pd2 = NULL
      for (idx in 1:2) {
        dd = switch (idx,
          '1' = popdef1,
          '2' = popdef2
        )
       if (is.null(dd)) {
          mu0 = NA
          var = NA
        } else{
          mu0 = dd$mean
          var = dd$var
        }
        
        input = hypthosis_test_input(hypthosis_test.pop_mean_var.mask, mu0=mu0, sigma2=var,
                                     heading=paste0('Population ', idx))
        if (is.vector(input)) {
          pdi = datadef(mean=input['mu0'], var=input['sigma2'])
          if (pdi$isvalid_mean_var()) {
            if (idx == 1) {
              pd1 = pdi
            } else {
              pd2 = pdi
            }
          } else {
            quit = TRUE
          }
        } else {
          quit = TRUE
        }
        if (quit) {
          break
        }
      }
      
      # sample input
      sd1 = NULL
      sd2 = NULL
      if (!quit) {
        for (idx in 1:2) {
          dd = switch (idx,
           '1' = sampledef1,
           '2' = sampledef2
          )
          if (is.null(dd)) {
            xbar = NA
            n = NA
          } else{
            xbar = dd$mean
            n = dd$n
          }
          
          input = hypthosis_test_input(hypthosis_test.sam_mean_n.mask, xbar=xbar, n=n,
                                       heading=paste0('Sample ', idx))
          if (is.vector(input)) {
            pdi = datadef(mean=input['xbar'], n=input['n'])
            if (pdi$isvalid_mean_n()) {
              if (idx == 1) {
                sd1 = pdi
              } else {
                sd2 = pdi
              }
            } else {
              quit = TRUE
            }
          } else {
            quit = TRUE
          }
          if (quit) {
            break
          }
        }
      }
      
      if (!quit) {
        pvalue = z.pvalue2sam(popdef1=pd1, popdef2=pd2, sampledef1=sd1, sampledef2=sd2, type=type)
        
        reject = hypthosis_test.do(type, pvalue, alpha, h0=c('mu0', 'mu1'))
      }
    }
  }

  return(reject)
}

# Do a hypothesis test
# :param sample1: vector of sample 1 data to do test on
# :param sample2: vector of sample 2 data to do test on
# :param type: hypothesis test type; Two='t', Lower='l', Upper='u'
# :param popdef1: population 1 data (mean & variance)
# :param popdef2: population 2 data (mean & variance)
# :param alpha: significance level
z.2sam_hypthosis_test_sample = function(sample1, sample2, type=NA, popdef1=NULL, popdef2=NULL, alpha=NA) {
  sampledef1 = datadef(data=sample1)
  sampledef2 = datadef(data=sample2)
  z.2sam_hypthosis_test(type, popdef1=popdef1, popdef2=popdef2, 
                   sampledef1=sampledef1, sampledef2=sampledef2, alpha=alpha)
}

# Main function to run tests
z_test_003 = function() {
  # class example
  class_sample1 = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
  class_sample2 = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)
  class_pop1 = datadef(var=5)
  class_pop2 = datadef(var=8.5)

  # 'Statistics for dummies" pg 246 Stats-absorbent v Sponge-0-matic example
  paper_towels = c(xbar1=3, sigma1=0.9, n1=50, xbar2=3.5, sigma2=1.2, n2=50)
  
  menu_sels = c()
  # options 1-3
  menu_class = paste0(c("data vector", unicode_chars("sigma2"), "=", class_pop1$var, "and",
                        "data vector", unicode_chars("sigma2"), "=", class_pop2$var), collapse = " ")
  for (type in hypothesis.valid_types) {
    menu_sels = append(menu_sels, paste0(c("Class example:", hypothesis.typeToStr(type), menu_class), collapse = " "))
  }
  # options 4-6
  for (type in hypothesis.valid_types) {
    menu_sels = append(menu_sels, paste0(c("Statistics for dummies[paper towels]:", hypothesis.typeToStr(type), 
                                           "xbar1", "=", paper_towels['xbar1'],
                                           paste_ns(c(unicode_chars("sigma"), unicode_chars("sub1"))), "=", paper_towels['sigma1'],
                                           paste_ns(c("n", unicode_chars("sub1"))), "=", paper_towels['n1'],
                                           "xbar2", "=", paper_towels['xbar2'],
                                           paste_ns(c(unicode_chars("sigma"), unicode_chars("sub2"))), "=", paper_towels['sigma2'],
                                           paste_ns(c("n", unicode_chars("sub2"))), "=", paper_towels['n2']),
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
      z.2sam_hypthosis_test_sample(sample1=class_sample1, sample2=class_sample2, type=type, 
                                   popdef1=class_pop1, popdef2=class_pop2, alpha=0.05)
      
    } else if ((sel >= 4) & (sel <= 6)) {
      for (sel_type in hypothesis.valid_types) {
        if (grepl(hypothesis.typeToStr(sel_type), menu_sels[sel])) {
          type = sel_type
          break
        }
      }
      z.2sam_hypthosis_test(type=type, popdef1=datadef(var=paper_towels['sigma1']**2), 
                                        popdef2=datadef(var=paper_towels['sigma2']**2), 
                                        sampledef1=datadef(mean=paper_towels['xbar1'], n=paper_towels['n1']), 
                                        sampledef2=datadef(mean=paper_towels['xbar2'], n=paper_towels['n2']), 
                                        alpha=0.05)

    } else if (sel == 7) {
      z.2sam_hypthosis_test()
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
  z_test_003()
}


