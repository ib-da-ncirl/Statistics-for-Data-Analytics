# Reference class to describe datset characteristics
datadef <- setRefClass("datadef",
                       fields = list(mean = "numeric",    # mean 
                                     var = "numeric",     # variance
                                     stddev = "numeric",  # standard deviation
                                     n = "numeric",       # sample size
                                     df = "numeric",      # degrees of freedom
                                     data = "vector"),    #sample data
                       methods = list(
                         initialize = function(mean = 0, 
                                               var = 0,
                                               stddev = 0,
                                               n = 0,
                                               df = 0,
                                               data = NA) {
                           mean <<- unname(mean)
                           var <<- unname(var)
                           stddev <<- unname(stddev)
                           n <<- unname(n)
                           df <<- unname(df)
                           data <<- unname(data)
                           populate()
                         },
                         populate = function() {
                           isv = is.vector(data, mode = "numeric")
                           ism = is.numeric(mean)
                           iss2 = is.numeric(var)
                           iss = is.numeric(stddev)
                           isn = is.numeric(n)
                           isdf = is.numeric(df)
                           
                           if (isv) {
                             # if there is data, use it to calc everything
                             mean <<- mean(data)
                             var <<- var(data)
                             stddev <<- sd(data)
                             n <<- length(data)
                             df <<- n - 1
                           } else {
                             if (iss & !iss2) {
                               var <<- unname(stddev)**2
                             } else if (!iss & iss2) {
                               stddev <<- unname(var)**0.5
                             } 
                             if (!isn & isdf) {
                               n <<- unname(df) + 1
                             } else if (isn & !isdf) {
                               df <<- unname(n) - 1
                             }
                           }
                         },
                         isvalid_mean_n = function() {
                           ism = is.numeric(mean)
                           isn = is.numeric(n)
                           valid = (ism & isn)
                           
                           if (valid) {
                             valid = (n > 0) & (as.integer(n)==n)
                           }
                           return(valid)
                         },
                         isvalid_mean_stddev = function() {
                           ism = is.numeric(mean)
                           iss = is.numeric(stddev)
                           valid = (ism & iss)
                           
                           if (valid) {
                             valid = (stddev > 0)
                           }
                           return(valid)
                         },
                         isvalid_mean_var = function() {
                           ism = is.numeric(mean)
                           iss2 = is.numeric(var)
                           valid = (ism & iss2)
                           
                           if (valid) {
                             valid = (var > 0)
                           }
                           return(valid)
                         },
                         isvalid = function() {
                           ism = is.numeric(mean)
                           iss2 = is.numeric(var)
                           iss = is.numeric(stddev)
                           isn = is.numeric(n)
                           isdf = is.numeric(df)
                           valid = (ism & iss2 & iss & isn & isdf)
                           
                           if (valid) {
                             valid = (var == stddev**2)
                             if (valid) {
                               valid = (n == df + 1)
                             }
                           }
                           return(valid)
                         },
                         toString = function() {
                           str = paste0("datadef(",
                                  "mean:", mean,
                                  ", var:", var,
                                  ", stddev:", stddev,
                                  ", n:", n,
                                  ", df:", df,
                                  ", data:", paste0(data,collapse = ","), 
                                  ")")
                           return(str)
                         }
                       )
)

