compute.prelim.stat.tests = function(x1, x2, y, error, subj, p_threshold = 0.05) {
  
  # errors and subject_codes at the moment not really used at the moment,
  # but just in case as input arguments
  
  # Create a data frame as some tests require this
  feat_data_in = data.frame(x = x1, x2 = x2, y = y, error = error, subj = subj,
                            stringsAsFactors = FALSE)
  
  # convert the independent variables to factors
  feat_data_in$x = factor(feat_data_in$x)
  feat_data_in$x2 = factor(feat_data_in$x2)
  
  # Init the output list
  out_list = list()
  
  # Shapiro-Wilk normality test
  out_list[['Shapiro']] = shapiro.test(y)
  out_list[['Shapiro']]$True = out_list[['Shapiro']]$p.value < p_threshold
  
  # Homogeneity of Variance
  # http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/
  
  # TODO! If you want to save some time, you could add a switch, and not compute
  # all measures based on Shapiro-Wilk result
  
  # Bartlett’s test
  # If the data is normally distributed, this is the best test to use.
  if (sum(is.na(x2)) == length(x1)) {
    out_list[['Bartlett']] = bartlett.test(y ~ x, data=feat_data_in)
    out_list[['Bartlett']]$True = out_list[['Bartlett']]$p.value < p_threshold
  } else {
    # two independent variables
  }
  
  # Levene's test
  # more robust to departures from normality than Bartlett’s test.
  if (sum(is.na(x2)) == length(x1)) {
    out_list[['Levene']] = leveneTest(y ~ x, data=feat_data_in)
    out_list[['Levene']]$True = out_list[['Levene']]$`Pr(>F)`[1] < p_threshold
  } else {
    # two independent variables
  }
  
  # Fligner-Killeen test
  # a non-parametric test which is very robust against departures from normality.
  if (sum(is.na(x2)) == length(x1)) {
    out_list[['Fligner']] = fligner.test(y ~ x, data=feat_data_in)
    out_list[['Fligner']]$True = out_list[['Fligner']]$p.value < p_threshold
  } else {
    # two independent variables
  }
  
  # Make a conclusion that it is easier to use in the following steps 
  out_list[['CONCLUSION']][['Normal']]$Name = 'Shapiro'
  out_list[['CONCLUSION']][['Normal']]$True = out_list[['Shapiro']]$True
  
  # If the data was indeed normal
  if (out_list[['CONCLUSION']][['Normal']]$True) {
    out_list[['CONCLUSION']][['Homoscedastic']]$Name = 'Bartlett'
    out_list[['CONCLUSION']][['Homoscedastic']]$True = out_list[['Bartlett']]$True
    
  } else { # If the data was not
    out_list[['CONCLUSION']][['Homoscedastic']]$Name = 'Fligner'
    out_list[['CONCLUSION']][['Homoscedastic']]$True = out_list[['Fligner']]$True 
  }
  
  # TODO! Add a switch if you want to control from outside this function, whether to 
  # choose Levene or Fligner when the data is non-normal, or between Bartlett and Levene
  # when the data is normal?
  
  return(out_list)
  
}

