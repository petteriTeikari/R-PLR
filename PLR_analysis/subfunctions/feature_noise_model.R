combine.uncertainties = function(vector, value, stdev, 
                                 rms_error, uncert_in, 
                                 value_index, method) {
  # Note! Now if we already have uncertainties in the bin, this
  # should account to this stdev as well
  
  if (identical(method, 'max') | identical(method, 'min')) {
    
    # TODO! recompute actually now these both based on value_index
    # error now computed from whole bin
    uncert_out = sqrt(stdev^2 + rms_error^2)    
    
  } else if (identical(method, 'median') | identical(method, 'median')) {
    
    uncert_out = sqrt(stdev^2 + rms_error^2)  
    
  } else if (identical(method, 'linearfit') | identical(method, 'linearfit_max')) {
    
    uncert_out = sqrt(stdev^2 + rms_error^2)    
    
  } else if (identical(method, 'timing')) {
    
    # TODO! recompute actually now these both based on value_index
    # error now computed from whole bin
    uncert_out = sqrt(stdev^2 + rms_error^2)  
    
  } else if (identical(method, 'auc')) {
    
    uncert_out = sqrt(stdev^2 + rms_error^2)  
    
  } else if (identical(method, 'aucSSDnsdflknsfdnfds.nfs,mnfds,mnsfd,mnsfd,dsfn')) {
    
    # TODO (fill these)  
    
  } else {
    uncert_out = sqrt(stdev^2 + rms_error^2) 
    # warning("Typo with method = '", method, "' and how did this propagate here?\n") 
  }
  
  return(uncert_out)
  
  # as we set uncert = 0, this is the same as stdev now
}

compute.rms.error = function(uncert_in) {
  
  squared_uncertain = uncert_in^2
  
  no_of_nans = sum(is.na(squared_uncertain))
  no_of_non_nans = length(uncert_in)- no_of_nans
  
  sum_uncertain = sum(squared_uncertain, na.rm = TRUE)
  mean_of_squared = sum_uncertain / no_of_non_nans
  rms_error = sqrt(mean_of_squared)
  
  return(rms_error)
  
}