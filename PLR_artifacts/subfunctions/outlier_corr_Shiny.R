outlier.corr <- function(raw_data, outliers_in, source_path, param) {
  
  correct_outliers = as.logical(unlist(param['correct_outliers'])) # if you want to manually correct for the outliers
  do_not_repeat_correction = as.logical(unlist(param['do_not_repeat_correction'])) # check if you have already corrected once
  correcting_person = as.name(unlist(param['correcting_person'])) # allow multiple people to correct, and check whether they correspond
  
  if (correct_outliers == FALSE) {
    # return the input as output, and do nothing
    outliers_out = outliers_in
  } else {
    # Now we need to call the Shiny app for interactive outlier
    # inclusion / exclusion
    outliers_out = outliers_in
  }
  
}