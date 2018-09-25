normalize.low.level = function(vector, baseline, 
                               normalize_method = 'hybrid', 
                               if_pupil_value = TRUE) {

  if (identical(normalize_method, 'hybrid')) {
    
    # $CE$4 is the baseline value (median)
    # =100*(($CE$4-$CI197)/$CE$4)
    
    if (if_pupil_value == TRUE) {
      
      denominator = baseline
      nominator = baseline - vector
      vector = nominator / denominator
      
      # to percentage
      vector = vector * -100
      
    } else {
      
      # take the absolute value
      denominator = baseline
      vector = vector / denominator
      vector = abs(vector)
    }
    
  } else if (identical(normalize_method, 'divisive')) {
    # (corrected pupilsize = pupilsize/baseline)
    
    # noww the same operation for both the pupil sizes and the error
    vector = vector / baseline
    
  } else if (identical(normalize_method, 'subtractive')) {
    
    # subtractive baseline correction
    # corrected pupilsize = pupilsize − baseline)
    # subtractive baseline correction ->  increased statistical power 
    # more than divisive correction
    
    # TODO!
    warning('DO NOT USE THIS YET AS THIS DOES NOT WORK WITH MAX/MINs of BIN FEATUREs! implement later')
    
    if (if_pupil_value == TRUE) {
      vector = vector - baseline
    } else {
      # no effect on the error the subtraction has 
    }
    
  } else {
    warning('Your normalization method "', normalize_method, '" is not defined! Typo?')
  }
}