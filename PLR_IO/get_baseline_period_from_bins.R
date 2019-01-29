get.baseline.period.from.bins = function(bins) {
  
  bin_names = bins$Name
  index = match("Baseline", bin_names)
  
  if (is.na(index)) {
    warning('No variable (column) name of "Baseline" was not found from bins.csv, Using the default value!')
    baseline_period = c(10, 15)
  } else {
    baseline_period = c(bins$Start[index], bins$End[index])
  }
  
  return(baseline_period)
  
}