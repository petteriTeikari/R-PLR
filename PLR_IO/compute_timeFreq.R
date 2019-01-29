compute.timeFreq = function(t, y) {
  
  library(Rwave)
  # noctave - number of powers of 2 for the scale variable
  # nvoice  - number of scales in each octave (i.e. between two consecutive powers of 2).
  
  # Now you probably know that you can compute the time-frequency distributions in some many
  # ways that add some other methods inside of this function if the very basic CWT does not
  # give you satisfactory results
  cwt_out = cwt(y, noctave=8, nvoice=12, twoD=FALSE, plot=FALSE)
  
  retPolar = cwtpolar(cwt_out) # Conversion to Polar Coordinates
  retModulus = cwtimage(retPolar$modulus) # Converts the output (modulus) of cwtpolar to a 2D array 
  retArgument = cwtimage(retPolar$argument) # Converts the output (argument) of cwtpolar to a 2D array 
  
  return(list(cwt_out, retModulus, retArgument))
  
}