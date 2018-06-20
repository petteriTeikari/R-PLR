copy.inFrameToOutDataframe = function(df_raw, df_filt) {
  
  # TODO! Loop through the elements?
  # Not really a problem when we have only one value differing though?
  
  # str(df_filt)
  df_out = df_raw
  df_out$pupil = df_filt$pupil
  # str(df_out)
  
  return(df_out)
  
}