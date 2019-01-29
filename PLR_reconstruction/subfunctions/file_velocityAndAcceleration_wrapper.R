file.fractal.wrapper = function(filename_path, data_path_out, param, debug = FALSE) {

  # INITIALIZE --------------------------------------------------------------

    # Check input
    just_filename = tail(strsplit(filename_path, .Platform$file.sep)[[1]], 1)
    filecode = strsplit(just_filename, '_')[[1]][1]
    cat(filecode, ' ')
    
    # READ IN
    df_in = read.csv(filename_path)
    # plot(df_in$time, df_in$pupil)
  
  # COMPUTATIONS ------------------------------------------------------------

    t = df_in$time
    y = df_in$pupil
    
    span = 0.1
    diffs_out = compute.PLR.derivatives(t, y, deriv_smooth = 'loess', 
                                        loess_span = span, debugON = TRUE)
      velocity = diffs_out[[1]]
      acceleration = diffs_out[[1]]
    

  
    
}
