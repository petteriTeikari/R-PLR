process.artifact.file <- function(path, data_path_out, IO_path, source_path,
                                  filename_path, param) {

  cat(" \nProcessing file:", filename_path, "\n")
  
  # Unwrap the parameters
  # easier to pass variables like this to "APIed" subfunctions rather than passing
  # the whole param to all of the subfunctions. Or clearer to follow what params are needed
  # and where
  blink_hard_threshold = as.numeric(unlist(param['blink_hard_threshold'])) # smaller values than this are blinks
  spline_iter_no = as.numeric(unlist(param['spline_iter_no'])) # default value = 2, higher does not seem to improve the result
  sigma_multip = as.numeric(unlist(param['sigma_multip'])) # times stdev for spline filter for outlier rejection
  debug_clean_on = as.logical(unlist(param['debug_clean_on'])) # if you want to see intermediate steps
  plot_output = as.logical(unlist(param['plot_output'])) # 
  
  # FILTERING PIPE ----------------------------------------------------------
    
    # Define the type of data
    # file_format = define.pupillometerFileformat(path, filename_path)
    
    # Import data
    # data_frame_in = import.pupildata(data_path, filename_path, file_format, IO_path)
    data_frame_in = read.csv(filename_path)
  
    # Clean the PLR (remove outliers)'
    data_frame_outlierfree = clean.plr(data_frame_in, source_path,  
                                       blink_hard_threshold, spline_iter_no, 
                                       sigma_multip, debug_clean_on)
    
    # Save to disk
    filecode = strsplit(tail(strsplit(filename_path, .Platform$file.sep)[[1]],1), '_')[[1]][1]
    export.pupil.dataframe.toDisk(data_frame_outlierfree, paste0(filecode, '.csv'), data_path_out, 'outlier_free')
    
    # Plot
    if(plot_output == TRUE) {
      # Takes the most time in this processing actually, even with a fast HDD
      plot.each.artifact.file(data_frame_in, data_frame_outlierfree, path_out, filename_path)
    }
    
}
