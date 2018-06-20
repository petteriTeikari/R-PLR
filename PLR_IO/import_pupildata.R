import.pupildata <- function(path, filename, file_format, IO_path) {
  
  # cat(path, '\n')
  # cat(filename, '\n')
  # cat(file_format)
  
  source(file.path(IO_path, 'import_SERI_pupillometer.R', fsep = .Platform$file.sep))
  
  # NOTE!
  # now the data frame should have two variables:
  #  - time
  #  - pupil
  # otherwise if you define other names, the remaining script does not work
  
  if(identical(file_format, "2column_test")) {
    cat("2column test for debugging \n")
    dat = read.csv(file.path(path, filename, fsep = .Platform$file.sep))
    
    # ghetto fix as the other file the x-axis is in frames, and in the other in seconds
    if(identical(filename, "PLR156_long_b_1_POAGsevere_debugTrace.csv")) {
      data_frame = data.frame(time=dat$x/fps, pupil=dat$y)  
    } else {
      data_frame = data.frame(time=dat$t, pupil=dat$y)
    }
    
  # The SERI syntax now
  } else if(identical(file_format, "_BR")) {
    cat("Importing the SERI format\n")
    header_rows = 0
    data_frame = import.SERI.pupillometer(filename, header_rows)
  
  # The filtered files, which have header rows  
  } else if(identical(file_format, "_BR_filt")) {
    cat("Importing the SERI format (with headers)\n")
    header_rows = 1
    data_frame = import.SERI.pupillometer(filename, header_rows)
    
  
    
  } else {
  cat("TODO, file_format, not detected properly '\n")
  # TODO! Why detect_fileformat.R does not read in the method correctly?
  dat = read.csv(file.path(path, filename, fsep = .Platform$file.sep))
  
  # ghetto fix as the other file the x-axis is in frames, and in the other in seconds
  if(identical(filename, "PLR156_long_b_1_POAGsevere_debugTrace.csv")) {
    data_frame = data.frame(time=dat$x/fps, pupil=dat$y)  
  } else {
    data_frame = data.frame(time=dat$t, pupil=dat$y)
    }
    
  }
}