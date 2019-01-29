import.SERI.pupillometer <- function(filename, header_rows) {
  
  # TODO! Add switch for detecting different versions
  
  # Read only the first triplet of RGB
  column_names = c("frame", "time", "pupil", "x", "y", "?", "R_center", "G_center", "B_center", "R", "G", "B")
  
  # This is when you read the reconstructed files
  if (header_rows == 1) {
    header_yes = TRUE
    no_of_cols_to_keep = 14 # TODO! Make it nicer
    no_of_cols = 14
    
    temp_data <- read.table(filename, fill=TRUE, header=header_yes, sep=",", 
                            colClasses=c("integer", "numeric", "numeric", 
                                         "integer", "integer", "integer",
                                         "integer", "integer", "integer", 
                                         "integer", "integer", "integer",
                                         "numeric", "numeric",
                                         rep("character", no_of_cols - no_of_cols_to_keep)))
    
    # TODO! Add warning for the 76 vs 77 columns !
    
    # This is the case with the raw data organized in folders
  } else {
    
    header_yes = FALSE
    no_of_cols_to_keep = 12
    # We specify the max column number now to 77
    no_of_cols = 77
    
    # Also we have to deal with varying number of columns 
    # (some rows have 76 colymns, and some do 77)
    # https://stackoverflow.com/questions/18922493/how-can-you-read-a-csv-file-in-r-with-different-number-of-columns
    
    temp_data <- read.table(filename, fill=TRUE, header=header_yes, sep=",", 
                            colClasses=c("integer", "numeric", "numeric", 
                                         "integer", "integer", "integer",
                                         "integer", "integer", "integer", 
                                         "integer", "integer", "integer",
                                         rep("character", no_of_cols - no_of_cols_to_keep)))
  }
  
  
  # dat = read.table(filename, header=FALSE, sep=",", col.names = paste0("COL",seq_len(77)), fill = TRUE)
  
  
  # Take only the 12 first columns (length of the column headers)
  dat = temp_data[1:no_of_cols_to_keep]
  
  # Assign the actual labels to the data frame
  if (header_rows == 1) {
    # Nothing needs to be done as columns correctly read
  } else {
    names(dat) = column_names
  }
  
  return(dat)
  
}