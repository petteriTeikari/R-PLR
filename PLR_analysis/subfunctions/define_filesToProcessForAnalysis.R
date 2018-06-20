define.filesToProcessForAnalysis = function(data_path, pattern_to_find, 
                                            process_only_one_file, name_to_find) {
  
  files = list.files(path=data_path, pattern=pattern_to_find, recursive=TRUE)
  files_fullpath = list.files(path=data_path, pattern=pattern_to_find, recursive=FALSE, full.names = TRUE)
  
  # print the lists found
    # str(files)
    # e.g. "PLR1058_BR_clean.csv"
    # str(files_fullpath)
    # e.g. "/home/petteri/Dropbox/LABs/SERI/Pupillometry Data/DATA_OUT/outlier_free/PLR1058_BR_clean.csv"
  
  cat(length(files_fullpath), " files found from ", data_path)
  
  if (process_only_one_file == TRUE) {
    
    # You could access single file ither with 
    # direct indexing, e.g. the 4th file
      # full_name = files_fullpath[1]
    
    # easier to define the filename that you want to process though
    index_for_name = grep(name_to_find, files_fullpath)
    
    # for advanced upgrade, check try-catch 
    # http://mazamascience.com/WorkingWithData/?p=912
    if (length(index_for_name) == 0) {
      # You could specify some other way to handle this as well
      warning("Specified filename = '", name_to_find, "' was not found, opening the 1st file now") 
      full_name = files_fullpath[1]
    } else {
      full_name = files_fullpath[index_for_name]
    }
    files_to_process = list(full_name)
    
    # BATCH PROCESS
  } else {
      files_to_process = files_fullpath
  }
  
  # manually you could define just one file if you wanted
  # now we just "files_to_process" with full paths
  # you could make it a key-value list at some point (data frame):
  
  return(files_to_process)
  
}