define.pupillometerFilesToProcess <- function(data_path, pattern_to_find, whatfor_the_files) {
  
  cat("Defining files to be processed\n")
  
  # TODO! You could make the "_BR" pattern non-hardcoded
  
  # Assumes that your study eye has been marked with _BR
  files <- list.files(path=data_path, pattern=pattern_to_find, recursive=TRUE)
  files_fullpath = list.files(path=data_path, pattern=pattern_to_find, recursive=TRUE, full.names = TRUE)
  
    # TODO! Add some error handling later maybe
    # http://mazamascience.com/WorkingWithData/?p=912
  
    # TODO! Does not handle the correctly if you have a subdirectory in "PLRXXX"
    # that also has the _BR.csv marking
  
  # List all the folders, not just the ones with _BR.csv marked files
  if (identical(whatfor_the_files, 'video')) {
    files_out = define.from.folder.names(data_path, files, files_fullpath)
  } else if (identical(whatfor_the_files, 'artifacts')) {
    files_out = files_fullpath
  }
  
  return(files_out)
  
}



# SUBFUNCTION to identify the data from the FOLDER NAMES  
define.from.folder.names = function (data_path, files, files_fullpath) {
  
  dirs = list.dirs(path = data_path, full.names = FALSE, recursive = TRUE)
  
  # https://stackoverflow.com/questions/46959981/include-pattern-in-list-dirs
  dirs <- dirs[ grepl("PLR*", dirs) ]
  # In Linux, you could invoke a system command
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/system.html
  
  # There must be an easier way to define the recursion depth (limit to 1),
  # but now some folders have a subfolder and we need to ignore that
  incl_dirs = list()
  for (i in 1:length(dirs)) {
    
    # separate possible subdir in PLRXXX from the path
    pass1a = strsplit(dirs[i], .Platform$file.sep)[[1]]
    pass1 = pass1a[1] # non-elegant way to remove the subdir
    
    # Now we might have _Exclude in folder name for exclusion
    if_found = grep("Exclude", pass1)
    if (length(if_found) != 0){
      cat("EXCLUDE the follwing patient:", pass1)
    } else {
      incl_dirs = c(incl_dirs, pass1) # add to output list, if not excluded
    }
  }
  
  # Check how many folders have incorrectly marked files
  cat(" Found", length(files), ' _BR files\n')
  cat("  .. from total of", length(incl_dirs), 'patient folders\n')
  cat("  .. .. thus you need to label", length(dirs)-length(files), 'files from folders\n')
  
  # Actually list the folders here that do not have _BR
  # ---
  
  # trim first PLR1058/PLR1058_BR.csv -> PLR1058
  files_justname = list()
  for (j in 1 : length(files)) {
    path_parts = strsplit(files[j], .Platform$file.sep)[[1]]
    files_justname = c(files_justname, path_parts[1])
  }
  
  # And then go through all directories, and see if a matching folder name
  # is found from the files with the _BR.csv ending
  cat("  Missing _BR for folder(s):", "\n")
  for (i in 1 : length(incl_dirs)) {
    found_from_dirs = grep(incl_dirs[i], files_justname)
    if (length(found_from_dirs) == 0){
      cat("   ", unlist(incl_dirs[i]), "\n")
    }
  }
  
  # TODO!
  # return in multiple formats (Variables)
  # https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
  # files_fullpath - full path, e.g. ~/Dropbox/Experiment/PLR2010/PLR2010_BR.csv
  # files - incudes the subdir, e.g. PLR2010/PLR2010_BR.csv
  # files_justname, e.g. PLR2010_BR.csv
  
  # Now just returning the full paths
  file_out = files_fullpath
  
  return(file_out)
}
