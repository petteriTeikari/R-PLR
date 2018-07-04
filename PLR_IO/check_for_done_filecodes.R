check.for.done.filecodes = function(files_to_process, path_check_for_done) {
  
  fullfiles_done = list.files(path=path_check_for_done, pattern='*.csv', 
                              recursive=FALSE, full.names = TRUE)
  
  split_init = strsplit(fullfiles_done, .Platform$file.sep)
  files_done = sapply(split_init, tail, 1)
  split_file = strsplit(files_done, '_')
  filecodes_done = unlist(lapply(split_file, '[[', 1))

  # INPUT
  split_init = strsplit(files_to_process, .Platform$file.sep)
  files_done = sapply(split_init, tail, 1)
  split_file = strsplit(files_done, '_')
  filecodes_input = unlist(lapply(split_file, '[[', 1))
    
  indices_done = filecodes_input %in% filecodes_done
  indices_undone = !indices_done
  
  number_of_undone = sum(indices_undone)
  cat('  .. found', number_of_undone, 'unprocessed input files')
  
  return(indices_undone)
  
}

