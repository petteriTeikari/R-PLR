check.for.done.filecodes = function(files_to_process, 
                                    path_check_for_done) {
  
  fullfiles_done = list.files(path=path_check_for_done, pattern='*.csv', 
                              recursive=FALSE, full.names = TRUE)
  
  if (length(fullfiles_done) != 0) {
    split_init = strsplit(fullfiles_done, .Platform$file.sep)
    files_done = sapply(split_init, tail, 1)
    split_file = strsplit(files_done, '_')
    filecodes_done = unlist(lapply(split_file, '[[', 1))
    
    # for example for feature computation, we might have many
    # files with the same filecodes
    filecodes_done = unique(filecodes_done)
    
  } else {
    warning('There are no done files from your "check path" = ', path_check_for_done, '\n',
            '  -> in other we assume now that you have not yet processed any of the input files')
    filecodes_done = c('')
  }
  
  duplicate_boolean = duplicated(filecodes_done)
  duplicate_index = which(duplicate_boolean)
  if (length(duplicate_index) > 0) {
    # cat(path_check_for_done)
    # cat('Duplicate DONE files found for: ', filecodes_done[duplicate_index], '\n')
    # cat('matching for: ', files_done[duplicate_index-1], '\n')
    # cat('matching for: ', filecodes_done[duplicate_index+1], '\n')
  }

  # INPUT
  if (length(files_to_process) != 0) {
    split_init = strsplit(files_to_process, .Platform$file.sep)
    files_input = sapply(split_init, tail, 1)
    split_file = strsplit(files_input, '_')
    filecodes_input = unlist(lapply(split_file, '[[', 1))
  } else {
    warning('There are no done files from your "input path"', '\n',
            '  -> now we cannot processing anything now!!!')
    filecodes_input = c('')
  }
    
  duplicate_boolean = duplicated(filecodes_input)
  duplicate_index = which(duplicate_boolean)
  if (length(duplicate_index) > 0) {
    # cat('Duplicate INPUT files found for: ', filecodes_input[duplicate_index], '\n')
    # cat('matching for: ', files_input[duplicate_index-1], '\n')
    # cat('matching for: ', filecodes_done[duplicate_index+1], '\n')
  }
  
  indices_done = filecodes_input %in% filecodes_done
  indices_undone = !indices_done
  number_of_undone = sum(indices_undone)
  
  cat('  .. found', number_of_undone, 'unprocessed input files\n')
  
  if (length(filecodes_done) > length(filecodes_input)) {
    indices_not_found_from_done = filecodes_done %in% filecodes_input
    number_of_not_found_from_done = sum(!indices_not_found_from_done)
    cat('  .. found', number_of_undone, 'unprocessed input files\n')
  }
  
  return(indices_undone)
  
}

