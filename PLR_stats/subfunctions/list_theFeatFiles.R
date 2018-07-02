list.theFeatFiles = function(data_path, pattern_to_find, dataset_type) {
  
  files = list.files(path=data_path, pattern=pattern_to_find, recursive=TRUE)
  no_of_files = length(files)
  
  # Check that the input is correct
  # if (identical(dataset_type, 'blue_red')) {
  #   cat('Dataset type is "', dataset_type, '"\n')
  #   if ((no_of_files %% 2) == 0) {
  #     cat(no_of_files, ' files found for importing, from', no_of_files/2, 'subjects\n')
  #   } else  {  
  #     warning('There is odd number of fails, as each subject should have both RED and BLUE conditions!')
  #   }
  # } else {
  #   warning('Dataset type = ', dataset_type, ' not defined yet, or is this a typo?')
  # }
  
  files_fullpath = list.files(path=data_path, pattern=pattern_to_find, recursive=TRUE, full.names = TRUE)
  
}