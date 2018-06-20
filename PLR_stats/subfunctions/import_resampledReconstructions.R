import.resampledReconstructions = function(data_path_traces, pattern_to_find, dataset_type) {
  
  # Get a file listing of the files found from the data folder
  files_to_read = list.files(path=data_path_traces, pattern=pattern_to_find, recursive=TRUE, full.names = TRUE)
  file_names = list.files(path=data_path_traces, pattern=pattern_to_find, recursive=TRUE, full.names = FALSE)
  
  subject_codes = list()
  for (i in 1:length(file_names)) {
    subject_codes_raw = c(subject_codes, strsplit(file_names[i], "_")[[1]][1])
    subject_codes = as.numeric(sub('PLR', '', subject_codes_raw))
  }
  
  cat('Reading in the reconstruced PLR traces\n')
  list = lapply(files_to_read, function(files_to_read){
    read.individual.trace.in(files_to_read)
  })
  
  # now we have a list that has as many elements as we files with
  # each time vector being for example int [1:1980] whereas we would
  # like each variable to be 2D Matrix
  cat(' -> Combining the read traces into a one huge list\n')
  list_huge = combine.list.dfs.to.one.list(list)
  
  # Convert then to a data frame
  # TODO! Maybe needless over memory overhead here if you a lot of traces
  # and not a lot of RAM
  # df = convert.huge.list.to.df(list_huge)
  
  return(list(list_huge, subject_codes))
  
}

# Read function to be looped through in the "lapply"
read.individual.trace.in = function(filename_path) {
  data_frame_in = read.csv(filename_path)
  return(data_frame_in)
}


# From list with individual data frames to a one
# large dataframe
combine.list.dfs.to.one.list = function(list) {
  
  no_of_files_in_list = length(list)
  var.names = colnames(list[[1]])
  no_of_vars = length(var.names)
  no_of_timepoints = length(list[[1]][[var.names[[1]]]])
  
  list_out = list()
  
  for (file_i in 1:no_of_files_in_list) {
  
    for (var_i in 1:no_of_vars) {
      
      var_name = var.names[[var_i]]
      
      # when operating on first file, initialize the empty matrices
      if (file_i == 1) {
        list_out[[var_name]] = matrix(data=NA, nrow=no_of_timepoints, 
                                               ncol=no_of_files_in_list)
      }
      
      # actually assign then
      vector_in = list[[file_i]][[var_name]]
      list_out[[var_name]][,file_i] = vector_in
      
    } # end of var_i
  } # end of file_i

  return(list_out)
  
}
  

# List into a data frame
convert.huge.list.to.df = function(list_huge) {

  # TODO! If this even makes any sense
  
  # this makes a 1980 obs of 9184 variable data frame:  
  # df = data.frame(list_huge)
  
  # https://www.rdocumentation.org/packages/qdapTools/versions/1.3.3/topics/list2df
  var.names = names(list_huge)
  no_of_vars = length(var.names)
  
  for (var_i in 1 : no_of_vars) {
    var_name = var.names[[var_i]]
    if (var_i == 1) {
      mat_2D = list_huge[[var_name]]
      # df_out = ?
    }
  }
}

