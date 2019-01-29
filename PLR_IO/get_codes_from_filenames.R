get.codes.from.filenames = function(filepaths) {
  
  codes_out = list()
  
  for (i in 1 : length(filepaths)) {
    
    filename_sep = strsplit(filepaths[i], .Platform$file.sep)[[1]]
    just_filename = tail(filename_sep, n=1)
    just_path = gsub(just_filename, '', filepaths[i])
    
    codes_out = c(codes_out, strsplit(just_filename, '_')[[1]][1])
    
  }
  
  return(codes_out)
  
}