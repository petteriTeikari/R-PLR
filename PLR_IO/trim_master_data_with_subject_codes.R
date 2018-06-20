trim.master.data.with.subject.codes = function(master_data, filecodes) {
  
  codes_master = master_data$`Subject code`
  
  # remove the PLR prefix
  codes_in = vector(length=length(filecodes))
  for (i in 1 : length(filecodes)) {
    codes_in[i] = gsub('PLR','',filecodes[i])
  }
  
  corresponding_boolean = codes_in %in% codes_master
  corresponding_indices = which(corresponding_boolean)
  
  number_of_matches = sum(corresponding_boolean)
  missing_from_master = codes_in[!corresponding_boolean]
  
  cat('Missing from MASTER DATA SHEET: ', missing_from_master, '\n')
  
  # Check for duplicates
  unik <- !duplicated(codes_master)  ## logical vector of unique values 
  seq_along(codes_master)[unik]  ## indices 
  unique_master = master_data[unik,]
  
  codes_found = codes_in[corresponding_indices]
  master_boolean = unique_master$`Subject code` %in% codes_found
  number_of_master_matches = sum(master_boolean)
  
  master_data_out = unique_master[master_boolean,]
  found_from_master = codes_in %in% master_data_out$`Subject code`
  sum(found_from_master)
  
  return(list(master_data_out, found_from_master))
  
}