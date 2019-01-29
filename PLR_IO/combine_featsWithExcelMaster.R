combine.featsWithExcelMaster = function(files, master_data) {

  # TODO! This function is quite slow actually, profile this and think
  # of optimization of this!
  
  # Go through all the processed feature of the PLR session
  # The number of file in practice is less than the 
  # rows in an e
  datalist = list()
  
  master_data_vars_added = master_data

  for (i in 1:length(files)) {
    
    # Get subject code from filename
    subject_code = get.subjectCodeFromFilename(files[i])
    
    # Get the color from filename 
    color = get.colorFromFilename(files[i])
    
    imported = import.feat.file(files[i], color)
      new_feats_per_file = imported[[1]]
      
      blue_tmp = imported[[2]]
      red_tmp = imported[[3]]
      global_tmp = imported[[4]]
      
      # add only if the derived feats are non-empty
      # QUick'n'dirty fix from refactoring break of code
      if (!check.if.derived.names.empty(names = blue_tmp)) {
        derived_feats_names_blue = blue_tmp 
      }
      if (!check.if.derived.names.empty(names = red_tmp)) {
        derived_feats_names_red = red_tmp
      }
      if (!check.if.derived.names.empty(names = global_tmp)) {
        derived_feats_names_global = global_tmp
      }
    
    # if the variable names are not found from the Master Sheet, add
    # them, i.e. we now computed blue_Baseline, red_baseline for example
    # here that are not found from the Excel file
    master_data_vars_added = add.variableInLoopToDataframe(new_feats_per_file, master_data_vars_added)
    
    # Now find the matching subject code from the Master file
    subject_index = find.subjectCodeFromMasterFile(subject_code, master_data_vars_added)
    
    # Finally add all the new features for the subject index (the row number in 
    # other words)  
    master_data_vars_added = add.theFeatsPerSubjectToMaster(master_data_vars_added, new_feats_per_file, subject_index)
    
    # DEBUG
    col_to_check = master_data_vars_added[[colnames(new_feats_per_file)[1]]]
    number_of_subjects_added = sum(!is.na(col_to_check), na.rm=TRUE)
    
    # cat('  ', i, ') PLR', subject_code, ' [index = ', subject_index, '] - color: ', color, 
    #         ' --> subjects added at this point = ', number_of_subjects_added, '\n')
    cat(subject_code, ' ')
  }
  
  # combine the names from blue and red
  derived_feats_names = c(derived_feats_names_blue, derived_feats_names_red, derived_feats_names_global)
  
  # remove possible NAs
  derived_feats_names = derived_feats_names[!is.na(derived_feats_names)]
  
  if (number_of_subjects_added == length(files)) {
    cat('All the PLR recordings had a match (with Red and Blue feats done) from the Master Excel File')
  }
  return(list(master_data_vars_added, derived_feats_names))
  
}
                    
  

# SUBFUNCTIONS ------------------------------------------------------------

check.if.derived.names.empty = function(names) {
  
  ratio_of_NAs = sum(is.na(names)) / length(names)
  if (ratio_of_NAs == 1) {
    is_empty = TRUE 
  } else {
    is_empty = FALSE
  }
  return(is_empty)
}

get.subjectCodeFromFilename = function(file) {
  
  filename_sep = strsplit(file, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  filename_split = strsplit(just_filename,'_')
  first_field_of_filename = unlist(filename_split)[1]
  subject_code = gsub('PLR', '', first_field_of_filename)
  
  return(subject_code)
  
}

get.colorFromFilename = function(file) {
   
  filename_sep = strsplit(file, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  filename_split = unlist(strsplit(just_filename, '_'))
  end_of_file = tail(filename_split, n=1)
  color = sub('.csv', '', end_of_file)
  
  return(color)
  
}



add.variableInLoopToDataframe = function(new_feats_per_file, master_data_out) {
  
  # The column names of the both input arguments
  vars_to_check = colnames(new_feats_per_file)
  master_vars = colnames(master_data_out)
  
  # https://stackoverflow.com/questions/1169248/test-if-a-vector-contains-a-given-element
  # Check if the new vars are found from the master variables
  vars_found_logical = vars_to_check %in% master_vars
  no_new_vars_to_add = sum(!vars_found_logical) # how many vars not found
  
  # Remove the variables that are already found from the Master file
  vars_to_add = vars_to_check[!vars_found_logical]
  
  # https://stackoverflow.com/questions/18214395/r-add-empty-columns-to-a-dataframe-with-specified-names-from-a-vector
  # add these new variables to master excel file
  master_data_out[,vars_to_add] <- as.numeric(NA)
  
  return(master_data_out)
  
}

find.subjectCodeFromMasterFile = function(subject_code, master_data_out) {
  
  subject_code_index = match(subject_code, master_data_out$`Subject code`)
  
  # check if the subject code was actually found
  if (!is.na(subject_code_index)) {
    # found okay
  } else {
    warning('No subject="', subject_code, '" was found from the Excel Master File!')
  }
  
  return(subject_code_index)
}

 add.theFeatsPerSubjectToMaster = function(master_data_vars_added, new_feats_per_file, subject_index) {
   
   varnames = colnames(new_feats_per_file)
   master_data_out = master_data_vars_added
   
   for (i in 1:length(new_feats_per_file)) {
     master_data_out[[varnames[i]]][subject_index] = new_feats_per_file[[varnames[i]]]
   }
   
   # Debug
   # cat('Before assigning to', varnames[length(new_feats_per_file)])
   # str(master_data_vars_added[[varnames[length(new_feats_per_file)]]][subject_index])
   # cat('After assigning to', varnames[length(new_feats_per_file)])
   # str(master_data_out[[varnames[length(new_feats_per_file)]]][subject_index])
   
   return(master_data_out)
   
 }