read.theMasterExcel = function(masterXLS_data_path = NA, filename = NA, fullpath = NA) {
  
  if (is.na(fullpath)) {
    fullpath = file.path(masterXLS_data_path, filename, fsep = .Platform$file.sep)
  }
  
  group_codes = c("Control", "Glaucoma", "Diabetes", "Exceptional Cases (Neuro)")
  
  # From different sheets, "hard-coded names now", all imported as data frames
  # TODO, you could "loopify" these.
  # Contain 23 variables (4 April 2018)
  excel_data_control = read_excel(fullpath, sheet = group_codes[1])
  excel_data_glaucoma = read_excel(fullpath, sheet = group_codes[2])
  excel_data_diabetes = read_excel(fullpath, sheet = group_codes[3])
  excel_data_neuro = read_excel(fullpath, sheet = group_codes[4])
  
  # TODO! R opening with a password?
  # XLConnect (0.2-13) can now read password protected excel files
  # https://stackoverflow.com/questions/35852722/how-do-you-read-a-password-protected-excel-file-into-r
  
  # Clean now individual sheets, output still data frames
  # Contain 24 variables as we add the Group "key" (4 April 2018)
  df_control = clean.excelSheet(excel_data_control, group_codes[1])
  df_glaucoma = clean.excelSheet(excel_data_glaucoma, group_codes[2])
  df_diabetes = clean.excelSheet(excel_data_diabetes, group_codes[3])
  df_neuro = clean.excelSheet(excel_data_neuro, group_codes[4])
  
  # NOTE! If you are interested in checking in R, all the variables in the original
  # Excel master sheet, here is your chance! A lot of the variables are thrown away
  # now at combining stages as a lot of variables are missing from different sheets
  
  # Easier to construct single data frame from all the the different data frames
  # However that would require that we have the same number of variables (i.e. Excel columns)
  # This is not the case on "4 April 2018" 
  
  # Define the variables to keep (modified furthermore inside the following subfunction)
  vars_to_keep = colnames(df_glaucoma) # "start from all the glaucoma variables"
  
  # Combine then
  df_master = combine.excelDataFramesToOne(df_control, df_glaucoma, df_diabetes, df_neuro, 
                                           vars_to_keep)
  
  # Add custom variables here if wanted (i.e. something that can be determined
  # from other variables, like the age_grouping)
  custom_vars = c("21-40", "40-55", "55-above")
  df_master = add.customVariablesToDataframe(df_master, custom_vars)
  
  return(df_master)
  
}