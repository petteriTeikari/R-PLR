combine.excelDataFramesToOne = function(df_control, df_glaucoma, df_diabetes, df_neuro, 
                                        vars_to_keep) {
  
  # TODO! put df's into a list or something so we do not fix the amount of dataframes 
  # that this function can take as input
  
  # This would also mean that you can more flexibly combine different sheet without
  # throwing too many cols away
  
  # vars_to_keep = vars_to_keep[vars_to_keep != "21-40"];
  # vars_to_keep = vars_to_keep[vars_to_keep != "40-55"];
  # vars_to_keep = vars_to_keep[vars_to_keep != "55-above"];
  # vars_to_keep = vars_to_keep[vars_to_keep != "Column3"]; # no idea what this is?
  
  # MAX!
  # TODO! Harmonize a bit your sheets, or what you are planning to do with your record-keeping
  # Hard to get the sheets now matched, I just take couple of values here
  # vars_to_keep = c("Subject code", "Age", "Gender", "Race", "Group")
  
  df_control2 = df_control[ , (names(df_control) %in% vars_to_keep)]
  df_glaucoma2 = df_glaucoma[ , (names(df_glaucoma) %in% vars_to_keep)]
  df_diabetes2 = df_diabetes[ , (names(df_diabetes) %in% vars_to_keep)]
  df_neuro2 = df_neuro[ , (names(df_neuro) %in% vars_to_keep)]
  
  # TODO! manual sheet selection
  # add the dystrophy / AD at some point
  ncol_control = ncol(df_control2) 
  ncol_glaucoma = ncol(df_glaucoma2)
  ncol_diabetes = ncol(df_diabetes2)
  ncol_neuro = ncol(df_neuro2)
  
  # Your columns are not equal length if this condition is met
  if (length(unique(c(ncol_control, ' ',   ncol_glaucoma, ' ',  ncol_diabetes, ' ', ncol_neuro))) != 1L) { 
    cat('\nYour sheets are not the same length! Fix your Master Data Sheet!')
    cat('\n', ' no of columns in Control sheet = ', ncol_control, '\n',   
        ' no of columns in Glaucoma sheet = ', ncol_glaucoma, '\n',  
        ' no of columns in Diabetes sheet = ', ncol_diabetes, '\n', 
        ' no of columns in Neuro sheet = ', ncol_neuro, '\n')
  } else {
    # cat('All columns the same length')
  }
  
  # Combined after variable cleaning
  # https://www.statmethods.net/management/merging.html
  comb1 <- rbind(df_control2, df_glaucoma2)
  comb2 <- rbind(comb1, df_diabetes2)
  comb3 <- rbind(comb2, df_neuro2)
  
  cat(' .. -- After combining all groups, we have "', length(comb3$`Subject code`), '" observations (subjects)')
  cat(' .. -- --- and "', length(colnames(comb3)), '" variables (Excel columns)')
  
  return(comb3)
}