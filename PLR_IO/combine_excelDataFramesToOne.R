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
  
  # Combined after variable cleaning
  # https://www.statmethods.net/management/merging.html
  comb1 <- rbind(df_control2, df_glaucoma2)
  comb2 <- rbind(comb1, df_diabetes2)
  comb3 <- rbind(comb2, df_neuro2)
  
  cat(' .. -- After combining all groups, we have "', length(comb3$`Subject code`), '" observations (subjects)')
  cat(' .. -- --- and "', length(colnames(comb3)), '" variables (Excel columns)')
  
  return(comb3)
}