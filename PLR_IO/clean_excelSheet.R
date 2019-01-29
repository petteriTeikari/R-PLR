clean.excelSheet = function(sheet_as_df, group_code) {
  
  # Sheet is stored as data frame (thus the _df)
  # ..
  # keeping the intermediate steps as separate variables,
  # not really a huge memory overhead from this
  
  # 1) remove rows that have NA subjects
  # https://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
  sheet_1 <- sheet_as_df[!(is.na(sheet_as_df$`Subject code`)),]
  # How to read this: ! means "not", so copying all the rows that do not match
  # the condition defined. IF SUBJECT CODE is NOT NA -> that row then it is copied to output
  
  # 2) remove the excluded the subjects
  sheet_2a <- sheet_1[!(sheet_1$`Study eye` == "Exclude"),]
  sheet_2b <- sheet_2a[!(sheet_2a$`Study eye` == "Excluded"),]
  
  # 3) convert the dates
  # The POSIXct might be a good format anyway
  # https://stackoverflow.com/questions/10089142/converting-hour-minutes-columns-in-dataframe-to-time-format
  # e.g. "2018-03-19 UTC"
  
  # FINALLY ADD the the group code which was as the Sheet Name in the 
  # Master Excel file
  
  # How many rows, in other words valid subjects left
  number_of_rows_left = length(sheet_2b$`Subject code`)
  
  # Replicate the group code chr (e.g. "Control") to a vector
  # that has as many as elements as there were rows
  vec = rep(group_code, number_of_rows_left) 
  sheet_2b$Group = vec
  
  rows_removed = length(sheet_as_df$`Subject code`) - number_of_rows_left
  cat('   .. Removed total of "', rows_removed, '" rows from', group_code, 'Excel sheet\n')
  
  return(sheet_2b)
  
}