export.PLRfeats.asCSV = function(data_path_out, filename_CSV, filepath_CSV,
                                 data_frame_in, bins, features_blue, features_red,
                                 fractal_features, timefreq_features) {
  
  filename_CSV_blue = sub('.csv', '_blue.csv', filename_CSV) 
  filepath_CSV_blue = file.path(data_path_out, filename_CSV_blue, fsep = .Platform$file.sep)
  write.csv.per.color(features_blue, "blue", bins, filepath_CSV_blue) 

  filename_CSV_red = sub('.csv', '_red.csv', filename_CSV) 
  filepath_CSV_red = file.path(data_path_out, filename_CSV_red, fsep = .Platform$file.sep)
  write.csv.per.color(features_red, "red", bins, filepath_CSV_red) 
  
  # and write global features
  global_features = c(fractal_features, timefreq_features)
  filename_CSV_global = sub('.csv', '_global.csv', filename_CSV) 
  filepath_CSV_global = file.path(data_path_out, filename_CSV_global, fsep = .Platform$file.sep)
  write.csv.per.color(global_features, "global", bins = NA, filepath_CSV_global) 
  
  
}


write.csv.per.color = function(features, color, bins, filepath_CSV) {
  
  # NOTE! Human friendly over-commenting and "overuse" of variable names
  
  # FOR WRITING THE BIN-based FEATURES
  options(warn = -1)
  if (!is.na(bins)) {
    
    options(warn = 0)
    columnnames = colnames(bins) # the header names, e.g. "Name", "Method", .. as defined in bins.csv
    
    # Write headers
    # add two column per feature: 1) value and 2) uncertainty
    feature_cols = c("Value", "Uncertainty")
    new_col_headers = c(feature_cols, columnnames)
    
    # t() - transposes the column vector into a row vector
    write.table(t(new_col_headers), file = filepath_CSV, sep = ",", row.names=FALSE, col.names=FALSE)
    
    for (i in (1:length(features))) { # Bin Names (e.g. baseline, max constriction, etc.)
      
      data_row = list() # initialize 
      
      # read the feature value and uncertainty
      value = features[[i]]$value
      uncertainty = features[[i]]$uncertainty
      
      # and add to the initialized data_row list
      data_row = c(data_row, value, uncertainty)
      
      for (j in (1:length(columnnames))) { # Name, Method, Start, End, StartString
        
        # cat(i, ' ',j, '\n')
        # this would be for example "Baseline", "MaxConstrict"
        col_name = columnnames[j]
        rows_per_col = bins[[col_name]]
        new_col = rows_per_col[i] # match with correct [i], e.g. "Baseline", when i = 1
        data_row = c(data_row, new_col)
      } # end of j
      
      # No the datarow is complete and we write them to CSV row-by-row
      write.table(t(data_row), file = filepath_CSV, sep = ",", 
                  row.names=FALSE, col.names=FALSE, append=TRUE)
      
    } # end of i
    
  # For GLOBAL FEATURES such as the fractal and time-frequency scalars
  } else {
    
    feature_cols = c("Name", "Value", "Uncertainty") 
    write.table(t(feature_cols ), file = filepath_CSV, sep = ",", row.names=FALSE, col.names=FALSE)
    
    for (i in (1:length(features))) {
      
      data_row = list() # initialize 
      name = features[[i]]$name
      value = features[[i]]$value
      uncertainty = features[[i]]$uncertainty
      data_row = c(data_row, name, value, uncertainty) # and add to the initialized data_row list
      
      # No the datarow is complete and we write them to CSV row-by-row
      write.table(t(data_row), file = filepath_CSV, sep = ",", 
                  row.names=FALSE, col.names=FALSE, append=TRUE)
      
    }
  }
  
  
  
}