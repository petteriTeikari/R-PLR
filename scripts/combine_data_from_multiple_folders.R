# Now we have three different folder containing columns that we want to combine
combine.data.from.multiple.folders = function(path_main, subfolder_paths, patterns,
                                              check_for_matching_cols = FALSE) {

  # install.packages("data.table")
  library(data.table)
  
  script.dir <- dirname(sys.frame(1)$ofile)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  source(file.path(IO_path, 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
  
  path_main = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/'
  subfolder_paths = c('imputation_final',
                      'recon_EMD',
                      file.path('recon_EMD', 'IMF_fusion', fsep = .Platform$file.sep))
  
  patterns = c('*_missForest.csv',
                '*_CEEMD.csv',
                '*_signals.csv')
  
  check_matching_cols = c(TRUE, FALSE, FALSE)
  
  data_path_out = file.path(path_main, 'reconstructed')
  

  # Get the file listings of all folders ------------------------------------
  
    file_listing = list()
    no_of_files = vector(, length=length(patterns))
    
    for (i in 1 : length(subfolder_paths)) {
      fullpath = file.path(path_main, subfolder_paths[i])
      colname = paste0('ind', i)
      file_listing[[colname]] = list.files(path=fullpath, pattern=patterns[i], recursive=FALSE, full.names = TRUE)
      no_of_files[i] = length(file_listing[[colname]])
    }
    
    # TODO! Check that lengths match, all folders should have the same amount of files
    # or check the filecode matches before adding 
    

  # Check that all the files in the same folder have same column names --------

    # In other words, if for one file you have the column hand-placed points missing, it is
    # easier for further processing if all the files have the same columns, just get the file
    # with the most column names and fill the others accordingly
    
    # TODO! This of course fails if you have some complicated errors, this part originally
    # wrote to add occasional missing columns due to too lazy programmer handling all the cases
    # when wanting processing done fast only
    
    if (check_for_matching_cols) {
    
      # Init matrices
      nrows = matrix(, nrow =length(file_listing[[1]]) , ncol = length(file_listing))
      ncols = matrix(, nrow =length(file_listing[[1]]) , ncol = length(file_listing))

      for (i in 1 : length(file_listing)) {
        
        colname = paste0('ind', i)
        
        # For example now the EMD results have different column lengths, as 
        # the number of IMFs is not fixed but depend on the signal
        if (check_matching_cols[i]) {
          
          for (f in 1 : length(file_listing[[colname]])) {
            filename = file_listing[[colname]][f]
            
            # https://stackoverflow.com/questions/32913151/is-it-possible-to-get-the-number-of-rows-in-a-csv-file-without-opening-it/32913255
            nrows[f,i] = length(count.fields(filename, skip = 1))  
            
            # https://www.rdocumentation.org/packages/data.table/versions/1.11.4/topics/fread
            ncols[f,i] = length(fread(filename, nrows=1)) 
            
            cat(paste0(i,'-', f), ' ')
          }
        }
      
      }
      
      # If all the headers have the same length, this just returns the first instance
      longest_col_indices = apply(ncols, 2, which.max)
      longest_col_indices = unlist(longest_col_indices[check_matching_cols])
      valid_col = which(check_matching_cols)
      
      # Read in, and see why it is so long
      longest_header = fread(file_listing[[valid_col]][2], nrows=1)
      names = colnames(longest_header)
      
    }
    

# Finally easy to combine -------------------------------------------------

    # TODO! Make more adaptive later
    vars_to_excl = c('X', 'X.1', 'X')
    vars_to_exclude_after = 'time' # don't have multiple time columns
    
    for (f in 1 : length(file_listing[[1]])) { 
      
      for (i in 1 : length(file_listing)) {
        
        # if this is the first folder in the list
        if (i == 1) {
          
          # read in
          df_out = read.csv(file_listing[[i]][f])
          
          # throw away some undesired columns
          vars_names_in = colnames(df_out)
          to_keep = is.na(match(vars_names_in, vars_to_excl))
          df_out = df_out[to_keep]
          
          # correct the time just in case it has been changed
          df_out[['time']] = df_out[['time']] - min(df_out[['time']])
          
        # for the subsequent folders, we have a data frame
        # to add the columns to
        } else {
          
          # read in
          df_in = read.csv(file_listing[[i]][f])
          
          # throw away some undesired columns
          vars_names_in = colnames(df_in)
          to_keep = is.na(match(vars_names_in, vars_to_exclude_after))
          
          df_out = cbind(df_out, df_in[to_keep])
        }
      } # end of folder
      
      # Define filename to be written
      filename_in = file_listing[[i]][f]
      split_filename = tail(strsplit(filename_in, .Platform$file.sep)[[1]],1)
      filecode = strsplit(split_filename, '_')[[1]][1]
      filename_out = paste0(filecode, '.csv')
      
      # replace the pupil value
      df_out[['pupil']] = df_out[['smooth']]
      
      # Write to disk, this over 2 MB
      export.pupil.dataframe.toDisk(df_out, filename_out, data_path_out, 'reconstruction')
      
      # Make a simple version, quicker to load from disk
      vars_to_keep_simple = c('time_onsetZero', 'pupil', 'error')
      to_keep_simple = !is.na(match(colnames(df_out), vars_to_keep_simple))
      df_out_simple = df_out[to_keep_simple]
      df_out_simple = df_out_simple[,c(3,1,2)] # reorder columns
    
      # make the time_onsetZero just time  
      names(df_out_simple) <- gsub(x = names(df_out_simple), pattern = "\\_onsetZero", replacement = "")  
      
      # Write the simplified to disk as well, this ~100 kb
      export.pupil.dataframe.toDisk(df_out_simple, filename_out, 
                                    file.path(data_path_out, 'simplified', fsep = .Platform$file.sep),
                                    'reconstruction_simplified')
      
    
    } # end of files
    
}



