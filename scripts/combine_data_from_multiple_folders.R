# Now we have three different folder containing columns that we want to combine
combine.data.from.multiple.folders = function(path_main = NA, 
                                              RPLR_scripts_path = NA,
                                              subfolder_paths = c('imputation_final',
                                                                   'recon_EMD',
                                                                   file.path('recon_EMD', 'IMF_fusion', fsep = .Platform$file.sep)), 
                                              patterns = c('*.csv',
                                                           '*.csv',
                                                           '*_signals.csv'),
                                              check_for_matching_cols = FALSE) {

  
  
  if (is.na(path_main)) {
    path_main = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/'
    subfolder_paths = c('imputation_final',
                        'recon_EMD',
                        file.path('recon_EMD', 'IMF_fusion', fsep = .Platform$file.sep))
    
    patterns = c('*_missForest.csv',
                 '*_CEEMD.csv',
                 '*_signals.csv')
    
  } else {
    script.dir <- RPLR_scripts_path
    check_matching_cols = c(TRUE, FALSE, FALSE)
  }
  
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  source(file.path(IO_path, 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
  data_path_out = file.path(path_main, 'reconstructed')
  

  # Get the file listings of all folders ------------------------------------
  
    file_listing = list()
    no_of_files = vector(, length=length(patterns))
    
    for (i in 1 : length(subfolder_paths)) {
      
      fullpath = file.path(path_main, subfolder_paths[i])
      colname = paste0('ind', i)
      
      # sort by modification date, in descending order
      # So that the latest modification will be written as the subject code recording
      details = file.info(list.files(path=fullpath, pattern=patterns[i], recursive=FALSE, full.names = TRUE))
      details = details[with(details, order(as.POSIXct(mtime)), decreasing = TRUE), ]
      file_listing[[colname]] = rownames(details)
      
      # get rid of duplicates
      # TODO!
      # rename.folder.contents.to.remove.duplicates(file_listing[[colname]], subfolder_paths[i], fullpath)
      
      if (grepl('IMF', subfolder_paths[i])) {
        signals_ind = grepl('signals', file_listing[[colname]])
        file_listing[[colname]] = file_listing[[colname]][signals_ind]
      }
      
      # do file listing again
      file_listing[[colname]] = list.files(path=fullpath, pattern=patterns[i], recursive=FALSE, full.names = TRUE)
      no_of_files[i] = length(file_listing[[colname]])
    }
    
    # TODO! Check that lengths match, all folders should have the same amount of files
    # or check the filecode matches before adding 
    pairs = combn(length(file_listing),2)
    
    # https://stackoverflow.com/questions/22569176/r-permutations-and-combinations-with-without-replacement-and-for-distinct-non-d
    # perms = ?
    
    for (i in 1:dim(pairs)[2]) {
      
      i1 = pairs[1,i]
      i2 = pairs[2,i]
      list = file_listing[[i1]]
      path = file.path(path_main, subfolder_paths[i2])
      
      indices_undone = check.for.done.filecodes(files_to_process = list, 
                                                path_check_for_done = path)
      undone_files = list[indices_undone]
    
      # TODO! Does not work
      if (length(undone_files) > 0) {
        # cat('Your folder = ', subfolder_paths[i2], 'is missing the following:\n')  
        # cat(undone_files)  
      }
    }
 
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
    
    normalization_problem = list()
    
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
      names(df_out) <- gsub(x = names(df_out), pattern = "smooth", replacement = "denoised")  
      
      # Make sure that the normalization is correct 
      out = recheck.normalization(df_out, i, filecode)
        df_out = out[[1]]
        
        if (!is.na(out[[2]])) {
          normalization_problem = c(normalization_problem, f)
        }
      
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

recheck.normalization = function(df_out, i, filecode, indices = NA, baseline_in = NA,
                                 dataset = 'SERI') {
  
  # TODO!
  # Use now hard-coded normalization period
  if (is.na(indices[1])) {
    t = c(-5, 0)
    i1 = which.min(abs(df_out$time_onsetZero - t[1]))
    i2 = which.min(abs(df_out$time_onsetZero - t[2]))
  } else {
    i1 = indices[1]
    i2 = indices[2]
  }
  
  if (is.na(baseline_in)) {
    baseline_in = df_out$baseline[1]  
  } else {
    
  }
  
  baseline_out = median(df_out$pupil[i1:i2])
  
  if (length(baseline_out) == 0) {
    cat('No baseline vector found')
    baseline_out = median(df_out$missForest[i1:i2])  
    df_out$pupil = df_out$missForest
  }
  
  problem = NA
  
  col_names = colnames(df_out)
  vars_to_normalize = c('pupil_raw', 'pupil_blink_thresholded', 
                        'pupil_outlierfree', 'pupil_outlier_corrected',
                        'pupil_imputeTS_kalman_StructTS',
                        'pupil_StructTS_iter', 'pupil_toBeImputed', 
                        'missForest', 'denoised',
                        'noiseNorm', 'noiseNonNorm', 'noise',
                        'base', 'loFreq', 'hiFreq',
                        'loess_cpt', 'cpt_meanvar')
  
  to_normalize_ind = col_names %in% vars_to_normalize
  col_names_to_normalize = col_names[to_normalize_ind]
  
  if (1 == 1) {
  
    # cat('Baseline now = ', round(baseline_out, digits=2), '! Something wrong now! Renormalizing')
    
    renormalized = renormalize.value(baseline_out, baseline_in, vector = df_out$pupil, i1 = i1, i2 = i2, dataset = dataset)
      df_out$pupil = renormalized[[1]]
      df_out$baseline = renormalized[[2]]
      df_out$baseline_median = df_out$baseline
      df_out$baseline_mean = renormalized[[3]]
    
      # normalize.PLR.reduced(df_out, config_path = NA, 
      #                        value_operator = 'median',
      #                        normalize_method = 'hybrid',
      #                        denormalize_first = TRUE,
      #                        global_baseline = FALSE,
      #                        indices = c(i1, i2))
    
    # cat(' \n')
    baseline_out = median(df_out$pupil[i1:i2], na.rm = TRUE)
    problem = 1
    # cat('   ... After normalization we got the baseline to = ', round(baseline_out, digits=2), 
    #     ' for subject', filecode, '\n')
    
    
    
    for (c in 1 : length(col_names_to_normalize)) {
      
      renormalized = renormalize.value(baseline_out, baseline_in, 
                                       vector = df_out[[col_names_to_normalize[c]]], 
                                       i1 = i1, i2 = i2, dataset = dataset)
      
      df_out[[col_names_to_normalize[c]]] = renormalized[[1]]
    }
    
  }
  
  return(list(df_out, problem))
  
}

renormalize.value = function(baseline_out, baseline_in, vector, i1, i2, dataset = 'SERI') {
  
  vector_in_raw = vector*baseline_in/100 + baseline_in
  
  baseline_again = median(vector_in_raw[i1:i2], na.rm = TRUE)
  baseline_mean = mean(vector_in_raw[i1:i2], na.rm = TRUE)
  
  denominator = baseline_again
  nominator = baseline_again - vector_in_raw
  vector = nominator / denominator
  
  # to percentage
  vector = vector * -100  
  
  if (identical(dataset, 'SERI_2017')) {
    vector = vector * 100  
  }
  
  
  return(list(vector, baseline_again, baseline_mean))
    
}

rename.folder.contents.to.remove.duplicates = function(files, name_string, fullpath) {
  
  name_string = tail(strsplit(name_string, .Platform$file.sep)[[1]],1)
  
  for (f in 1 : length(files)) {
    
    filecode = strsplit(tail(strsplit(files[f], .Platform$file.sep)[[1]],1), '_')[[1]][1]
    just_filename = tail(strsplit(files[f], .Platform$file.sep)[[1]],1)
    
    # quick'n'dirty
    # TODO! Fix for more efficient
    df_in = read.csv(files[f])
    
    if (grepl('fusion', name_string)) {
      filename_out = paste0(filecode, '_signals.csv')
    } else {
      filename_out = paste0(filecode, '.csv')
    }
    
    fullpath_done = file.path(fullpath, 'to_be_deleted', fsep = .Platform$file.sep)
    # check if they exist or need to be created
    if (dir.exists(fullpath_done) == FALSE) {
      cat('Creating the subdirectory for ToBeDeleted: ', fullpath_done)
      dir.create(fullpath_done, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
    from = files[f]
    to = file.path(fullpath_done, just_filename, fsep = .Platform$file.sep)
    file.rename(from, to)
    
    export.pupil.dataframe.toDisk(df_in, filename_out, fullpath, name_string)
    
    
    
  }
  
}
