resample.and.trim.SERI2017 = function(path_in = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_final') {
  
  # INIT --------------------------------------------------------------------
  
    source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/PLR_IO/export_pupil_dataframe_toDisk.R')
  
    # Define computer-specific paths
    paths = list()
    paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
    
    path_out = file.path(path_in, '..', 'SERI_2017_final_resampled', fsep = .Platform$file.sep)
    path_out_ML = file.path(path_out, 'machineLearning', fsep = .Platform$file.sep)
  
    files = list.files(path=path_in, pattern='*.csv', recursive=FALSE, full.names = TRUE)
    diagnosis = read.table(list.files(path=path_in, pattern='*.txt', recursive=FALSE, full.names = TRUE),
                         stringsAsFactors = FALSE, sep = '\t', header = TRUE)
  
  # IMPORT FILES ------------------------------------------------------------  
    
    # preallocate
    blue_PLR = list()
    red_PLR = list()
    diff_PLR = list()
    diff_filecodes = list()
    diagnoses_out = list()
    
    # counters
    red_found = 1
    blue_found = 1
    diff_found = 1
    
    # check how much the indices vary to specify the trimming
    onset_indices = vector(,length = length(files))
    offset_indices = vector(,length = length(files))
    for (i in 1 : length(files)) {
      data_in = read.csv(files[i], stringsAsFactors = FALSE)
      onset_indices[i] = which(data_in$time_onsetZero == 0)
      offset_indices[i] = tail(which(data_in$light_on == 1),1)
      cat('.')
    }
    
    # manual outlier removal from short indices
    outlier_indices = onset_indices < 100
    files = files[!outlier_indices]
    onset_indices = onset_indices[!outlier_indices]
    offset_indices = offset_indices[!outlier_indices]
    diff = offset_indices - onset_indices
    
    # outlier removal based on short postlight durations
    postlight_duration = length(data_in$time) - offset_indices
    outlier_indices = postlight_duration < 700
    files = files[!outlier_indices]
    onset_indices = onset_indices[!outlier_indices]
    offset_indices = offset_indices[!outlier_indices]
    postlight_duration = postlight_duration[!outlier_indices]
    diff = diff[!outlier_indices]
    
    # get the limits for pre and post-light durations
    min_onset = min(onset_indices)
    min_offset = min(offset_indices)
    min_light_duration = min(diff)
    min_postlight_duration = min(postlight_duration)
    
    for (i in 1 : length(files)) {
      
      cat('. ')
      
      # metadata
      file_in = files[i]
      just_filename = tail(strsplit(files[i], .Platform$file.sep)[[1]],1)
      code_in = strsplit(just_filename, '_')[[1]][1]
      color_in = strsplit(just_filename, '_')[[1]][2]
      
      if (i > 1) {
        
        prev_filename = tail(strsplit(files[i-1], .Platform$file.sep)[[1]],1)
        code_prev = strsplit(prev_filename, '_')[[1]][1]
        color_prev = strsplit(prev_filename, '_')[[1]][2]
        
        # current index
        data_in = read.csv(file_in, stringsAsFactors = FALSE)
        zero_in = which(data_in$time_onsetZero == 0)
        light_offset_in = tail(which(data_in$light_on == 1),1)
        
        i1 = zero_in
        i2 = light_offset_in
        
        data_out = trim.and.resample(data_in, time = data_in$time,
                                     i1, i2, min_onset, 
                                     min_light_duration, min_postlight_duration)
        
        filename_out = paste0(toupper(code_in), '_', color_in, '.csv')
        
        # previous index
        data_prev = read.csv(files[i-1], stringsAsFactors = FALSE)
        zero_prev = which(data_prev$time_onsetZero == 0)
        light_offset_prev = tail(which(data_prev$light_on == 1),1)
        
        i1 = zero_prev
        i2 = light_offset_prev
        
        data_out_prev = trim.and.resample(data_prev, time = data_prev$time,
                                          i1, i2, min_onset, 
                                          min_light_duration, min_postlight_duration)
        
        filename_prev = paste0(toupper(code_prev), '_', color_prev, '.csv')
        
        
      } else {
        
        prev_filename = NA
        code_prev = NA
        color_prev = NA
        
      }
      
      
      if (identical(color_in, 'blue')) {
        blue_temp = data_in$pupil
        
        
      } else if (identical(color_in, 'red')) {
        red_temp = data_in$pupil
        
      }
      
      # TODO 
      # Compute the difference
      if (identical(code_in, code_prev)) {
        
        blue_PLR[[blue_found]] = blue_temp
        blue_found = blue_found + 1
        
        red_PLR[[red_found]] = red_temp
        red_found = red_found + 1
        
        diff_PLR[[diff_found]] = blue_PLR[[blue_found-1]] - red_PLR[[red_found-1]]
        diff_filecodes[[diff_found]] = code_in
        data_diff = diff.of.dataframes(data_out, data_out_prev)
        filename_diff = paste0(toupper(code_prev), '_', 'diff', '.csv')
        
        # export.pupil.dataframe.toDisk(data_out, filename_out, path_out, 'resampled')
        # export.pupil.dataframe.toDisk(data_out_prev, filename_prev, path_out, 'resampled')
        # export.pupil.dataframe.toDisk(data_diff, filename_diff, path_out, 'resampled')
        
        # save the diagnosis
        index_code = which(toupper(diagnosis$subject_code) %in% toupper(code_in))
        
        diagnoses_out[[diff_found]] = diagnosis$diagnosis[index_code]
        
        diff_found = diff_found + 1
        
      } 
      
    }
    
    # save the remaining diagnosis codes
    uniq = unique(diff_filecodes)
    index_codes = toupper(diagnosis$subject_code) %in% diff_filecodes
    
    diagnosis_out_df = data.frame(subject_code = diagnosis$subject_code[index_codes],
                                  diagnosis = diagnosis$diagnosis[index_codes],
                                  stringsAsFactors = FALSE)
    
    file = file.path(path_out, 'diagnosis_codes.txt', fsep = .Platform$file.sep) 
    write.table(diagnosis_out_df, file = file, quote = TRUE, sep = "\t", row.names = FALSE)
    
    # convert list into matrices
    # diagnoses = diagnosis$diagnosis[index_codes]
    # blue_PLR_mat = do.call(cbind, blue_PLR)
    # red_PLR_mat = do.call(cbind, red_PLR)
    # diff_PLR_mat = do.call(cbind, diff_PLR)
    # 
    # save.image(file = '/home/petteri/Dropbox/write_traces_v.RData') # creating ".RData"
    
    
    
  
}

diff.of.dataframes = function(data_out, data_out_prev) {
  
  to_excl = c('time', 'light_on', 'light_state', 'light_state_str',
              'baseline_in', 'X', 'outlier_labels', 'included_points',
              'excluded_points', 'handplaced_points', 'time_onsetZero')
  
  col_names = colnames(data_out)
  col_names_prev = colnames(data_out_prev)
  
  indices_to_keep = col_names %in% to_excl
  indices_to_keep_prev = col_names_prev %in% to_excl
  
  col_names = col_names[!indices_to_keep]
  col_names_prev = col_names_prev[!indices_to_keep_prev]
  
  if (length(col_names) != length(col_names_prev)) {
    
    ind_found = col_names %in% col_names_prev
    names_missing = col_names[!ind_found]
    ind_found2 = col_names_prev %in% col_names
    names_missing2 = col_names_prev[!ind_found2]
    
    if (length(ind_found) > length(ind_found2)) {
      col_names = col_names[ind_found]
    } else {
      col_names = col_names_prev[ind_found2]
    }
    
  }
  
  data_diff = list()
  
  for (c in 1 : length(col_names)) {
  
    prev = data_out_prev[[col_names[c]]]
    current = data_out[[col_names[c]]]
    
    if (length(prev) != 0 & length(current) != 0) {
      data_diff[[col_names[c]]] = prev - current
    }
    
  }
  
  # and copy the inputs
  for (excl in 1 : length(to_excl)) {
    data_diff[[to_excl[excl]]] = data_out[[to_excl[excl]]]
  }
  
  # data frame
  data_diff_df = data.frame(data_diff)
  
  return(data_diff)
  
}

trim.and.resample = function(data_in, time, i1, i2, min_onset, 
                             min_light_duration, min_postlight_duration) {
  
  to_excl = 'light_state_str'
  col_names = colnames(data_in)
  indices_to_keep = !(col_names %in% to_excl)
  col_names = col_names[indices_to_keep]
  data_out = data_in
  
  for (col in 1 : length(col_names)) {
      
    # before light onset
    time_pre = time[(i1-min_onset+1):i1]
    pre_light = data_in[[col_names[col]]][(i1-min_onset+1):i1]
    
    # during light
    time_light = time[i1:i2]
    light = data_in[[col_names[col]]][i1:i2]
    
      # interpolate to same length
      xout = seq(from = time_light[1], to = tail(time_light,1),
                 length.out = min_light_duration)
      
      # Need at least two non-NA values for interpolation
      if ( (length(light) - sum(is.na(light))) > 2) {
        out = approx(time_light, light, xout=xout, method = "linear")
        light_interp = out$y
      } else {
        light_interp = vector(,length(xout))
        light_interp[] = NA 
      }
    
    # after 
    time_post = time[(i2+1):(i2+1+min_postlight_duration)]
    post_light = data_in[[col_names[col]]][(i2+1):(i2+1+min_postlight_duration)]
    
    # put back together
    time_fused = c(time_pre, time_light, time_post)
    data_fused = c(pre_light, light, post_light)
    
    na_time = is.na(time_fused)
    data_fused = data_fused[!na_time]
    time_fused = time_fused[!na_time]
    
    
    # Interpolate back to input length
    time_final = seq(from = time_fused[1], to = tail(time_fused,1),
                     length.out = length(time))
    
    # Need at least two non-NA values for interpolation
    if ( (length(light) - sum(is.na(light))) > 2) {
      out_final = approx(time_fused, data_fused, xout = time_final)
      data_final = out_final$y
    } else {
      data_final = vector(,length(time_final))
      data_final[] = NA 
    }
    
    
    
    # put back to the data frame
    data_out[[col_names[col]]] = data_final
    
  }
  
  # manual fixes
  data_out$light_state_str = data_out$light_state
  data_out$light_state_str[data_out$light_state_str == 0] = 'pre_light'
  data_out$light_state_str[data_out$light_state_str == 1] = 'light'
  data_out$light_state_str[data_out$light_state_str == 2] = 'post_light'
  
  data_out$time = data_out$time - min(data_out$time)
  
  # re-define the "time_onsetZero"
  light_onset = which(data_out$light_on == 1)[1]
  data_out$time_onsetZero = data_out$time - data_out$time[light_onset]
  
  return(data_out)
  
}


