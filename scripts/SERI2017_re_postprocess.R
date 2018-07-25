SERI2017.re.postprocess = function() {
  
  # Define computer-specific paths
  # TODO! Make more adaptive
  paths = list()
  paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
  paths[['data_in']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017'
  paths[['data_out']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_EMD'
  
  # Init scripts
  source(file.path(paths[['RPLR']][['base']], 'clean_and_reconstruct_all_PLR.R', fsep = .Platform$file.sep))
  paths = init.paths.and.functions(paths)
  paths = source.subfunctions(paths)
  import.and.install.libraries()
  param = init.PLR.processing.params(paths)
  paths[['data_in']][['EMD']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_denoisingFusion/imputation_final'
  paths[['data_out']][['EMD_fusion']] = file.path(paths[['data_out']][['base']], 'IMF_fusion', fsep = .Platform$file.sep)
  paths[['data_out']][['EMD_denoising']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_denoisingFusion'
  paths[['data_out']][['EMD_denoising_corr']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/imputation_correction'
  
  # quick'n'dirty sourcing, TODO!
  source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/PLR_reconstruction/batch_auto_EMD_fusion.R')
  source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/scripts/combine_data_from_multiple_folders.R')
  
  library(doParallel)
  registerDoParallel(cores=4)
  
  # Impute the missing values
  batch.AnalyzeAndReImpute(data_path =  paths[['data_out']][['EMD_denoising_corr']], 
                           RPLR_recon_path = paths[['recon']],
                           parameters = param[['recon']],
                           RPLR_paths = paths[['RPLR']],
                           masterExcel = paths[['data_in']][['excelMasterPath']],
                           process_only_unprocessed = FALSE, # no need to re-process all 400 files
                           path_check_for_done = paths[['data_out']][['reconstructed']], 
                           pupil_col = 'pupil_imputation_corrected',
                           smooth_trace_before_imputation = TRUE,
                           combine_with_database = FALSE,
                           vars_to_keep = c('time', 'pupil', 'error'))

  # Run Empirical Mode Decomposition for denoising
  batch.EMD.decomposition(data_path = paths[['data_in']][['EMD']], 
                          data_path_out = paths[['data_out']][['base']],
                          RPLR_recon_path = paths[['recon']],
                          parameters = param[['recon']],
                          RPLR_paths = paths[['RPLR']],
                          masterExcel = paths[['data_in']][['excelMasterPath']],
                          process_only_unprocessed = FALSE,
                          path_check_for_done = paths[['data_out']][['reconstructed']],
                          pupil_col = 'pupil',
                          dataset_string = 'SERI_median')
  
  # Do quick'n'dirty fusion of the EMD results (with our without proper fusion)
  batch.auto.EMD.fusion(data_path = paths[['data_in']][['EMD']], 
                        data_path_EMD = paths[['data_out']][['base']],
                        data_path_EMD_fusion = paths[['data_out']][['EMD_fusion']],
                        data_path_out = paths[['data_out']][['EMD_denoising']],
                        RPLR_recon_path = paths[['recon']],
                        parameters = param[['recon']],
                        RPLR_paths = paths[['RPLR']],
                        masterExcel = paths[['data_in']][['excelMasterPath']],
                        process_only_unprocessed = FALSE,
                        path_check_for_done = paths[['data_out']][['reconstructed']],
                        pupil_col = 'pupil')
  
  # We have still the raw traces with a lot of crap in them and we could combine
  # them with the cleaned ones to have all the traces in one .csv per subject
  combine.different.SERI.folders(raw_path = '/home/petteri/Dropbox/Data/SERI_PLR',
                                 raw_csv_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_raw',
                                 EMD_path = paths[['data_out']][['base']],
                                 fusion_path = paths[['data_out']][['EMD_denoising']],
                                 output_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_final')
  
  
}


combine.different.SERI.folders = function(raw_path, raw_csv_path, EMD_path, fusion_path, output_path) {
 
  import.SERI.tda.folders(raw_path, path_out = raw_csv_path)
  combine.raw.with.processed(raw_csv_path, EMD_path, fusion_path, output_path)
  
}


combine.raw.with.processed = function(raw_csv_path, EMD_path, fusion_path, output_path,
                                      pattern = '*.csv') {
  
  files = list()
  files[['raw']] = list.files(path=raw_csv_path, pattern=pattern, recursive=FALSE, full.names = TRUE)
  files[['EMD']] = list.files(path=EMD_path, pattern=pattern, recursive=FALSE, full.names = TRUE)
  files[['fusion']] = list.files(path=fusion_path, pattern=pattern, recursive=FALSE, full.names = TRUE)

  file.fusion(files, output_path)
    
}

file.fusion = function(files, output_path) {
  
  filecodes = list()
  filecodes[['raw']] = get.subjectcodes.of.listing(file_listing = files[['raw']])
  filecodes[['EMD']] = get.subjectcodes.of.listing(file_listing = files[['EMD']])
  filecodes[['fusion']] = get.subjectcodes.of.listing(file_listing = files[['fusion']])
  
  colors = list()
  colors[['raw']] = get.subjectcodes.of.listing(file_listing = files[['raw']], field_out = 'color')
  colors[['EMD']] = get.subjectcodes.of.listing(file_listing = files[['EMD']], field_out = 'color')
  colors[['fusion']] = get.subjectcodes.of.listing(file_listing = files[['fusion']], field_out = 'color')
  
  combine.files(files, filecode, colors, output_path)
    
}

combine.files = function(files, filecode, colors, output_path) {
  
  lengths = c(length(filecodes[['raw']]), 
              length(filecodes[['EMD']]), 
              length(filecodes[['fusion']]))
  
  indices = 1:length(filecodes)
  shortest_list_ind = which.min(lengths)
  not_ind = !(indices %in% shortest_list_ind)
  indices_rem = indices[not_ind]
  
  list_out = list()
  
  folder_names = names(filecodes)
  
  for (i in 1 : lengths[shortest_list_ind]) {
    
    # The shortest data
    filecode_in = filecodes[[shortest_list_ind]][i]
    color_in = colors[[shortest_list_ind]][i]
    data_in = read.csv(files[[shortest_list_ind]][i])
    
    # the remaining longer data,
    # TODO! hard-coded now for two remaining folders
    longer1_indices_code = filecodes[[indices_rem[1]]] %in% filecode_in
    longer1_indices_color = colors[[indices_rem[1]]] %in% color_in
    longer1_index = which(longer1_indices_code & longer1_indices_color)
    
    longer2_indices_code = filecodes[[indices_rem[2]]] %in% filecode_in
    longer2_indices_color = colors[[indices_rem[2]]] %in% color_in
    longer2_index = which(longer2_indices_code & longer2_indices_color)
    
    # Do not add this _in file if there is no matching files from other worlds
    if (length(longer1_index) != 0 & length(longer2_index) != 0) {
      
      data_longer1 = read.csv(files[[indices_rem[1]]][longer1_index])
      data_longer2 = read.csv(files[[indices_rem[2]]][longer2_index])
      
      # get the raw file to have the raw pupil vector for computing the baseline
      raw_index = which(grepl('raw', folder_names))
      raw_data = read.csv(files[[raw_index]][i])
      pupil_index = which(grepl('pupil', colnames(raw_data)))
      pupil_vector = raw_data[[pupil_index]]
      light_vector = raw_data[['light_on']]
      time_vector = raw_data[['time']]
      
      baseline_indices = get.baseline.indices(light_vector, time_vector)
      df_in = normalize.PLR.reduced(df = raw_data, indices = baseline_indices, pupil_col = 'pupil_raw')
      baseline_in = df_in$baseline[1]
      
      # Filter (i.e. remove unneeded columns and normalize)
      
      data_in = filter.df(df_in = data_in, listing_folder = folder_names[[shortest_list_ind]], 
                          baseline_indices, baseline_in, i = i, filecode = filecode_in)
      
      data_longer1 = filter.df(df_in = data_longer1, listing_folder = folder_names[[indices_rem[1]]], 
                               baseline_indices, baseline_in, i = i, filecode = filecode_in)
      
      data_longer2 = filter.df(df_in = data_longer2, listing_folder = folder_names[[indices_rem[2]]], 
                               baseline_indices, baseline_in, i = i, filecode = filecode_in)
      
      # combine dataframes
      df = cbind(data_longer1, data_longer2, data_in)
      
      # create time vector with 0 at light onset
      light_onset_index = which(df$light_on == 1)[1]
      df$time_onsetZero = df$time - df$time[light_onset_index]
      
      filename_out = paste0(toupper(filecode_in), '_', color_in, '.csv')
      export.pupil.dataframe.toDisk(df, filename_out, output_path, 'full_data')
      
      # list_out[[i]] = df
      
    } else {
      
      warning('Not processing at all subject = ', filecode_in, ' (color = ', color_in, ') as it did not have counterpart(s)\n',
              '   "', folder_names[[indices_rem[1]]], '", index matching the folder "', folder_names[[shortest_list_ind]], '" = ', longer1_index, '\n',
              '   "', folder_names[[indices_rem[2]]], '", index matching the folder "', folder_names[[shortest_list_ind]], '" = ', longer2_index)    
      
    }
    
    
  }
  
  
}

filter.df = function(df_in, listing_folder, baseline_indices, baseline_in, 
                     i = NA, filecode = NA,
                     excl_var = c('time', 'pupil_StructTS_iter')) {
  
  if (!identical(listing_folder, 'raw')) {
    
    col_names = colnames(df_in)
    indices = !(col_names %in% excl_var)  
    df_out = df_in[indices]    
    
    if (identical(listing_folder, 'fusion')) {
      norm = recheck.normalization(df_out, i, filecode = filecode_in, 
                            indices = baseline_indices, baseline_in = baseline_in,
                            dataset = 'SERI_2017')
    
      df_out = norm[[1]]
    }
    
  } else {
    
    df_in$light_state_str = df_in$light_state
    df_in$light_state_str[df_in$light_state_str == 0] = 'pre_light'
    df_in$light_state_str[df_in$light_state_str == 1] = 'light'
    df_in$light_state_str[df_in$light_state_str == 2] = 'post_light'
    
    df_in$baseline_on = as.numeric(vector(, length(df_in$light_state)))
    df_in$baseline_on[baseline_indices[1]:baseline_indices[2]] = 1
    
    df_in = normalize.PLR.reduced(df = df_in, indices = baseline_indices, pupil_col = 'pupil_raw')
    
    df_out = df_in
  }
  
  
  
  return(df_out)
  
}

get.baseline.indices = function(light_vector, time_vector,
                                time_window = c(-5, 0)) {
  
  # TODO! If you want other ways how to compute the baseline instead of this hybrid method.
  # add here, or start using the R-PLR functions (better way)
  light_on = which(light_vector == 1)[1] # onset
  time_onset = time[light_on] - time_window[2]
  time_baseline_start = time_onset + time_window[1]

  baseline_start_ind = which.min(abs(time_baseline_start - time))  
  baseline_end_ind = light_on -1
  
  baseline_indices = c(baseline_start_ind, baseline_end_ind)

  return(baseline_indices)
  
}

get.subjectcodes.of.listing = function(file_listing, field_out = 'subjectcode') {
  
  step1 = sapply(file_listing, function(x) strsplit(x, .Platform$file.sep)[[1]], USE.NAMES=FALSE)
  
  filenames = sapply(step1[dim(step1)[1],], function(x) tail(x, 1), USE.NAMES=FALSE)
  split_fields = sapply(filenames, function(x) strsplit(x, '_')[[1]], USE.NAMES=FALSE)
  
  subject_codes = toupper(sapply(split_fields[1,], function(x) x[1], USE.NAMES=FALSE))
  colors = sapply(split_fields[2,], function(x) x[1], USE.NAMES=FALSE)

  if (identical(field_out, 'subjectcode')) {
    return(subject_codes)    
  } else {
    return(colors)  
  }
  
  
}

import.SERI.tda.folders = function(raw_path, path_out,
                                   pattern = '.tda',
                                   condition = '50s') {
  
  # Get all the .tda files first
  files = list.files(path=raw_path, pattern=pattern, recursive=TRUE, full.names = TRUE)
  indices_red = grepl('_r_', files)
  indices_blue = grepl('_b_', files)
  
  # then pick onlt the ones with "condition" in their filename
  indices = grepl(condition, files)
  files_condition = files[indices]
  
  # split into red and blue
  red_condition = indices_red & indices
  blue_condition = indices_blue & indices
  files_red = files[red_condition]
  files_blue = files[blue_condition]
  
  # get the diagnosis, strsplit for R list
  step1 = sapply(files, function(x) strsplit(x, .Platform$file.sep)[[1]], USE.NAMES=FALSE)
  
  filenames = sapply(step1[], function(x) tail(x, 1), USE.NAMES=FALSE)
  split_fields = sapply(filenames, function(x) strsplit(x, '_')[[1]], USE.NAMES=FALSE)
  subject_codes = sapply(split_fields, function(x) x[1], USE.NAMES=FALSE)
  
  red_codes = subject_codes[red_condition]
  blue_codes = subject_codes[blue_condition]
  
  no_of_fields = length(step1[[1]])
  diagnoses = sapply(step1, function(x) x[(no_of_fields-3) : (no_of_fields-2)], USE.NAMES=FALSE)
  diagnoses = paste(diagnoses[1,], diagnoses[2,], sep='_')
  
  red_diagnoses = diagnoses[red_condition]
  blue_diagnoses = diagnoses[blue_condition]
  
  # Now actually import the .tdas
  red_list_of_DFs = list()
  blue_list_of_DFs = list()
  save_to_disk = TRUE
  
  for (i in 1 : length(files_red)) {
    red_list_of_DFs[[i]] = import.tda.file(file_in = files_red[i], color = 'red',
                                            diagnosis = red_diagnoses[i],
                                            subject_code = red_codes[i],
                                            save_to_disk = save_to_disk,
                                            path_out = path_out)  
  }
  
  for (i in 1 : length(files_blue)) {
    blue_list_of_DFs[[i]] = import.tda.file(file_in = files_blue[i], color = 'blue',
                                           diagnosis = blue_diagnoses[i],
                                           subject_code = blue_codes[i],
                                           save_to_disk = save_to_disk,
                                           path_out = path_out)  
  }
  
  # TODO! If you actually want to return the DFs instead of reading back 
  # from the disk, add that here
  
  # get unique subject codes
  uniq = !duplicated(subject_codes)
  uniq_codes = subject_codes[uniq]
  uniq_diagnoses = diagnoses[uniq]
  
  diagnosis_df = data.frame(subject_code = uniq_codes, diagnosis = uniq_diagnoses,
                            stringsAsFactors = FALSE)
  
  file = file.path(path_out, 'diagnosis.txt', fsep = .Platform$file.sep) 
  write.table(diagnosis_df, file = file, quote = TRUE, sep = "\t", row.names = FALSE)
  
}


import.tda.file = function(file_in, color = 'unknownColor', diagnosis = NA, subject_code = NA,
                           fps = 120, length_out = 1981, save_to_disk = TRUE,
                           path_out = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_raw') {
  
  skip_rows = 25
  data = read.table(file_in, skip=skip_rows, header = FALSE, sep = "\t", 
                   blank.lines.skip = TRUE,
                   row.names = NULL)
  
  colnames(data) = c('Sample', 'H1', 'V1', 'D1', 'LightOn')
  
  data$LightOn[data$LightOn == 48] = 0
  data$LightOn[data$LightOn == 49] = 1
  
  data$time = data$Sample / fps
  time_new = seq(from = data$time[1], to = tail(data$time,1),
                 length.out = length_out)
  
  y_new = approx(data$time, y = data$D1, time_new, method = "linear")
  light = approx(data$time, y = data$LightOn, time_new, method = "linear")
  
  time = time_new
  pupil = y_new$y
  light_on = light$y
  
  # pre-light, light, and post-light "states" for PLR modeling,
  # i.e. pupil size changes more during light
  light_state = light_on
  last_light_on = tail(which(light_on == 1),1)
  
  if (length(last_light_on) == 0) {
    warning('for some reason, no light trigger is found from file = ', file_in, '\n',
            'not saving to disk at all!')
    
    return(NA)
    
  } else {
    
    light_state[(last_light_on+1):length(time)] = 2
    light_state_str = light_state
    light_state_str[light_state_str == 0] = 'pre_light'
    light_state_str[light_state_str == 1] = 'light'
    light_state_str[light_state_str == 2] = 'post_light'
    
    df_out = data.frame(time = time, pupil_raw = pupil, light_on = light_on,
                        light_state = light_state, light_state_str = light_state_str,
                        stringsAsFactors = FALSE)
    
    # plot(data$time, data$D1, type ='l')
    # plot(time, pupil, type ='l')
    
    if (save_to_disk) {
      filename_out = paste0(toupper(subject_code), '_', color, '.csv')
      export.pupil.dataframe.toDisk(df_out, filename_out, path_out, 'raw')
    }
    
    return(df_out)
    
  }
  
  
  
}
