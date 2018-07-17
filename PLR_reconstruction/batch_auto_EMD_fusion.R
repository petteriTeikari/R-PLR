batch.auto.EMD.fusion = function(data_path, data_path_EMD, data_path_EMD_fusion, data_path_out,
                                  RPLR_recon_path, parameters,
                                  RPLR_paths, masterExcel,
                                  process_only_unprocessed = FALSE,
                                  path_check_for_done,
                                  pupil_col = 'pupil') {
  
  # get file listings
  files_imputed = list.files(path=data_path, pattern='*.csv', recursive=FALSE, full.names = TRUE)
  files_EMD = list.files(path=data_path_EMD, pattern='*.csv', recursive=FALSE, full.names = TRUE)
  files_EMD_fusion = list.files(path=data_path_EMD_fusion, pattern='*signals.csv', recursive=FALSE, full.names = TRUE)
  
  source(file.path(RPLR_recon_path, 'subfunctions', 'post_process_decomposition_IMFs.R', fsep = .Platform$file.sep))
  source(file.path(RPLR_recon_path, 'subfunctions', 'lowLevel_decomposition_wrappers.R', fsep = .Platform$file.sep))
  
  # go through the files
  for (file in 1: length(files_imputed)) {
    
    # IMPUTED
    df_in = read.csv(files_imputed[file])
    df_in = check.data.integrity(df_in, datatype = 'imputed')
    filecode_in = strsplit(tail(strsplit(files_imputed[file], .Platform$file.sep)[[1]],1), '_')[[1]][1]
    
    # EMD
    index_1 = find.corresponding.filecode(filecode = filecode_in, file_listing = files_EMD)
    if (is.na(index_1)) {
      warning('You are missing EMD for filecode = "', filecode_in, '" this should not be the case!')
    } else {
      df_EMD = read.csv(files_EMD[index_1])
      filecode_EMD = strsplit(tail(strsplit(files_EMD[index_1], .Platform$file.sep)[[1]],1), '_')[[1]][1]
    }
    
    # EMD Fusion
    index_2 = find.corresponding.filecode(filecode = filecode_in, file_listing = files_EMD_fusion)
    if (is.na(index_2)) {
      cat('You are missing EMD Fusion for filecode = "', filecode_in, '", we estimate now the correct fusion automagically')
      df_EMD_fusion = NA
    } else {
      df_EMD_fusion = read.csv(files_EMD_fusion[index_2])
      filecode_EMD_fusion = strsplit(tail(strsplit(files_EMD_fusion[index_2], .Platform$file.sep)[[1]],1), '_')[[1]][1]
      df_EMD_fusion = check.data.integrity(df_in = df_EMD_fusion, datatype = 'IMF_fusion')
    }
    
    # Combine the data for output
    df_out = combine.processing(df_in, df_EMD, df_EMD_fusion, path = data_path_EMD)
    
    # Save to disk
    just_filename = tail(strsplit(files_imputed[file], .Platform$file.sep)[[1]], 1)
    just_filename = gsub('_missForest', '', just_filename)
    
    export.pupil.dataframe.toDisk(df_out, just_filename,
                                  data_path_out, 'reconstructed')
    
  }
  
  
}

combine.processing = function(df_in, df_EMD, df_EMD_fusion, path) {
  
  # Do nothing for these guys basically
  df_out = df_in
  
  if (length(df_EMD_fusion) != 1) {
    
    denoised_array = cbind(df_EMD_fusion$hiFreq, df_EMD_fusion$loFreq, df_EMD_fusion$base)
    df_out[['denoised']] = rowSums(denoised_array, na.rm = TRUE)
    
  } else {
    
    cleaned = pre.clean.EMD.df(df_EMD)
    df_IMFs = cleaned[[1]] 
    names_IMF = cleaned[[2]]
    
    # TODO! Make this more intelligent at some point!
    IMF_index_estimates = estimate.imf.combination.indices(df_IMFs, input_type = '1stPass', 
                                                           path = path, verbose = FALSE)
    
    noise_array = cbind(df_IMFs$CEEMD_IMF_1, df_IMFs$CEEMD_IMF_2,
                           df_IMFs$CEEMD_IMF_3, df_IMFs$CEEMD_IMF_4,
                           df_IMFs$CEEMD_IMF_5)
    
    noise_sum = rowSums(denoised_array, na.rm = TRUE)
    df_out[['denoised']] = df_out[['pupil']] - noise_sum
    
  }
  
  # Renormalize
  # i1 = 296
  # i2 = 446 
  # baseline_in = median(df_out[['denoised']][i1:i2], na.rm = TRUE)
  # baseline_out = baseline_in # no idea of the original baseline
  # renormalize.value(baseline_out, baseline_in, vector = df_out[['denoised']], i1, i2)
  
  loess_decomp = loess.decomposition(t = df_out[['time']], y = df_out[['denoised']])
    df_out[['base']] = loess_decomp[[1]]
    df_out[['loFreq']] = loess_decomp[[2]]
    df_out[['hiFreq']] = loess_decomp[[3]]
  
  return(df_out)
  
}

find.corresponding.filecode = function(filecode, file_listing) {
  
  if (length(file_listing) == 0) {
    
    ind = NA
    
  } else {
  
    # filelisting
    step1 = sapply(file_listing, function(x) strsplit(x, .Platform$file.sep)[[1]], USE.NAMES=FALSE)
    step2 = sapply(step1[dim(step1)[1],], function(x) strsplit(x, '_')[[1]], USE.NAMES=FALSE)
    subject_codes = step2[1,]
    # color = ## TODO!
    
    # check if the filecode is found from file_listing
    ind = which(subject_codes %in% filecode) # will find both blue and red
    
    if (length(ind) == 0) {
      ind = NA
    }
    
  }
  
  return(ind)
  
  
}

check.data.integrity = function(df_in, datatype) {
  
  if (identical(datatype, 'imputed')) {
    df_in[['pupil_toBeImputed']] = df_in$pupil
    df_in[['pupil_toBeImputed']][df_in[['outlier_labels']] == 1] = NA
  } else if (identical(datatype, 'IMF_fusion')) {
    noiseArray = cbind(df_in[['noiseNorm']], df_in[['noiseNonNorm']])
    df_in[['denoised']] = rowSums(noiseArray, na.rm = TRUE)
  }
  
  return(df_in)
}
  
  