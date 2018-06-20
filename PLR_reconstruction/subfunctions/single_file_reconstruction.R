single.file.reconstruction = function(t, y, error_frac, filecode, 
                                      master_data, found_from_master, param, vars_to_keep,
                                      debugTrim = FALSE) {
  
  if (debugTrim) {
    
    margins = 450 # 15*30
    zero_ind = which(t == 0)
    
    # trim to make functions run faster so you can test the code
    t = t[1 : 300]
    y = y[1 : 300]
    error_frac = error_frac[1 : 300]
  } 
  
  # Init modeling
  
    # TODO! Re-define if paths change
    source(file.path(param[['paths']][['source_path']], 'helper_functions.R', fsep = .Platform$file.sep))
    source(file.path(param[['paths']][['source_path']], 'data_imputation_wrapper.R', fsep = .Platform$file.sep))
    source(file.path(param[['paths']][['source_path']], 'lowLevel_imputation_wrappers.R', fsep = .Platform$file.sep))
    source(file.path(param[['paths']][['source_path']], 'data_denoising_wrapper.R', fsep = .Platform$file.sep))
    source(file.path(param[['paths']][['source_path']], 'lowLevel_denoising_wrappers.R', fsep = .Platform$file.sep))
    source(file.path(param[['paths']][['source_path']], 'data_decomposition_wrapper.R', fsep = .Platform$file.sep))
    source(file.path(param[['paths']][['source_path']], 'lowLevel_decomposition_wrappers.R', fsep = .Platform$file.sep))
    source(file.path(param[['paths']][['source_path']], 'data_joint_modeling_wrapper.R', fsep = .Platform$file.sep))
  
    # compute weights from fractional errors (error in relation to the mean signal)
    weights = 1 / error_frac^2
    weights_norm = weights / max(weights, na.rm = TRUE)
    
    #  # Convert to data frame
    # https://stackoverflow.com/questions/42696575/convert-a-matrix-to-a-2d-data-frame-in-r
    pupil_df = convert.vectors.into.dataframe(t, y, error_frac, weights_norm, filecode)
    
  # RECONSTRUCTION PARAMETERS ---------------------------------------------------------------- 
    
    param[['imputation']][['methods']] = c('imputeTS_kalman_StructTS')
    param[['denoising']][['methods']] = c('wmtsa_s8')
    param[['decomposition']][['methods']] = c('CEEMD')
    param[['joint']][['methods']] = c('1')
    
    # SETTINGS
    param[['imputation']][['fps']] = 30
    param[['imputation']][['debug_series']] = 1
    param[['denoising']][['debug_subject_to_plot']] = 'PLR4229'
    param[['decomposition']][['EMD_trials']] = 100
    
  # IMPUTATION ---------------------------------------------------------------- 
  
    imputed = data.imputation.wrapper(pupil_df, t, y, error_frac, weights_norm, filecode, param[['imputation']], master_data, found_from_master) 
      
  # DENOISING ---------------------------------------------------------------- 
  
    # denoised = data.denoising.wrapper(pupil_df, imputed, t, y, error_frac, weights_norm, filecode, param[['denoising']], master_data, found_from_master)
  
  # DECOMPOSITION ---------------------------------------------------------------- 

    decomposed = data.decomposition.wrapper(pupil_df, imputed, t, y, error_frac, weights_norm, filecode, param[['decomposition']], master_data, found_from_master)
  
  # JOINT ---------------------------------------------------------------- 
    
    # joint = data.joint.modeling.wrapper(pupil_df, t, y, error_frac, weights_norm, filecode, param[['joint']], master_data, found_from_master)
    
  # PACK Pupil Vectors to output ---------------------------------------------------------------- 
    
    # TODO! Make adaptive if you change the methods above!
    # output = list()
    
    # IMPUTATION
    # imputed_name = param[['imputation']][['methods']][[1]]
    # output[[imputed_name]] = imputed[[imputed_name]]$pupil
    # output[[paste(imputed_name, 'error', sep='_')]] = imputed[[imputed_name]]$error
    
    # DENOISING
    
    # DECOMPOSITION
    
    # JOINT MODELING
    
    names_decomp = names(decomposed) 
    decomp_out = decomposed[[names_decomp[1]]] # list
    str(decomp_out)
    
    # decomp_df = data.frame(residue=decomp_out$residue)
    
    decomp_list_out = list()
    EMD_METHOD = param[['decomposition']][['methods']][1]
    no_of_imfs_saved = dim(decomp_out$imf)[2]
    
    for (i in 1 : no_of_imfs_saved) {
      var_name = paste(EMD_METHOD, 'IMF', i, sep='_')
      decomp_list_out[[var_name]] = decomp_out$imf[,i]
    }
    
    var = 'residue'
    decomp_list_out[[var]] = decomp_out[[var]]
    
    decomp_df = data.frame(decomp_list_out)
    
    return(decomp_df)
      
}


