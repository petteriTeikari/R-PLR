dataset.modeling.wrapper = function(t, y, error_frac, filecodes, param, master_data, found_from_master) {

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
    pupil_df = convert.matrices.into.dataframe(t, y, error_frac, weights_norm, filecodes)
    
  # RECONSTRUCTION PARAMETERS ---------------------------------------------------------------- 
    
    param[['imputation']][['methods']] = c('imputeTS_kalman_StructTS')
    param[['denoising']][['methods']] = c('wmtsa_s8')
    param[['decomposition']][['methods']] = c('EMD')
    param[['joint']][['methods']] = c('1')
    
    # SETTINGS
    param[['imputation']][['fps']] = 30
    param[['imputation']][['debug_series']] = 1
    param[['denoising']][['debug_subject_to_plot']] = 'PLR4229'
    param[['decomposition']][['EMD_trials']] = 100
    
  # IMPUTATION ---------------------------------------------------------------- 
  
    imputed = data.imputation.wrapper(pupil_df, t, y, error_frac, weights_norm, filecodes, param[['imputation']], master_data, found_from_master) 
      
  # DENOISING ---------------------------------------------------------------- 
  
    denoised = data.denoising.wrapper(pupil_df, imputed, t, y, error_frac, weights_norm, filecodes, param[['denoising']], master_data, found_from_master)
  
  # DECOMPOSITION ---------------------------------------------------------------- 

    decomposed = data.decomposition.wrapper(pupil_df, denoised, t, y, error_frac, weights_norm, filecodes, param[['decomposition']], master_data, found_from_master)
  
  # JOINT ---------------------------------------------------------------- 
    
    joint = data.joint.modeling.wrapper(pupil_df, t, y, error_frac, weights_norm, filecodes, param[['joint']], master_data, found_from_master)
    
  # PACK Pupil Vectors to output ---------------------------------------------------------------- 
    
    # TODO! Make adaptive if you change the methods above!
    output = list()
    
    # IMPUTATION
    imputed_name = param[['imputation']][['methods']][[1]]
    output[[imputed_name]] = imputed[[imputed_name]]$pupil
    output[[paste(imputed_name, 'error', sep='_')]] = imputed[[imputed_name]]$error
    
    # DENOISING
    
    # DECOMPOSITION
    
    # JOINT MODELING
      
}


