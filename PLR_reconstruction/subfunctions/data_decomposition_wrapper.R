data.decomposition.wrapper = function(pupil_df, input_data, t, y, error_frac, weights_norm, filecode, param_decomp, master_data, found_from_master) {
  
  # BACKGROUND and Packages -------------------------------------------------------------
  
    # EMD
    # https://cran.r-project.org/web/packages/EMD/index.html
  
    # https://www.r-bloggers.com/wheres-the-magic-emd-and-ssa-in-r/
  
    # Example
    # http://dasan.sejong.ac.kr/~dhkim/software_emd.html
  
    # An R interface for libeemd C library for ensemble empirical mode decomposition (EEMD) 
    # and its complete variant (CEEMDAN). 
    # https://github.com/helske/Rlibeemd
  
  # INPUT CHECK -------------------------------------------------------------
  
    # param_decomp = param[['decomposition']]
    
    # number here refers to differently processed time series objects
    no_of_denoised_in = length(input_data)
    names_in = names(input_data)
    
    # samples_in = dim(input_data[[1]]$pupil)[1]
    # observations_in = dim(input_data[[1]]$pupil)[2]
    samples_in = length(input_data[[names_in[1]]]$pupil)
    observations_in = 1
   
  # DECOMPOSITION -----------------------------------------------------------------------
    
    no_of_different_decomposition_methods = length(param_decomp[['methods']])
    decomposition_list = list()
    
    for (in_data in 1 : no_of_denoised_in) {
      
      for (i in 1 : no_of_different_decomposition_methods) {
        
        method_name = param_decomp[['methods']][i]
        var_name_to_save = paste(method_name, names_in[in_data], sep='=')
        
        cat(var_name_to_save, '\n')
        if (grepl('EMD', method_name)) {
          
          # decomposition_list[[var_name_to_save]] = decomp.EMD.wrapper(t, input_data[[names_in[in_data]]], 
          #                                                            filecodes, method_name, param_decomp, debug = FALSE)
          
          decomposition_list[[var_name_to_save]] = decomp.EMD.per.subject(t, input_data[[names_in[in_data]]]$pupil, filecode, method_name, param_decomp, debug)
          
        } else {
          warning('You have typo for denoising method? "', param_den[['methods']][i], '" not implemented yet')
        }  
      }
    }
    
    return(decomposition_list)
     
}
