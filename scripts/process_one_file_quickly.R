process.one.file.quickly = function() {
  

  # BORING INIT STUFF GOES HERE ---------------------------------------------

    # Define computer-specific paths
    # TODO! Make more adaptive
    paths = list()
    
    # TODO! Read this location automagically
    # Now this ghetto fix for small number of users
    if (identical(toString(Sys.info()["nodename"]), "RAY-XPS" ) &&
        identical(toString(Sys.info()["login"]), "Ray P. Najjar")) {
      
        paths[['RPLR']][['base']] = 'C:/Users/Ray-Najjar/Desktop/GitPLR/R-PLR'
             
     } else if (identical(toString(Sys.info()["nodename"]), "petteri-Server")) {
       
        paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
        
     } else {
       
       warning('No path defined for: ', Sys.info())
     }
    
    paths[['data_in']][['base']] = file.path(paths[['RPLR']][['base']], 'test_data', fsep = .Platform$file.sep)
    paths[['data_out']][['base']] = paths[['data_in']][['base']]
    
    # Init scripts
    source(file.path(paths[['RPLR']][['base']], 'PLR_artifacts', 'subfunctions', 'spline_filter.R', fsep = .Platform$file.sep))
    source(file.path(paths[['RPLR']][['base']], 'scripts', 'process_one_file_quickly.R', fsep = .Platform$file.sep))
    source(file.path(paths[['RPLR']][['base']], 'PLR_IO', 'normalize_low_level.R'))
    source(file.path(paths[['RPLR']][['base']], 'PLR_IO', 'compute_PLR_derivatives.R'))
    source(file.path(paths[['RPLR']][['base']], 'plot', 'plot_quick_velocity_and_acceleration.R'))

    paths = init.paths.and.functions(paths)
    paths = source.subfunctions(paths)
    import.and.install.libraries(paths)
    param = init.PLR.processing.params(paths)
  

  # INPUT DATA -------------------------------------------------------

    # TODO! Make an interactive GUI to pop out
    filename = 'whiteflash_09252018_17202570.csv'
    data_in = read.csv(file.path(paths[['data_in']][['base']] , filename, fsep = .Platform$file.sep), 
                       header = FALSE)
    
    pupil = data.frame(time_raw = data_in$V2, pupil_raw = data_in$V3, light = data_in$V10)
    
  # PRE-PROCESS -------------------------------------------------------
    
    pupil$pupil_raw = quick.n.dirty.glitch.fix(pupil$pupil_raw)
    
    # no idea why we have NAs in Light now?
    pupil$light[is.na(pupil$light)] = 255
    pupil$light = pupil$light / 255
    
    # fake RED on and BLUE on
    pupil$blue_on = pupil$light
    pupil$red_on = vector(mode = 'numeric', length(pupil$blue_on))
    
    # baseline_indices
    ind2 = which(pupil$light == 1)[1] - 1
    ind1 = ind2 - 5*30 # quick'n'dirty manual timing
    pupil$time = pupil$time_raw - pupil$time_raw[ind2+1]
    
  # ACTUAL PROCESSING -------------------------------------------------------
    
    # LOESS FIT
    loess <- loess(pupil_raw ~ time, data=pupil, span=0.2)
    fit_y = predict(loess, newdata = pupil$time)
    
    # Compare difference
    sigma_threshold = 1.96
    abs_diff = abs(pupil$pupil_raw - fit_y)
    sd_detrended = sd(abs_diff, na.rm = TRUE)
    
    # Find outlier indices
    outliers = abs_diff > sigma_threshold * sd_detrended
    
    # Set the outliers to NA
    pupil$pupil_raw[outliers] = NA
    
    # Re-smooth
    # increase the smoothness here by increasing span
    loess <- loess(pupil_raw ~ time, data=pupil, span=0.05)
    fit_y = predict(loess, newdata = pupil$time)
    pupil$pupil_raw = fit_y
    
    # Normalize
    baseline = median(pupil$pupil[ind1:ind2], na.rm = TRUE)
    pupil$pupil = normalize.low.level(pupil$pupil_raw, baseline)

    # Compute the derivatives
    derivs = compute.PLR.derivatives(pupil$time, pupil$pupil)
    pupil$velocity = derivs[[1]]
    pupil$acceleration = derivs[[2]]
    
  # PLOT -------------------------------------------------------
    
    plot.acc.and.veloc(dataframe = pupil)
    
    qplot(pupil$time, pupil$pupil)
    qplot(pupil$time, pupil$velocity)
    qplot(pupil$time, pupil$acceleration)
    
    write.csv(pupil, file = "example_plot.csv")
    
}


# SUBFUNCTION

init.paths.and.functions = function(paths) {
  
  # TODO! If you actually change these, the batch_ functions atm have
  # hard-coded variable names
  # Debugging the "SERI syntax"
  
  # Create output
  if (dir.exists(paths[['data_out']][['base']]) == FALSE) {
    dir.create(paths[['data_out']][['base']], showWarnings = TRUE, recursive = FALSE, mode = "0777")
    cat('Creating the directory for DATA OUTPUT\n')
  }
  
  # Derived paths
  paths[['data_out']][['video']] = file.path(paths[['data_out']][['base']], 'VIDEO')
  paths[['data_out']][['artifacts']] = file.path(paths[['data_out']][['base']], 'outlier_free') 
  paths[['data_out']][['artifacts_corrected']] = file.path(paths[['data_out']][['artifacts']], 'outlier_free_corrected') 
  paths[['data_out']][['resampling']] = file.path(paths[['data_out']][['artifacts']], 'recon_resampled')
  paths[['data_out']][['imputed']] = file.path(paths[['data_out']][['base']], 'imputation_final')
  paths[['data_out']][['EMD']] = file.path(paths[['data_out']][['base']], 'recon_EMD') 
  paths[['data_out']][['EMD_fusion']] = file.path(paths[['data_out']][['EMD']], 'IMF_fusion') 
  paths[['data_out']][['EMD_recomposed']] = file.path(paths[['data_out']][['base']], 'recon_EMD_subcomp_fusion') 
  paths[['data_out']][['reconstruction']] = file.path(paths[['data_out']][['base']], 'recon')
  paths[['data_out']][['reconstructed']] = file.path(paths[['data_out']][['base']], 'reconstructed') 
  paths[['data_out']][['FinalOUT']] = file.path(paths[['data_out']][['base']], 'FinalOUT') 
  paths[['data_out']][['features']] = file.path(paths[['data_out']][['base']], 'PLR_feat') 
  
  # Create outlier_free
  if (dir.exists(paths[['data_out']][['artifacts']]) == FALSE) {
    dir.create(paths[['data_out']][['artifacts']], showWarnings = TRUE, recursive = FALSE, mode = "0777")
    cat('Creating the directory for DATA OUTLIER_FREE\n')
  }
  
  # inputs from these outputs
  paths[['data_in']][['video']] = paths[['data_in']][['base']]
  paths[['data_in']][['artifacts']] = paths[['data_out']][['video']]
  paths[['data_in']][['resampling']] = paths[['data_out']][['artifacts']]
  paths[['data_in']][['resampling_corr']] = paths[['data_out']][['artifacts_corrected']]
  paths[['data_in']][['imputation']] = paths[['data_out']][['resampling']]
  paths[['data_in']][['imputation_rerun']] = file.path(paths[['data_out']][['base']], 
                                                       'recon_imputation_correction', fsep = .Platform$file.sep)
  paths[['data_in']][['EMD']] = paths[['data_out']][['imputed']]
  paths[['data_in']][['features']] = paths[['data_out']][['FinalOUT']]
  
  # Excel Master data sheet
  paths[['data_in']][['masterSheet']] = file.path(paths[['data_out']][['base']], '..', fsep = .Platform$file.sep)
  paths[['data_in']][['masterFile']] = 'Master_File_De-Identified_Copy_For_Petteri.xlsx' 
  paths[['data_in']][['excelMasterPath']] = file.path(paths[['data_in']][['masterSheet']], 
                                                 paths[['data_in']][['masterFile']], fsep = .Platform$file.sep)
  
  # TODO! Actually you want to read from the manually reconstructed ones
  paths[['data_in']][['reconstruction']] = paths[['data_out']][['artifacts']]
  # paths[['data_in']][['reconstruction']] = paths[['data_out']][['artifacts_corrected']]
                                      
  # define the package paths
  paths[['video']] = file.path(paths[['RPLR']][['base']], 'PLR_video', fsep = .Platform$file.sep)
  paths[['artifacts']] = file.path(paths[['RPLR']][['base']], 'PLR_artifacts', fsep = .Platform$file.sep)
  paths[['recon']] =  file.path(paths[['RPLR']][['base']], 'PLR_reconstruction', fsep = .Platform$file.sep)
  paths[['analysis']] =  file.path(paths[['RPLR']][['base']], 'PLR_analysis', fsep = .Platform$file.sep)
  
  # R paths
  paths[['RPLR']][['IO']] = file.path(paths[['RPLR']][['base']], 'PLR_IO', fsep = .Platform$file.sep)
  paths[['RPLR']][['scripts']] = file.path(paths[['RPLR']][['base']], 'scripts', fsep = .Platform$file.sep)
  
  # Source the main processing subfunctions
  source(file.path(paths[['video']], 'batch_PLR_videos.R', fsep = .Platform$file.sep))
  source(file.path(paths[['artifacts']], 'batch_PLR_artifacts.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'subfunctions', 'init_reconstruction.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_PLR_resample.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_AnalyzeAndReImpute.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_EMD_decomposition.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_recompose_EMD_subdecompositions.R', fsep = .Platform$file.sep))
  source(file.path(paths[['RPLR']][['scripts']], 'combine_data_from_multiple_folders.R', fsep = .Platform$file.sep))
  
  source(file.path(paths[['RPLR']][['IO']], 'compute_PLR_derivatives.R', fsep = .Platform$file.sep))
  
  source(file.path(paths[['recon']], 'batch_data_decompose_for_augmentation.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'subfunctions', 'file_decomp_augmentation_wrapper.R', fsep = .Platform$file.sep))
  
  source(file.path(paths[['analysis']], 'batch_PLR_analyze_reconstructions.R', fsep = .Platform$file.sep))
  
  return(paths)
  
}

import.and.install.libraries = function(paths) {
  
  # TODO!
  # Put ALL THE LIBRARIES needed here
  # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
  
  cat('Checking the LIBRARIES\n')
  
  if (!require("data.table")) install.packages("data.table"); library("data.table")
  
  # VIDEO
  if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  
  # ARTIFACTS
  # if (!require("forecast")) install.packages("forecast"); library("forecast")
  if (!require("changepoint")) install.packages("changepoint"); library("changepoint")
  
  # RECONSTRUCTION
  subfunction_path = file.path(paths[['recon']], 'subfunctions', fsep = .Platform$file.sep)
  source(file.path(subfunction_path, 'init_reconstruction.R', fsep = .Platform$file.sep))
  paths_for_output = init.reconstruction(script.dir = paths[['recon']], 
                                         data_path = paths[['data_in']][['resampling_corr']], 
                                         source_path = subfunction_path, 
                                         IO_path = paths[['RPLR']][['IO']]) 
  
  
  # FRACTAL ANALYSIS
  if (!require("MFDFA")) install.packages("MFDFA"); library("MFDFA")
  if (!require("fractal")) install.packages("fractal"); library("fractal")
  if (!require("nonlinearTseries")) install.packages("nonlinearTseries"); library("nonlinearTseries")
    # IF configure: error: missing required header GL/gl.h
    # ERROR: configuration failed for package ‘rgl’
    # THEN: sudo apt-get install libglu1-mesa-dev
    # For fresh installation, you might need these
    # sudo apt-get install libglu1-mesa-dev libcurl4 libfreetype6-dev libcurl4-openssl-dev
    # For Apple / Mac, you need XQuartz unstalled
  
  # TIME-FREQ
  
  
  # SHINY APPs
  if (!require("Cairo")) install.packages("Cairo"); library("Cairo")
  # https://gykovacsblog.wordpress.com/2017/05/15/installing-cairo-for-r-on-ubuntu-17-04/
  # sudo apt-get install libcairo2-dev libgtk2.0-dev xvfb xauth xfonts-base libxt-dev
  
}

source.subfunctions = function(paths) {
  
  source(file.path(paths[['RPLR']][['IO']], 'check_for_done_filecodes.R', fsep = .Platform$file.sep))
  return(paths)
  
}

init.PLR.processing.params = function(paths) {
  
  param = list()
  param[['video']][['dummy']] = NA
  param[['artifacts']][['dummy']] = NA
  param[['recon']][['dummy']] = NA
  param[['decomp_augm']][['dummy']] = NA
  
  return(param)
  
}

