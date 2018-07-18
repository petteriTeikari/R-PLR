batch.PLR.analyze.reconstructions = function(data_path =  '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/FinalOUT', 
                                             data_path_out = NA,
                                             RPLR_analysis_path = NA,
                                             parameters, RPLR_paths, masterExcel,
                                             process_only_unprocessed = FALSE,
                                             path_check_for_done, 
                                             no_of_cores_to_use = detectCores()-1,
                                             pupil_col = 'pupil') {

  # Initialize --------------------------------------------------------------
  
    # Libraries needed
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(doParallel)
  
    # install.packages("pracma")
    library(pracma) # for AUC, numerical derivatives
    library(segmented) # broken stick
    library(metafor) # for regression with uncertainty
    library(zoo) # for interpolating the NAs away
  
    # install.packages("nonlinearTseries")
    library(nonlinearTseries) # for DFA
  
    # install.packages("MFDFA")
    library(MFDFA)
  
    library(EMD)
    library(hht)
    
    # Define Paths
    if (is.na(RPLR_analysis_path)) {
      script.dir <- dirname(sys.frame(1)$ofile)
      # data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
    } else {
      script.dir = RPLR_analysis_path
    }
  
    source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
    source_path_recon = file.path(script.dir, '..', 'PLR_reconstruction', 'subfunctions', fsep = .Platform$file.sep)
    IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
    config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)

    # define the output for results ('..', goes one folder back)
    if (dir.exists(data_path_out) == FALSE) {
      cat('Creating the directory for DATA output')
      dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
    # define the output for results ('..', goes one folder back)
    data_fractal_out = file.path(data_path, '..', 'fractalAnalysis', fsep = .Platform$file.sep)
    if (dir.exists(data_fractal_out) == FALSE) {
      cat('Creating the directory for DATA output')
      dir.create(data_fractal_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
    # define the output for results ('..', goes one folder back)
    data_timefreq_out = file.path(data_path, '..', 'timeFrequency', fsep = .Platform$file.sep)
    if (dir.exists(data_timefreq_out) == FALSE) {
      cat('Creating the directory for DATA output')
      dir.create(data_timefreq_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
    data_images_path_out = file.path(data_path, '..', 'feats_out', fsep = .Platform$file.sep)
    if (dir.exists(data_images_path_out) == FALSE) {
      cat('Creating the directory for FEATURES IMAGE output')
      dir.create(data_images_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
  
    # SOURCE SUBFUNCTIONS, same as importing in Python
    source(file.path(source_path, 'define_filesToProcessForAnalysis.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'process_single_PLR_for_analysis.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'import_binDefinitions.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'get_baseline_period_from_bins.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'import_pupildata.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'import_SERI_pupillometer.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'define_whenLightWasOn.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'compute_PLR_features.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'plot_PLRwithFeatures.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'get_datapoints_per_bin.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'compute_indiv_feature.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'feature_noise_model.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'feature_slope_wrapper.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'export_PLRwithFeatures.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'export_PLRfeats_asHDF5.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'export_PLRfeats_asCSV.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'normalize_PLR_reduced.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'normalize_low_level.R', fsep = .Platform$file.sep))
    
    source(file.path(IO_path, 'define_baseline_points.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'check_for_done_filecodes.R', fsep = .Platform$file.sep))
    
    source(file.path(source_path, 'file_fractal_wrapper.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'fractal_monofractal_wrapper.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'fractal_multifractal_wrapper.R', fsep = .Platform$file.sep))
    
    source(file.path(source_path_recon, 'post_process_decomposition_IMFs.R', fsep = .Platform$file.sep))
    source(file.path(source_path_recon, 'init_reconstruction.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'file_timeFreq_wrapper.R', fsep = .Platform$file.sep))
  
    # PARAMETERS
    normalize_on = TRUE
    normalize_method = 'hybrid' # 'hybrid' ['divisive' or 'subtractive, see http://doi.org/10.3758/s13428-017-1007-2]
    normalize_indiv_colors = FALSE # when true, normalize both colors on their pre-"light onsets"
    
    process_only_one_file = FALSE # If FALSE, doing batch processing
    pattern_to_find = "*.csv"
    
  # List the names from the folder that you want to process ----------------------------------------
    
    # TODO! Make possible to give all these variables from outside as well
    # so you could process the outliers, analyze the hand-crafted features, 
    # and the statistics on one button push
    files_to_process = define.filesToProcessForAnalysis(data_path, pattern_to_find, 
                                                        process_only_one_file, name_to_find)
      
    process_only_unprocessed = TRUE
    if (process_only_unprocessed) {
      indices_undone = check.for.done.filecodes(files_to_process, path_check_for_done)
      files_to_process = files_to_process[indices_undone]
    }
    
  # PROCESS ALL FILE(S) OF FOLDER ---------------------------------------------
  
  
    features = mclapply(files_to_process, function(files_to_process){
      process.single.PLR.for.analysis(files_to_process, data_path_out, data_norm_path_out, data_images_path_out, 
                                      data_fractal_out, data_timefreq_out, config_path, 
                                      normalize_on, normalize_method, normalize_indiv_colors)
    },  mc.cores = no_of_cores_to_use) 
    # ~4h40min at home AMD (1-core)
    
}