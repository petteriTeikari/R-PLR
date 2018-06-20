# Initialize --------------------------------------------------------------

  # Libraries needed
  library(ggplot2)
  library(grid)
  library(gridExtra)

  # install.packages("pracma")
  library(pracma) # for AUC, numerical derivatives
  library(segmented) # broken stick
  library(metafor) # for regression with uncertainty
  library(zoo) # for interpolating the NAs away
  
  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(IO_path, 'config', fsep = .Platform$file.sep)
  
  # Overwrite the initialized data folder, if wanted
  # the use of ".Platform$file.sep" is better for Windows vs. Linux/Max
  # compatibility as the separator is different "\" vs. "/"
    data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/recon'
    # TODO! make non-hardcoded, relative maybe
    # data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/recon/debug'
    
  # define the output for results ('..', goes one folder back)
  data_path_out = file.path(data_path, '..', 'PLR_feat', fsep = .Platform$file.sep)
  if (dir.exists(data_path_out) == FALSE) {
    cat('Creating the directory for DATA output')
    dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  data_images_path_out = file.path(data_path_out, 'images', fsep = .Platform$file.sep)
  if (dir.exists(data_images_path_out) == FALSE) {
    cat('Creating the directory for FEATURES IMAGE output')
    dir.create(data_images_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  data_norm_path_out = file.path(data_path, '..', 'normalized', fsep = .Platform$file.sep)
  if (dir.exists(data_norm_path_out) == FALSE) {
    cat('Creating the directory for NORMALIZED DATA output')
    dir.create(data_norm_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  # SOURCE SUBFUNCTIONS, same as importing in Python
  source(file.path(source_path, 'define_filesToProcessForAnalysis.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'process_single_PLR_for_analysis.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'import_binDefinitions.R', fsep = .Platform$file.sep))
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
  source(file.path(source_path, 'normalize_PLR.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'normalize_low_level.R', fsep = .Platform$file.sep))
  
  source(file.path(source_path, 'define_baseline_points.R', fsep = .Platform$file.sep))
  
  # Import bin definitions for hand-crafted features from CSV-file
  bins = import.binDefinitions(config_path)
  
  # PARAMETERS
  normalize_on = TRUE
  normalize_method = 'hybrid' # 'hybrid' ['divisive' or 'subtractive, see http://doi.org/10.3758/s13428-017-1007-2]
  normalize_indiv_colors = FALSE # when true, normalize both colors on their pre-"light onsets"
  
  process_only_one_file = FALSE # If FALSE, doing batch processing
  pattern_to_find = "*_recon.csv"
  name_to_find = "PLR4070_BR_recon.csv" # purpose of this line? [PT]
  
# List the names from the folder that you want to process ----------------------------------------
  
  # TODO! Make possible to give all these variables from outside as well
  # so you could process the outliers, analyze the hand-crafted features, 
  # and the statistics on one button push
  files_to_process = define.filesToProcessForAnalysis(data_path, pattern_to_find, 
                                                      process_only_one_file, name_to_find)
    
# PROCESS ALL FILE(S) OF FOLDER ---------------------------------------------

  # The real advantage of this "loopless approach" is that you can easily run 
  # it in parallel using mclapply (from multicore package) 
  # instead of lapply. Or parLapply from snow
  features = lapply(files_to_process, function(files_to_process){
    process.single.PLR.for.analysis(files_to_process, data_path_out, data_norm_path_out, data_images_path_out, 
                                    bins, normalize_on, normalize_method, normalize_indiv_colors)
  })
  
  