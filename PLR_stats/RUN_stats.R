# Initialize --------------------------------------------------------------

  # Define the location of this file 
  if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
  full_path = rstudioapi::getActiveDocumentContext()$path
  script.dir = strsplit(full_path, split = .Platform$file.sep, fixed=TRUE)[[1]]
  if (identical(.Platform$OS.type, 'windows')) {
    # script.dir = strsplit(full_path_script, split = '\\', fixed=TRUE)[[1]]
    # just to make sure that this is correctly split
  }
  
  just_the_file = tail(script.dir,1)
  cat('   --- just_the_file = ', just_the_file, '\n')
  cat('   --- --- full_path_script = ', full_path, '\n\n')
  script.dir = gsub(just_the_file, '', full_path)
  
  # remove the last separator
  if (substr(script.dir, nchar(script.dir), nchar(script.dir)) == '/') {
    script.dir = substr(script.dir, 1, nchar(script.dir)-1)
  } else if (substr(script.dir, nchar(script.dir), nchar(script.dir)) == '/') {
    script.dir = substr(script.dir, 1, nchar(script.dir)-1)
  }
  
  # Read in the default in and out folders from a config text file:
  config_paths = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  config_paths_file = 'paths.csv'
  config_file_path = file.path(config_paths, config_paths_file, fsep = .Platform$file.sep)
  
  cat(paste0('Reading default DATA IN/OUT paths from "', config_paths_file, '", located in: \n\t\t',
             config_paths))
  
  if (file.exists(config_file_path)) {
    paths_cfg = read.csv(config_file_path, header = FALSE, stringsAsFactors = FALSE)
    win_indices = paths_cfg$V2 == 'windows'
  } else {
    warning('Problem in finding the config file for the DATA paths, was trying to open it from:\n\t\t',
            config_paths, '\n\t\t', 
            'file name = ', config_paths_file)
  }
  
  if (identical(.Platform$OS.type, 'windows')) {
    paths_win = paths_cfg$V3[win_indices]
    PLR_folder = paths_win[2]
  } else {
    paths_unix = paths_cfg$V3[!win_indices]
    PLR_folder= paths_unix[2]
    # PLR_folder = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT'
  }
  
  # TODO! make non-hardcoded, relative maybe
  masterXLS_data_path = file.path(PLR_folder, '..')
  
  # Libraries needed
  if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  if (!require("grid")) install.packages("grid"); library("grid")
  if (!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")
  library(reshape2)
  if (!require("doParallel")) install.packages("doParallel"); library("doParallel")

  # data wrangling
  if (!require("reshape2")) install.packages("reshape2"); library("reshape2")
  if (!require("tidyr")) install.packages("tidyr"); library("tidyr")
  if (!require("dplyr")) install.packages("dplyr"); library("dplyr")
  if (!require("plyr")) install.packages("plyr"); library("plyr")
  if (!require("pracma")) install.packages("pracma"); library("pracma")
  if (!require("car")) install.packages("car"); library("car") # for Levene's test e.g.
  # You need system-level installation of linear algebra packages
  # sudo apt install liblapack-dev liblapack3 libopenblas-base libopenblas-dev r-cran-car

  # https://stackoverflow.com/questions/7049272/importing-excel-files-into-r-xlsx-or-xls
  # install.packages("readxl") # CRAN version
  if (!require("readxl")) install.packages("readxl"); library("readxl") # for Levene's test e.g.

  # install.packages("MatchIt")
  # install.packages("optmatch")
  #library(MatchIt) # for age-matching
  #library(optmatch) # for age-matching

  # for converting list into a data frame
  # https://www.rdocumentation.org/packages/qdapTools/versions/1.3.3/topics/list2df
  # if (!require(qdap)) install.packages("qdap")

  # data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  stat_path = file.path(script.dir, 'stat_subfunctions', fsep = .Platform$file.sep)
  
  # Overwrite the initialized data folder, if wanted
  # the use of ".Platform$file.sep" is better for Windows vs. Linux/Max
  # compatibility as the separator is different "\" vs. "/"
  data_path_feats = file.path(PLR_folder, 'PLR_feat', fsep = .Platform$file.sep)
  data_path_traces = file.path(PLR_folder, 'reconstructed', fsep = .Platform$file.sep)
  
  # define the output for results ('..', goes one folder back)
  data_path_out = file.path(data_path_feats, '..', 'PLR_stats', fsep = .Platform$file.sep)
  if (dir.exists(data_path_out) == FALSE) {
    cat('Creating the directory for STATS output')
    dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  # define the output for results ('..', goes one folder back)
  data_deeplearning_path_out = file.path(data_path_feats, '..', 'for_deepLearning', fsep = .Platform$file.sep)
  if (dir.exists(data_path_out) == FALSE) {
    cat('Creating the directory for STATS output')
    dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  # If you want to test this, if you get stuck somewhere, so have intermediate results
  debug_path = file.path(PLR_folder, '..', 'DATA_SERI_DebugDevel')
  if (dir.exists(debug_path) == FALSE) {
    cat('Creating the directory for DEBUG output')
    dir.create(debug_path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  # SOURCE
  source(file.path(script.dir, 'STAT_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'import_computedFeats.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'import_feat_file.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'import_resampledReconstructions.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'list_theFeatFiles.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'read_theMasterExcel.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'combine_excelDataFramesToOne.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'clean_excelSheet.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'combine_featsWithExcelMaster.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'add_customVariablesToDataframe.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
  
  # PARAMETERS
  dataset_type = 'blue_red' # "blue_red" when there are these conditions
                            # if only blue features would be computed, you would add
                            # some code for the import.computedFeats() to handle this
  
  # Parameters
  parameters = list()
  
  # Settings
  settings = list()
  settings[['script_dir']] = script.dir
  settings[['stat_path']] = stat_path
  settings[['data_path_out']] = data_path_out
  settings[['data_deep_path_out']] = data_deeplearning_path_out
  settings[['no_of_cores_to_use']] = detectCores() # test if works outside linux
  
# Import the files --------------------------------------------------------------
  
  XLS_filename = 'Master_File_De-Identified_Copy_For_Petteri.xlsx' 
  pattern_to_find = '*.csv'
  feats_out = import.computedFeats(data_path_feats, pattern_to_find, dataset_type, 
                                          masterXLS_data_path, XLS_filename)
  
    data_frame_feats = feats_out[[1]] # e.g. 505 obs of 153 variables
    derived_feats_names = feats_out[[2]]

  pattern_to_find = '*_reconstruction.csv'
  
  # don't extract all the column names to constrain a bit the memory use, and 
  # make things faster, also TODO! that all the traces have the same column names
  included_vars = c('time', 'time_onsetZero', 'time_maxDeriv_zero', 'pupil', 'error', 'error_fractional',
                    'pupil_raw', 'pupil_blink_thresholded', 'pupil_outlierfree', 'pupil_outlier_corrected', 'missForest', 
                    'noiseNorm', 'noiseNonNorm', 'hiFreq', 'loFreq', 'base', 'denoised',
                    'oscillations', 'oscillations_denoised', 
                    'oscillations_hiFreq', 'base_osc', 'baseline')
  
  out_import = import.resampledReconstructions(data_path_traces, pattern_to_find, 
                                               dataset_type, included_vars, settings)
  
    list_traces = out_import[[1]]
    subject_codes_traces = out_import[[2]]
    # e.g. 287 obs with each 1980 datapoints, and 32 variables
    
    # Export the variables used at the moment to disk
    # DEBUG POINT #1
    save.image(file = file.path(debug_path, 'debug_point1.Rdata')) # 153.5 MB
    
    
    # Save to disk
    filename_path = 'ALL_combined_features.csv'
    export.pupil.dataframe.toDisk(data_frame_feats, filename_path, data_path_out, 'feat_stats')
    
    # The Traces
    # traces_out = file.path(data_path_out, 'PLR_traces_out.RData', fsep = .Platform$file.sep)
    # save(list_traces, file=traces_out) # TODO! something fancier maybe? Harder with 2D though
    
# PROCESS --------------------------------------------------------------
  
  # NOTE1 If any of the above failed, you can read the "correct results" from Rdata
  # http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata
  # UNCOMMENT THE FOLLOWING LINE and hit Ctrl+Enter
  # load(file = file.path(debug_path, 'debug_point1.Rdata'))
    
  # NOTE2 The Rdata does not contain correct path definitions (as they depend on your machine ultimately),
  # nor the correct packages (as if you need them installed on your own machine)
    
  # Gather everything here that you might wanto to do for the data
  STAT.wrapper(data_frame_feats, list_traces, subject_codes_traces,
               dataset_type, derived_feats_names, 
               parameters, settings)
  
  