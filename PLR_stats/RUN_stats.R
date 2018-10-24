# Initialize --------------------------------------------------------------

  # Libraries needed
  library(ggplot2) # for plotting
  library(grid)
  library(gridExtra)
  library(reshape2)

  library(doParallel)

  # data wrangling
  library(reshape2)
  library(tidyr)
  library(dplyr)
  library(plyr)

  if (!require("pracma")) install.packages("pracma"); library("pracma")

  # install.packages("car")
  if (!require("car")) install.packages("car"); library("car")
  # library(car) # for Levene's test e.g.


  # install.packages("MAMSE")
  # library(MAMSE) # for ROC
  # install.packages("pROC")
  # library(pROC) # for ROC

  # https://stackoverflow.com/questions/7049272/importing-excel-files-into-r-xlsx-or-xls
  # install.packages("readxl") # CRAN version
  library(readxl)

  # install.packages("MatchIt")
  # install.packages("optmatch")
  #library(MatchIt) # for age-matching
  #library(optmatch) # for age-matching

  # for converting list into a data frame
  # https://www.rdocumentation.org/packages/qdapTools/versions/1.3.3/topics/list2df
  # if (!require(qdap)) install.packages("qdap")

  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile) # https://stackoverflow.com/a/15373917
  # data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  stat_path = file.path(script.dir, 'stat_subfunctions', fsep = .Platform$file.sep)
  
  # Overwrite the initialized data folder, if wanted
  # the use of ".Platform$file.sep" is better for Windows vs. Linux/Max
  # compatibility as the separator is different "\" vs. "/"
  PLR_folder = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT'
  data_path_feats = file.path(PLR_folder, 'PLR_feat', fsep = .Platform$file.sep)
  data_path_traces = file.path(PLR_folder, 'reconstructed', fsep = .Platform$file.sep)
  
  # TODO! make non-hardcoded, relative maybe
  masterXLS_data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder'
  
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
  settings[['no_of_cores_to_use']] = detectCores() 
  
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
    
    # Export the files to disk as well
    
    # Save to disk
    filename_path = 'ALL_combined_features.csv'
    export.pupil.dataframe.toDisk(data_frame_feats, filename_path, data_path_out, 'feat_stats')
    
    # The Traces
    traces_out = file.path(data_path_out, 'PLR_traces_out.RData', fsep = .Platform$file.sep)
    # save(list_traces, file=traces_out) # TODO! something fancier maybe? Harder with 2D though
    
# PROCESS --------------------------------------------------------------
  
  # CONTINUE, GLOBAL NAMES!

  # Gather everything here that you might wanto to do for the data
  STAT.wrapper(data_frame_feats, list_traces, subject_codes_traces,
               dataset_type, derived_feats_names, 
               parameters, settings)
  
  