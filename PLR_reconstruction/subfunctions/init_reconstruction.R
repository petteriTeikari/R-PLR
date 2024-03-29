# INIT SCRIPT (i.e. HIDE THE BORING STUFF)
init.reconstruction = function(script.dir, data_path, source_path, IO_path, from_where_call = NA) {
  
  # Plotting libraries
  if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  if (!require("grid")) install.packages("grid"); library("grid")
  if (!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")
  if (!require("reshape2")) install.packages("reshape2"); library("reshape2")
  
  # https://stackoverflow.com/questions/7049272/importing-excel-files-into-r-xlsx-or-xls
  # install.packages("readxl") # CRAN version
  if (!require("readxl")) install.packages("readxl"); library("readxl")
  
  # IMPUTATION
  # install.packages("Amelia")
  # library(Amelia)
  if (!require("imputeTS")) install.packages("imputeTS"); library("imputeTS")
  # install.packages("imputeTS")
  # library(imputeTS)
  
  # DENOISING
  # install.packages("TSrepr")
  # library(TSrepr) 
  # install.packages("pdSpecEst")
  # library(pdSpecEst)
  # install.packages("tvd")
  # library(tvd)
  # install.packages("robustgam")
  # library(robustgam)
  # install.packages("wmtsa")
  
  # install.packages("ifultools")
  if (!require("wmtsa")) install.packages("wmtsa"); library("wmtsa")
  if (!require("ifultools")) install.packages("ifultools"); library("ifultools")
  if (!require("Rwave")) install.packages("Rwave"); library("Rwave")

  
  # IMPUTATION
  if (!require("missForest")) install.packages("missForest"); library("missForest")
  if (!require("doParallel")) install.packages("doParallel"); library("doParallel")
  
  
  # DECOMPOSITION
  if (!require("EMD")) install.packages("EMD"); library("EMD")
  if (!require("hht")) install.packages("hht"); library("hht")
  
  # SOURCE SUBFUNCTIONS
  source(file.path(source_path, 'single_file_reconstruction.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'get_timing_stats_of_inputs.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'resample_reconstructed_PLR.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'define_whenLightWasOn.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'compute_PLR_derivatives.R', fsep = .Platform$file.sep))
  
  source(file.path(source_path, 'create_PLR_dataset_matrix.R', fsep = .Platform$file.sep))
  
  source(file.path(IO_path, 'import_binDefinitions.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'define_baseline_points.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'normalize_PLR.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'normalize_low_level.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'normalize_PLR_reduced.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'get_baseline_period_from_bins.R', fsep = .Platform$file.sep))
  
  source(file.path(IO_path, 'get_codes_from_filenames.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'trim_master_data_with_subject_codes.R', fsep = .Platform$file.sep))
  
  source(file.path(IO_path, 'read_theMasterExcel.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'clean_excelSheet.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'combine_excelDataFramesToOne.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'add_customVariablesToDataframe.R', fsep = .Platform$file.sep))
  
  source(file.path(source_path, 'dataset_modeling_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'plot_functions.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'compute_power_spectrum.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'compute_timeFreq.R', fsep = .Platform$file.sep))
  
  source(file.path(source_path, 'helper_functions.R', fsep = .Platform$file.sep))
  
  source(file.path(source_path, 'analyze_and_reimpute.R', fsep = .Platform$file.sep))
  
  source(file.path(source_path, 'helper_functions.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'data_imputation_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'lowLevel_imputation_wrappers.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'data_denoising_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'lowLevel_denoising_wrappers.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'data_decomposition_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'file_decomposition_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'lowLevel_decomposition_wrappers.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'data_joint_modeling_wrapper.R', fsep = .Platform$file.sep))
  
  cat('Calling "init_reconstruction.R from ', from_where_call, ' (if NA, a "generic call")')
  create_dirs = TRUE
  
  if (identical(from_where_call, 'Resampling')) {
    data_path = file.path(data_path, '..', fsep = .Platform$file.sep)
    cat('\nChanging base path for data outputs to:', data_path, '\n\n')
  }
  
  if (identical(from_where_call, 'analyzReimpute')) {
    data_path = file.path(data_path, '..', fsep = .Platform$file.sep)
    cat('\nChanging base path for data outputs to:', data_path, '\n\n')
    create_dirs = FALSE
  }
  
  data_path_out = file.path(data_path, '..', 'recon', fsep = .Platform$file.sep)
  data_resampled_path_out = file.path(data_path, '..', 'recon_resampled', fsep = .Platform$file.sep)
  data_trimmed_path_out = file.path(data_path, '..', 'recon_trimmed', fsep = .Platform$file.sep)
  data_temp_path_out = file.path(data_path, '..', 'recon_EMD', fsep = .Platform$file.sep)
  
  if (create_dirs) {
  
    # QUick fix as the folder were needlessly created all over again
    
    # Debugging the "SERI syntax"
    if (dir.exists(data_path) == FALSE) {
      cat(' .. Creating the directory for DATA PATH:\n')
      cat(' .. .. ', data_path, '\n')
      dir.create(data_path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
    if (dir.exists(data_path_out) == FALSE) {
      cat(' .. Creating the directory for DATA Recon output:\n')
      cat(' .. .. ', data_path_out, '\n')
      dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
  
    if (dir.exists(data_resampled_path_out) == FALSE) {
      cat(' .. Creating the directory for DATA Imputed output:\n')
      cat(' .. .. ', data_resampled_path_out, '\n')
      dir.create(data_resampled_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
  
    
    if (dir.exists(data_trimmed_path_out) == FALSE) {
      cat(' .. Creating the directory for DATA Trimmed output:\n')
      cat(' .. .. ', data_trimmed_path_out, '\n')
      dir.create(data_trimmed_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
  
    if (dir.exists(data_temp_path_out) == FALSE) {
      cat(' .. Creating the directory for DATA Recon EMD output:\n')
      cat(' .. .. ', data_temp_path_out, '\n')
      dir.create(data_temp_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    }
    
  }
  
  return(list(data_path_out, data_resampled_path_out, data_trimmed_path_out, data_temp_path_out))
  
}