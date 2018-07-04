retrieve.data.for.visualization = function(filename_path = NA,
                                           plot_param = list()) {
  
  # TODO! pass more input arguments if you wish
  # source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/plot/retrieve_data_for_visualization.R')
  
  # INIT --------------------------------------------------------------------
  
    path_input = init.report(filename_path) # Init paths
    path_input = get.corresponding.files.from.other.folders(path_input) # from Feats, Fractal, and Time-Freq
  
  # PARAMETERS --------------------------------------------------------------------
    
    plot_param = define.plot.parameters(feat_example = path_input[[file_feats_full]])
    
  # RETRIEVE THE DATA FROM DISK  --------------------------------------------------------------------
    
    data_to_plot = retrieve.data.from.disk(path_input, plot_param)
    return(list(path_input, plot_param, data_to_plot))
       
}

# ------------
# SUBFUNCTIONS
# ------------

init.report = function(filename_path) {
  
  library(tidyr)
  path_input = list()
  path_input[['datatypes']] = c('recon') 
  
  # TODO! Hardcoded "recon" belive, you could make it point to path_input[['datatypes']][1]
  
  if (is.na(filename_path)) {
    warning('You need to define the input file! Using the default debug file now')
    # This is the file path for the reconstructed PLR trace
    path_input[['path_recon']]  = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/reconstructed'
    file = 'PLR1002_reconstruction.csv'
    path_input[['path_recon_filename']] = file.path(path_input[['path_recon']],  file, fsep = .Platform$file.sep)
  } else {
    path_input[['path_recon_filename']] = filename_path
  }
  
  # Splitting input fullpath to filename, path, and the subject code  
  path_input[[paste0('file_', path_input[['datatypes']][1])]] = 
    tail(strsplit(path_input[['path_recon_filename']], .Platform$file.sep)[[1]], n=1)
  path_input[['path_recon']] = 
    gsub(path_input[[paste0('file_', path_input[['datatypes']][1])]], '', path_input[['path_recon_filename']])
  path_input[['filecode']] = strsplit(path_input[[paste0('file_', path_input[['datatypes']][1])]], '_')[[1]][1]
  
  # Define the locations of other data sources
  path_input[['path_feats']] = file.path(path_input[['path_recon']], '..', 'PLR_feat', fsep = .Platform$file.sep)
  path_input[['path_feat_stats']] = file.path(path_input[['path_recon']], '..', 'PLR_stats', fsep = .Platform$file.sep)
  path_input[['path_fractal']] = file.path(path_input[['path_recon']], '..', 'fractalAnalysis', fsep = .Platform$file.sep)
  path_input[['path_timefreq']] = file.path(path_input[['path_recon']], '..', 'timeFrequency', fsep = .Platform$file.sep)
  
  # Master data sheet
  path_input[['path_masterDataSheet']] = file.path(path_input[['path_recon']], '..', '..', fsep = .Platform$file.sep)
  path_input[['file_masterDataSheet']] = 'Master_File_De-Identified_Copy_For_Petteri.xlsx'
  
  # And the output
  path_input[['path_output']] = file.path(path_input[['path_recon']], '..', 'figures_out', fsep = .Platform$file.sep)
  
  # R-PLR
  path_input[['R-PLR']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
  
  # SOURCE
  source(file.path(path_input[['R-PLR']], 'PLR_IO', 'import_feat_file.R', fsep = .Platform$file.sep))
  source(file.path(path_input[['R-PLR']], 'PLR_IO', 'import_binDefinitions.R', fsep = .Platform$file.sep))
  source(file.path(path_input[['R-PLR']], 'PLR_stats', 'stat_subfunctions', 'data_wrangling_functions.R', fsep = .Platform$file.sep))
  
  # Config path
  path_input[['config_path']] = file.path(path_input[['R-PLR']], 'config', fsep = .Platform$file.sep)
  path_input[['bin_definitions']] = import.binDefinitions(path_input[['config_path']])
  
  # Put the fields to alphabetical order
  path_input = path_input[order(names(path_input))]
  
  cat(' Initializing data list for "', path_input[['filecode']], '"\n')
  return(path_input)
  
}

get.corresponding.files.from.other.folders = function(path_input) {
  
  # Note that the file pattern wildcards and
  # expected are fixed now! So this will fail and if you change previous blocks
  # TODO! You could define these constants in some text .cfg file?
  
  names = c('feats', 'timefreq', 'fractal')
  path_input[['datatypes']] = c(path_input[['datatypes']], names)
  
  # Feats
  i = 1
  path_input = correspondence.subroutine(path=path_input[['path_feats']], pattern='*.csv',
                                         filecode = path_input[['filecode']],
                                         string_fileonly = paste0('file_', names[i]), 
                                         string_fullpath = paste0('path_', names[i], '_filename'),
                                         path_input, 
                                         expected_n = 3)
  
  # Time-Freq
  i = i + 1
  path_input = correspondence.subroutine(path=path_input[['path_timefreq']], pattern='*.RData',
                                         filecode = path_input[['filecode']],
                                         string_fileonly = paste0('file_', names[i]), 
                                         string_fullpath = paste0('path_', names[i], '_filename'),
                                         path_input, 
                                         expected_n = 1)
  
  # Fractal
  i = i + 1
  path_input = correspondence.subroutine(path=path_input[['path_fractal']], pattern='*.RData',
                                         filecode = path_input[['filecode']],
                                         string_fileonly = paste0('file_', names[i]), 
                                         string_fullpath = paste0('path_', names[i], '_filename'),
                                         path_input, 
                                         expected_n = 1)

  
  # Put the fields to alphabetical order
  path_input = path_input[order(names(path_input))]
  
  return(path_input)

  
}

correspondence.subroutine = function(path, pattern, filecode,
                                     string_fileonly, string_fullpath,
                                     path_input, expected_n = 1) {
  
  # Get file listing
  file_listing = list.files(path=path, pattern=pattern, 
                            recursive=FALSE, full.names = TRUE)
  
  # Find the corresponding indices
  correspond_indices = which(grepl(filecode, file_listing))
  
    if (length(correspond_indices) == 0) {
      warning('No corresponding "', string_fileonly, '" files were found for reconstruction file = ', 
              path_input[['filename_only']], ' (filecode = ', filecode, ')')
    } 
  
  # Get the full paths for the corresponding files
  corresponding_files = file_listing[correspond_indices]
  
  # Get rid of the path 
  corresponding_only_files = gsub(paste0(path, .Platform$file.sep), '', corresponding_files)
  
    if (length(correspond_indices) != expected_n) {
      warning('For "', string_fileonly, '" we would have expected to find "', expected_n, '" correspondences, but we found "',
              length(correspond_indices), '" instances for following files:\n', corresponding_only_files)
    }
  
  # output to the input path list
  path_input[[string_fileonly]] = corresponding_only_files
  path_input[[string_fullpath]] = corresponding_files
  path_input[[paste0(string_fileonly, '_indices')]] = correspond_indices
  
  return(path_input)
  
}

define.plot.parameters = function(feat_example) {
  
  # TODO! check from the "feat_example" right away if it is
  # even possible to plot the features that you like, e.g.
  # check for typos in the request
  
  # plot_param = list()
  
  # Logic here the same as in "STAT_wrapper()"
  plot_param[['recon_vars']] =  c('time', 'time_onsetZero', 'time_maxDeriv_zero',
                                  'pupil', 'error', 'pupil_raw', 'pupil_blink_thresholded',
                                  'pupil_outlierfree', 'pupil_outlier_corrected',
                                  'missForest', 'noiseNorm', 'noiseNonNorm',
                                  'hiFreq', 'loFreq', 'base', 'smooth',
                                  'baseline', 'R', 'G', 'B')
  
  plot_param[['feats_to_plot']] =  c('MaxConstr', 'QuickPhasic', '6SecondPIPR', 'MFDFA_spectrum_peak_hq')
  
  return(plot_param)
  
}

retrieve.data.from.disk = function(path_input, plot_param) {

  data_out = list()
  
  # e.g. Recon, Feats, Fractal and TimeFreq  
  datatypes = path_input[['datatypes']]
  
  for (d_type in 1 : length(datatypes)) {
    
    # parse from the list to easier variable names
    datatype = datatypes[d_type]
    fullfile_path = path_input[[paste0('path_', datatype, '_filename')]]
    path = path_input[[paste0('path_', datatype)]]
    file = path_input[[paste0('file_', datatype)]]

    # We need custom import functions for all different types of data now
    if (identical(datatype, 'recon')) {
      
      data_out[[datatype]] = import.recon.data.for.plot(fullfile_path, path, file,
                                                        plot_param, path_input) 
        
    } else if (identical(datatype, 'feats')) {
      
      data_out[[datatype]] = import.feats.data.for.plot(fullfile_path, path, file,
                                                        plot_param, path_input)
      
    } else if (identical(datatype, 'fractal')) {
      
      data_out[[datatype]] = import.fractal.data.for.plot(fullfile_path, path, file,
                                                           plot_param, path_input) 
      
    } else if (identical(datatype, 'timefreq')) {
      
      data_out[[datatype]] = import.timefreq.data.for.plot(fullfile_path, path, file,
                                                            plot_param, path_input) 
      
    } else {
      warning('No custom parser defined for datatype = ', datatype)
    }
    
  }
  
  return(data_out)
  
}

import.recon.data.for.plot = function(fullfile_path, path, file,
                                      plot_param, path_input) {
  
  # TODO! Optimize all reads at some point
  # https://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes
  data_in = read.csv(fullfile_path)
 
  # get rid of unwanted columns 
  data_in = data_in[plot_param[['recon_vars']]]
  
  # rename smooth to denoised
  colnames(data_in) <- gsub(x = colnames(data_in), pattern = "smooth", replacement = "denoised")
  
  return(data_in)
  
}

import.feats.data.for.plot = function(fullfile_path, path, file,
                                      plot_param, path_input) {
  
  # Get bin names
  bin_names = path_input[['bin_definitions']][['Name']]
  
  # Get global names
  ind_global = which(grepl('_global', file))
  cat('reading in = ', fullfile_path[ind_global])
  df_global = read.csv(fullfile_path[ind_global], stringsAsFactors = FALSE)
  global_names = unique(df_global[['Name']])
  
  # What features we would like to keep
  feats_to_keep = plot_param[['feats_to_plot']]
  
  # Read in and go through the conditions (red, blue, global)
  feat_combined = go.through.feat.conditions(file, fullfile_path, plot_param,
                                             bin_names, global_names)
  
  # GO through all the features given as input in 
  # "plot_param[['feats_to_plot']]"
  list_feat_combined = go.through.features(feat_combined, feats_to_keep)
  
  # TODO! Convert to something else form again, if your front-end accept
  # easier something else
  
  return(list_feat_combined)
  
}

import.fractal.data.for.plot = function(fullfile_path, path, file,
                                        plot_param, path_input) {
  
  load(fullfile_path)
  return(mfdfa)
  
}

import.timefreq.data.for.plot = function(fullfile_path, path, file,
                                         plot_param, path_input) {
  
  load(fullfile_path)
  timefreq = list()
  timefreq[['hgram']] = hgram
  timefreq[['hspec']] = hspec
  return(timefreq)
  
}

go.through.feat.conditions = function(file, fullfile_path, plot_param,
                                      bin_names, global_names) {
  
  # How many features per subject
  no_of_conditions = length(fullfile_path)
  
  # Read in and go through the conditions (red, blue, global)
  for (c in 1 : no_of_conditions) {
    
    color = gsub('.csv', '', tail(strsplit(file[c], '_')[[1]], 1), '.')
    
    imported = import.feat.file(file_in = fullfile_path[c], color)
    new_feats_per_file = imported[[1]]
    derived_feats_names_blue = imported[[2]]
    derived_feats_names_red = imported[[3]]
    derived_feats_names_global = imported[[4]]
    
    feats_trim = trim.df.with.tidyr(df_subset = new_feats_per_file,
                                    feats_to_keep = plot_param[['feats_to_plot']], 
                                    bin_names, global_names, verbose = FALSE,
                                    check_for_empty_subjects = FALSE)
    
    if (c == 1) {
      feat_combined = feats_trim
    } else {
      feat_combined = cbind(feat_combined, feats_trim)
    }
  }
  
  return(feat_combined)
  
}

go.through.features = function(feat_combined, feats_to_keep) {
  
  list_feat_combined = list()
  for (f in 1:length(feats_to_keep)) {
    
    list_feats = feature.fields.to.list(df_trim = feat_combined,
                                        feat_in = feats_to_keep[[f]], 
                                        vars_to_plot = colnames(feat_combined))
    
    list_feat_combined[[feats_to_keep[[f]]]] = list_feats
    
  }
  
  return(list_feat_combined)

}