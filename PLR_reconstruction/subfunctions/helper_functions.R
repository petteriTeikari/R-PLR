# SUBFUNCTIONS ------------------------------------------------------------

# GET FILES
get.files.for.reconstruction = function(data_path, pattern_to_find) {
  
  files_to_process = list.files(path=data_path, pattern=pattern_to_find, recursive=FALSE, full.names = TRUE)
  if (length(files_to_process) == 0) {
    warning('No files found from path =', data_path, ' with the following pattern = ', pattern_to_find)
  }
  return(files_to_process)
}

# INIT SCRIPT (i.e. HIDE THE BORING STUFF)
init.reconstruction = function(script.dir, data_path, source_path, IO_path) {
  
  # Libraries needed
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(reshape2)
  
  # https://stackoverflow.com/questions/7049272/importing-excel-files-into-r-xlsx-or-xls
  # install.packages("readxl") # CRAN version
  library(readxl)
  
  # IMPUTATION
  # install.packages("Amelia")
  # library(Amelia)
  # install.packages("imputeTS")
  library(imputeTS)
  
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
  library(wmtsa)
  # install.packages("ifultools")
  library(ifultools)
  # install.packages("Rwave")
  library(Rwave)
  
  # DECOMPOSITION
  # install.packages("EMD")
  library(EMD)
  # install.packages("hht")
  library(hht)
  
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
  
  
  # Debugging the "SERI syntax"
  data_path_out = file.path(data_path, '..', 'recon', fsep = .Platform$file.sep)
  if (dir.exists(data_path_out) == FALSE) {
    cat('Creating the directory for DATA output')
    dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  data_resampled_path_out = file.path(data_path, '..', 'recon_resampled', fsep = .Platform$file.sep)
  if (dir.exists(data_resampled_path_out) == FALSE) {
    cat('Creating the directory for DATA output')
    dir.create(data_resampled_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  data_trimmed_path_out = file.path(data_path, '..', 'recon_trimmed', fsep = .Platform$file.sep)
  if (dir.exists(data_trimmed_path_out) == FALSE) {
    cat('Creating the directory for DATA output')
    dir.create(data_trimmed_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  data_temp_path_out = file.path(data_path, '..', 'recon_temp', fsep = .Platform$file.sep)
  if (dir.exists(data_temp_path_out) == FALSE) {
    cat('Creating the directory for DATA output')
    dir.create(data_temp_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  return(list(data_path_out, data_resampled_path_out, data_trimmed_path_out, data_temp_path_out))
  
}

# INPUT LIMITS
dataset.lims = function(time_range, hard_limits, debugON, fps = 30) {
  
  # Get the minimum of time vector
  no_of_cols = length(time_range[[1]])
  unlisted_range = unlist(time_range) # this becomes a vector
  no_of_rows = length(unlisted_range)/no_of_cols
  mat_range = matrix(unlisted_range, nrow = no_of_rows, byrow = TRUE)
  
  # Now get the largest start time, and smallest end time
  # And the minimum number of samples before and after
  time_limits = c(max(mat_range[,1]), min(mat_range[,2]),
                  max(mat_range[,3]), min(mat_range[,4]),
                  min(mat_range[,5]), min(mat_range[,6]))
  
  time_lims_ind =  c(time_limits[5], time_limits[6])
  
  # drop the outlier length recordings from modeling the input data
  filter_out = plot.and.filter.time.lims(mat_range, time_limits, hard_limits, files_to_process, fps, debugON = FALSE)
  exclusion_boolean = filter_out[[1]]
  time_new = filter_out[[2]]
  
  return(list(time_limits, time_lims_ind, exclusion_boolean, time_new))
  
}  

# PLOT SUBFUNCTION
plot.and.filter.time.lims = function(mat_range, time_limits, hard_limits, files_to_process, fps, debugON) {
  
  # Melt for plotting
  samples_before_df = melt(data.frame(before = mat_range[,5]))
  samples_after_df = melt(data.frame(after = mat_range[,6]))
  
  # Find extremes
  before_lims = c(min(samples_before_df$value), max(samples_before_df$value))
  before_lims_ind = c(which.min(samples_before_df$value), which.max(samples_before_df$value))
  after_lims = c(min(samples_after_df$value), max(samples_after_df$value)) 
  after_lims_ind = c(which.min(samples_after_df$value), which.max(samples_after_df$value)) 
  
  # Find the corresponding files for these extremes
  file_before_least_samples = files_to_process[before_lims_ind[1]]
  file_before_most_samples = files_to_process[before_lims_ind[2]]
  file_after_least_samples = files_to_process[after_lims_ind[1]]
  file_after_most_samples = files_to_process[after_lims_ind[2]]
  
  # Let's use the limits defined for the light onset adjusted
  if (time_limits[3] > hard_limits[1]) {
    warning('Your hand-defined MINIMUM TIME LIMIT is smaller than the lowest time point found in the DATASET')
  }
  
  if (time_limits[4] > hard_limits[2]) {
    warning('Your hand-defined MAXIMUM TIME LIMIT is LARGER than the largest end time found in the DATASET')
  }
  
  max_samples_for_dataset_possible = time_limits[5] + time_limits[6]
  if (debugON) {
    plot.recording.histograms(samples_before_df, before_lims, samples_after_df, after_lims)
  }
  
  # If we have couple of weirdly short  recordings, we can throw them away for modeling
  # the input files with equal lengths (like using matrix completion or something other requiring
  # a matrix with same amount of rows and columns)
  # TODO! hard_limits now defined manually (hard-coded) so you could make them adaptive as well later
  too_short_before_boolean = mat_range[,5] < hard_limits[1]
  too_short_before_indices = which(too_short_before_boolean == TRUE)
  too_short_after_boolean = mat_range[,6] < hard_limits[2]
  too_short_after_indices = which(too_short_after_boolean == TRUE)
  
  cat('We found', length(too_short_before_indices), 'files that had too few samples BEFORE light onset (threshold =', hard_limits[1], 'samples)\n')
  for (i in 1:length(too_short_before_indices)) {
    cat(' .. ', files_to_process[too_short_before_indices[i]], '\n')  
  }
  
  cat('We found', length(too_short_after_indices), 'files that had too few samples AFTER light onset (threshold =', hard_limits[2], 'samples)\n')
  for (i in 1:length(too_short_after_indices)) {
    cat(' .. ', files_to_process[too_short_after_indices[i]], '\n')  
  }
  
  #  Combine now the indices as some files can appear in both lists
  exclusion_boolean = too_short_before_boolean | too_short_after_boolean
  exclusion_indices = which(exclusion_boolean == TRUE)
  cat('In the end the following', length(exclusion_indices), 'are excluded from modelling\n')
  for (i in 1:length(exclusion_indices)) {
    cat(' .. ', files_to_process[exclusion_indices[i]], '\n')  
  }
  
    # NOTE! the more loose the threshold, the shorter your trimmed files for modeling going to be 
    # e.g. if you approve all the files above 454 samples, then all files files for modeling will be trimmed to that BEFORE length
    # in practice this may or may not have an effect, but 6 samples less from before light onset in practice is not going to make
    # a huge difference if you get in more files for modeling. 
  
  # get rid of the excluded files
  mat_range2 = mat_range[!exclusion_boolean,]
  
  # Melt again and plot
  samples_before_df2 = melt(data.frame(before = mat_range2[,5]))
  samples_after_df2 = melt(data.frame(after = mat_range2[,6]))
  
  # Find extremes again
  before_lims2 = c(min(samples_before_df2$value), max(samples_before_df2$value))
  after_lims2 = c(min(samples_after_df2$value), max(samples_after_df2$value)) 
  
  if (debugON) {
    plot.recording.histograms(samples_before_df2, before_lims2, samples_after_df2, after_lims2)
  }
  
  # if instead of trimming the recordings to the same length, we would rather
  # want to redefine the time vectors (as due to technical glitch, there might be gaps in data)
  samples_total = fps*(time_lims[2] - time_lims[1]) + 1
  time_new = seq(from = time_lims[1], to = time_lims[2], length = samples_total)
  
  return(list(exclusion_boolean, time_new))
  
}

plot.recording.histograms = function(samples_before_df, before_lims, samples_after_df, after_lims) {

  p = list()

  # https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
  p[[1]] = ggplot(samples_before_df, aes(value, fill = variable)) + geom_histogram(alpha = 0.75, binwidth = 1)
  p[[1]] = p[[1]] + labs(x="Number of samples", 
                         y="Number of files",
                         title="Length Jitter",
                         subtitle=paste('Min =', before_lims[1], 'samples, Max = ', before_lims[2], ' samples'))
  
  p[[2]] = ggplot(samples_after_df, aes(value, fill = variable)) + geom_histogram(alpha = 0.95, binwidth = 1)
  p[[2]] = p[[2]] + labs(x="Number of samples", 
                         y="Number of files",
                         title="Length Jitter",
                         subtitle=paste('Min =', after_lims[1], 'samples, Max = ', after_lims[2], ' samples'))
  
  
  no_of_cols = 2 # TODO! make adaptive to list length
  do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
  
}

estimate.light.range.for.trimmed = function(list_of_PLR_dataframes) {
  
  # "guess common" light_range from the first file
  # namely, get the length of the blue and red light durations and use that for all
  
  # TODO! make a bit more intelligent later
  frame = list_of_PLR_dataframes[[1]]
  light_range = define.whenLightWasOn(frame)
  return(light_range)
  
}

combine.list.of.dataframes = function(list_of_PLR_dataframes, vars_to_keep, normalize = TRUE) {
  
  number_of_files = length(list_of_PLR_dataframes)
  no_of_samples = length(list_of_PLR_dataframes[[1]]$X)
  
  norm_name = 'pupil_norm'
  col_names = c(vars_to_keep, norm_name)
  
  # init list
  list_out = list()
  # number_of_files = 4
  
  for (i in 1 : number_of_files) {
    
    # cat(' .. #', i, number_of_files, ' ')
    
    # from list to data frame
    dataframe = list_of_PLR_dataframes[[i]]
    
    # subset of desired variables
    df_subset = dataframe[vars_to_keep]
    
    # define baseline
    bins = import.binDefinitions(config_path)
    baseline_period = get.baseline.period.from.bins(bins)
    baseline_period = c(-5, 0)
    b_i1 = which.min(abs(baseline_period[1] - df_subset$time_onsetZero))
    b_i2 = which.min(abs(baseline_period[2] - df_subset$time_onsetZero))
    baseline = median(df_subset$pupil_outlier_corrected[b_i1:b_i2], na.rm = TRUE)
    
    if (normalize) {
      df_subset[[norm_name]] = normalize.low.level(df_subset$pupil_outlier_corrected, 
                                                   baseline, normalize_method = 'hybrid', if_pupil_value = TRUE)
    }
    
    for (j in 1 : length(col_names)) {
      
      if (i == 1) {
        
        # preallocate the matrix on first file
        list_out[[col_names[j]]] = matrix(nrow=no_of_samples, ncol=number_of_files)
        vector = df_subset[[col_names[j]]]
        list_out[[col_names[j]]][,i] = vector
        
      } else {
        
        vector = df_subset[[col_names[j]]]
        list_out[[col_names[j]]][,i] = vector
        
      }
    }
  }
  
  return(list_out)
  
}

convert.vectors.into.dataframe = function(t, y, error_frac, weights_norm, filecode) {
  
  pupil_df = data.frame(time = t, subject = filecode, pupil = y, 
                        error_frac = error_frac, weights = weights_norm)
  
  return(pupil_df)
}

convert.matrices.into.dataframe = function(t, y, error_frac, weights_norm, filecodes) {
  
  time_vector = t[,1] # cheating a bit and assuming that all the time vectors are indeed the same
  
  colnames(y) = filecodes
  rownames(y) = time_vector
  y_df = melt(y)
  colnames(y_df) = c('time', 'subject', 'pupil')
  
  colnames(error_frac) = filecodes
  rownames(error_frac) = time_vector
  error_df = melt(error_frac)
  colnames(error_df) = c('time', 'subject', 'error_frac')
  
  colnames(weights_norm) = filecodes
  rownames(weights_norm) = time_vector
  weights_df = melt(weights_norm)
  colnames(weights_df) = c('time', 'subject', 'weights')
  
  # combine these all, so we should have 2 factors (time and subject), and then 3 variables:
  # pupil size, fractional error and normalized weights
  pupil_df = cbind(y_df, error_df$error_frac, weights_df$weights)
  colnames(pupil_df) = c('time', 'subject', 'pupil', 'error_frac', 'weights')
  
  return(pupil_df)
}


convert.vectors.to.time.series.object = function(t, y, filecodes, error_frac = NA, weights_norm = NA, fps = 30) {

  frequency = 1 # time-series often defined for month, so in other applications you could
  # have 10 years of data with frequency then being 10 if you are interested
  # in annual patterns
  
  # Again time series object will assume that your data would come with time stamps whereas 
  # our pupil recording only have the time vector in seconds, so make this conform with the
  # expectations for time series objects
  
  isVector = FALSE
  # Input is a vector
  if (length(dim(t)) == 0) {
    timeVec = t  
    isVector = TRUE
  } else { # matrix
    timeVec = t[,1]
  }
  
  baseline_time = '17:00:00.0' # Arbitrary start point
  startTime = timeVec - t[1] # make first time point zero
  begin_string = paste(baseline_time, sep='')
  totalTime_sec = tail(timeVec,1)
  if (totalTime_sec > 60) {
    warning('Implement minute addition later')
  }
  
  full_seconds = floor(totalTime_sec)
  digits = 4
  fraction_of_seconds = round(10^digits*(totalTime_sec - full_seconds), digits=4)
  
  end_string = paste('17:00:', full_seconds, '.', fraction_of_seconds, sep='')
  
  # Typically time series are given at second-level precision whereas we have fractions
  # https://stackoverflow.com/questions/34927600/sequence-with-fractional-second-intervals-in-r
  options(digits.secs=4)
  begin_time = as.POSIXct(begin_string, format="%H:%M:%OS",tz="UTC")
  end_time = as.POSIXct(end_string, format="%H:%M:%OS", tz="UTC")
  interval = 1 / as.numeric(fps);
  time.seq = seq(from=begin_time, to=end_time, by=interval)
  
  # ts_pupil = ts(y, start = begin_time, end = end_time, frequency = frequency)
  # Error in attr(data, "tsp") <- c(start, end, frequency) : 
  # invalid time series parameters specified
  ts_pupil = ts(y, frequency = frequency)
  
  if (!missing(filecodes) & !isVector) {
    colnames(ts_pupil) = filecodes
  }
  
  return(ts_pupil)
  
}

