# BORING INIT -------------------------------------------------------------

path = '/home/petteri/Dropbox/PLR_data/INSERM_PLR'
path_out = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/INSERM_PLR'

train_data = 'Trad_PLR_TRAIN.csv'
test_data = 'Trad_PLR_TEST.csv'

library(tidyr)
library(imputeTS)

RPLR_path = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
RPLR_recon_path = file.path(RPLR_path, 'PLR_reconstruction')

source(file.path(RPLR_path, 'PLR_reconstruction', 'subfunctions', 'create_PLR_dataset_matrix.R', fsep = .Platform$file.sep))
source(file.path(RPLR_path, 'PLR_IO', 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
source_path = file.path(RPLR_path, 'PLR_artifacts', 'subfunctions')
source(file.path(source_path, 'clean_plr.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'changepoint_detection.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'spline_filter.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'clean_trace_simple.R', fsep = .Platform$file.sep))
source(file.path(RPLR_path, 'PLR_reconstruction', 'subfunctions', 'helper_functions.R'))
source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/scripts/combine_data_from_multiple_folders.R')
source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/PLR_reconstruction/subfunctions/lowLevel_imputation_wrappers.R')

train = read.csv(file.path(path, train_data, fsep = .Platform$file.sep), header = FALSE)
test = read.csv(file.path(path, test_data, fsep = .Platform$file.sep), header = FALSE)
all_traces = rbind(train, test)

labels = all_traces[,1]
labels[labels == 0] = 'noisy'
labels[labels == 1] = 'cleaned'
data = all_traces[,2:dim(train)[2]]

last_time_value = 15 * 60 # 15 min x 60 seconds
time = 1 : dim(data)[2]
multiplier = last_time_value / tail(time, 1)
time = time * multiplier
fps_in = 1 / time[2] - time[1]


# Clean a bit and save ---------------------------------------------------------------------

  # Save to disk 
  length_out = 1981 # hard-coded now to match the length of the "SERI 2018" traces
  
  for (i in 339 : length(labels)) {
    
    time_new = seq(from = time[1], to = tail(time,1),
                   length.out = length_out)
    
    trace_in = tidyr::gather(data[i,])$value
    error = as.numeric(vector(, length(data[i,])))
    
    fps_out = 1 / time_new[2] - time_new[1]
    
    data_frame_in = data.frame(time = time, pupil = trace_in,
                               error = error)
    
    df_out = clean.trace.simple(data_frame_in)
    
    # re-normalize
    i1 = 200
    i2 = 240
    
    renormalized_raw = renormalize.value(baseline_out = median(trace_in, na.rm = TRUE), 
                                     baseline_in = median(trace_in, na.rm = TRUE), 
                                     vector = trace_in, i1 = i1, i2 = i2)
    
    renormalized = renormalize.value(baseline_out = median(df_out$pupil[i1:i2], na.rm = TRUE), 
                                     baseline_in = median(df_out$pupil[i1:i2], na.rm = TRUE), 
                                     vector = df_out$pupil, i1 = i1, i2 = i2)
    
    df_out$pupil = renormalized[[1]]
    df_out[['pupil_raw']] = renormalized_raw[[1]]
    # plot(df_out$time, df_out$pupil, type = 'l')
    
    # Kalman fails for the manual exclusions
    # L-BFGS-B needs finite values of 'fn' 
    # TODO add try/catch!
    
    # use simple Kalman imputation
    if (sum((is.na(df_out$pupil))) != length(df_out$pupil) && i != 94 && i != 147 && i != 158 && i != 204 && i != 259 && i != 313 && i != 339)  {
      
      y = df_out$pupil
      y_na = na.kalman(df_out$pupil, model ="StructTS")
      
      # interpolate for output
      spline_results = spline(time, y = y_na, xout = time_new)
      y_interp = spline_results$y
      
      spline_results2 = spline(time, y = df_out[['pupil_raw']], xout = time_new)
      y_raw = spline_results2$y
      
      df_out_interp = data.frame(time = time_new, pupil = y_interp, pupil_raw = y_raw,
                                 error = as.numeric(vector(,length(time_new))))
      
      df_out[['pupil_raw']] = 
        
      sequence_no = sprintf("%.4d",i)
      filename = paste0(sequence_no, '_', labels[i], '.csv')
      export.pupil.dataframe.toDisk(df_out_interp, filename, path_out, 'INSERMn1981')
      
    } else {
    
      warning('All values were NAs for ', i)
        
    }
    
    
    
  }

