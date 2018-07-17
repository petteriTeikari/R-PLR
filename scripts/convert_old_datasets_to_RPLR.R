convert.old.datasets.to.RPLR = function() {
  
  library(tidyr)
  
  RPLR_path = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
  # RPLR_path = file.path(dirname(sys.frame(1)$ofile), '..', fsep = .Platform$file.sep)
  source(file.path(RPLR_path, 'PLR_reconstruction', 'subfunctions', 'create_PLR_dataset_matrix.R', fsep = .Platform$file.sep))
  source(file.path(RPLR_path, 'PLR_IO', 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))


  # INIT --------------------------------------------------------------------  
  
    data_path = '/home/petteri/Dropbox/PLR_data/SERI_2017'
    data_path_out = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017'
    
    if (grepl(data_path, 'SERI_2017')) {
      dataset = 'SERI2017'
    } else {
      warning('Nothing yet defined for the dataset at ', data_path)  
    }
  
  # IMPORT
    
    dataset = import.old.dataset(data_path, dataset)
    
}


import.old.dataset = function(data_path, dataset) {
  
  files_to_import = list()
  if (identical(dataset, 'SERI2017')) {
    fps = 30
    files_to_import[['blue']] = 'blue_short_outlierfree.csv'
    files_to_import[['red']]  = 'red_short_outlierfree.csv'
    files_to_import[['subj_code']]  = 'subject_code.csv'
    files_to_import[['classes_names']]  = 'class_name.csv'
    files_to_import[['classes_numerical']] = 'classes.csv'
  }
  
  names_files = names(files_to_import)
  data_in = list()
  for (i in 1 : length(names_files)) {
    filepath_in = file.path(data_path, files_to_import[[names_files[i]]], fsep = .Platform$file.sep)
    data_in[[names_files[i]]] = read.csv(filepath_in, header=FALSE)
  }
  
  if (identical(dataset, 'SERI2017')) {
    
    blue = data_in[['blue']]
    red = data_in[['red']]
    
    classes_raw = data_in[['classes_names']]
    classes = tidyr::gather(classes_raw)$value
    
    # combine pathologies
    classes = gsub('Normals_Control', 'CONTROL2017', classes)
    classes_early_ind = grepl('Early', classes)
    classes[classes_early_ind] = 'EARLY2017'
    
    classes_NTG_ind = grepl('NTG', classes)
    classes_POAG_ind = grepl('POAG', classes)
    remaining_ind = classes_NTG_ind | classes_POAG_ind
    classes[remaining_ind] = 'GLAUCOMA2017'
    
    colnames(blue) = data_in[['classes_names']]
    colnames(red) = data_in[['classes_names']]
    
    subject_codes =  tidyr::gather(data_in[['subj_code']])$value
    
    time = (1:dim(blue)[1])/fps
  }
  
  # Go through the columns (subjects) in the dataframe and save to disk
  no_of_subjects = dim(blue)[2]
  
  for (s in 1 : no_of_subjects) {
    
    t = time
    time_common_length = 1981 # just happen to come to this length from the main SERI 2018 dataset
    time_new = seq(from = t[1], to = tail(t,1), length.out = time_common_length)
    
    indices = get.corresponding.time.indices(t, time_new, fps, verbose_comb = FALSE)
    indices[indices == 0] = NA # remove possible ZEROS from indices
    indices = indices[!is.na(indices)] # remove possible NAs from indices
    
    y = blue[[s]]
    blue_new = y[indices]
    y = red[[s]]
    red_new = y[indices]
    
    # empty error vector
    error = as.numeric(vector(, length = length(red_new)))
    
    # plot(time_new, y_new, type='l')
    fps_out = fps* length(blue_new) / length(blue[[s]]) 
    
    no_of_nans_blue = sum(is.na(blue_new))
    no_of_nans_red = sum(is.na(red_new))
    
    # create dataframe(s) out 
    data_frame_out = data.frame(time = time_new, pupil = blue_new, error = error)
    filename = paste0(subject_codes[s], '_blue.csv')
    export.pupil.dataframe.toDisk(data_frame_out, filename, data_path_out, 'SERI2017_outlierfree')
    
    data_frame_out = data.frame(time = time_new, pupil = red_new, error = error)
    filename = paste0(subject_codes[s], '_red.csv')
    export.pupil.dataframe.toDisk(data_frame_out, filename, data_path_out, 'SERI2017_outlierfree')
    
    
  }
  
  
  
}



