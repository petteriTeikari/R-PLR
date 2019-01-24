save.matrices.for.deep.learning = function(data_frame_feats, list_traces, subject_codes,
                                           list_agematched, master_indices_out, grouping_vars_out,
                                           combine_pathology = TRUE, 
                                           factors_kept = c('CONTROL', 'NTG', 'POAG', 'GLAUCOMA+', 'OTHER GLAUCOMA', 'PACG'),
                                           parameters, settings) {
  
  # Combine POAG/NTG -> Glaucoma
  if (combine_pathology) {
    cat('Combining (sub)pathologies as one')
    factors_in = combine.pathologies(factors_in = grouping_vars_out, 
                                     factors_kept = factors_kept) # parameters[['factors_keep']][[parameters[['main_factor']]]])
    # grouped_list = split.into.groups.by.grouping(list_agematched, factors_in, parameters)
  }
  
  # Export the input data list to disk
  y_agematched = export.list.to.disk.for.deep.learning(t = list_agematched$time[,1], 
                                                       y = list_agematched$pupil, 
                                                       err = list_agematched$error, 
                                                        classes = factors_in, 
                                                        subject_codes = subject_codes, # [master_indices_out],
                                                        title_string = 'glaucoma_SERI',
                                                        path_out = settings[['data_deep_path_out']],
                                                        split_into_test_and_train = TRUE)
  
  # Synthesize data now for data augmentation
  augm_list = augment.traces.for.deep.learning(list_in = list_agematched,
                                              list_full = trim.list(list_traces, master_indices_out),
                                              t = list_agematched$time[,1],
                                              y = list_agematched$pupil,
                                              err = list_agematched$error,
                                              classes = factors_in,
                                              subject_code = subject_codes[master_indices_out])

  y_augm = augm_list[[1]]
  classes_augm = augm_list[[2]]
  subjects_augm = augm_list[[3]]
  
  # Combine POAG/NTG -> Glaucoma
  if (combine_pathology) {
    cat('Combining (sub)pathologies as one also for the augmented set')
    factors_augm = combine.pathologies(factors_in = classes_augm, 
                                       factors_kept = factors_kept) # parameters[['factors_keep']][[parameters[['main_factor']]]])
    # grouped_list = split.into.groups.by.grouping(list_agematched, factors_in, parameters)
  }
  
  # Augment the dataset with the raw input
  # TODO!

  # Export the augmented data list to disk
  y_agematched_aug = export.list.to.disk.for.deep.learning(t = list_agematched$time[,1],
                                                       y = y_augm,
                                                       err = list_agematched$error,
                                                       classes = factors_augm,
                                                       subject_code = subjects_augm,
                                                       title_string = 'glaucoma_SERI_augm',
                                                       path_out = settings[['data_deep_path_out']],
                                                       split_into_test_and_train = TRUE)
  
  
  
}


augment.traces.for.deep.learning = function(list_in, list_full, t, y, error,
                                            classes, subject_code, method = '1st_try_average',
                                            increase_by_nr = 10, range = c(-0.3, 0.3)) {
  
  
  cat('Augmentation method is = ', method, '\n')
  
  if (identical(method, '1st_try') | identical(method, '1st_try_average')) {
  
    for (subj in 1 : dim(y)[2]) {
    
      y_subj = y[,subj]
      
      hiFreq = list_full$hiFreq[,subj]
      loFreq = list_full$loFreq[,subj] 
      noise = rowSums(cbind(list_full$noiseNorm[,subj], list_full$noiseNonNorm[,subj]), na.rm = TRUE)
      base = list_full$base_osc[,subj] 
      
      # loFreq
      multip1 = runif(1, range[1], range[2])
      y_augm1 = rowSums(cbind(y_subj, (multip1*loFreq)), na.rm = TRUE)
      
      # loFreq + hiFreq
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      y_augm2 = rowSums(cbind(y_subj, (multip1*loFreq), (multip2*hiFreq)), na.rm = TRUE)
      
      # loFreq + noise
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      y_augm3 = rowSums(cbind(y_subj, (multip1*loFreq), (multip2*noise)), na.rm = TRUE)
      
      # hiFreq
      multip1 = runif(1, range[1], range[2])
      y_augm4 = rowSums(cbind(y_subj, (multip1*hiFreq)), na.rm = TRUE)
      
      # hiFreq + noise
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      y_augm5 = y_subj + (multip1*hiFreq) + (multip2*noise)
      
      # hiFreq + loFreq + noise
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      multip3 = runif(1, range[1], range[2])
      y_augm6 = y_subj + (multip1*hiFreq) + (multip2*loFreq) + (multip3*noise)
      
      # hiFreq + loFreq + noise + base
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      multip3 = runif(1, range[1], range[2])
      multip4 = runif(1, range[1], range[2])
      y_augm7 = y_subj + (multip1*hiFreq) + (multip2*loFreq) + (multip3*noise) + (multip4*base)
      
      # Smooth1
      loess_model_deg2 = loess(hiFreq~t, span = 0.05, degree = 2)
      residual_hi_deg2 = hiFreq - loess_model_deg2$fitted
      y_augm8 = y_subj - residual_hi_deg2
      
      # Smooth2
      loess_model_lo = loess(loFreq~t, span = 0.1, degree = 2)
      residual_lo = loFreq - loess_model_lo$fitted
      y_augm9 = y_augm8 - residual_lo
      
      # Smooth3
      loess_smooth = loess(y_augm9~t, span = 0.1, degree = 2)
      residual_smooth = y_augm9 - loess_smooth$fitted
      y_augm10 = y_augm9 - residual_smooth
      
      no_of_cols = 11
      y_new_per_subject = cbind(y_subj, y_augm1, y_augm2, y_augm3, y_augm4, y_augm5,
                                y_augm6, y_augm7, y_augm8, y_augm9, y_augm10)
    
      
      labels_new_per_subject = rep(classes[subj], no_of_cols)
      subjectcode_new_per_subject = rep(subject_code[subj], no_of_cols)
      
      if (subj == 1) {
        
        y_out = y_new_per_subject
        labels_out = labels_new_per_subject
        subjects_out = subjectcode_new_per_subject
        
      } else {
        
        y_out = cbind(y_out, y_new_per_subject)
        labels_out = c(labels_out, labels_new_per_subject)
        subjects_out = c(subjects_out, subjectcode_new_per_subject)
        
      }
      
    } # end of for loop
    
    if (identical(method, '1st_try_average')) {
      
      # NOTE! these part have been added later if you are wondering about slightly
      # redundant looping
      
      # # as a simple method to reduce the synthetic data, we take an average of the synthetic data
      number_of_timepoints = size(y_out)[1]
      number_of_subjects_now = size(y_out)[2] 
      downsample_factor = number_of_subjects_now / size(y)[2]
      
      # go through the subjects
      for (sub in seq(1,number_of_subjects_now,downsample_factor)) {
       
        # indices for given subject
        i1 = sub
        i2 = sub + downsample_factor - 1
        count = i2 / downsample_factor
        
        # mean (first column is the original)
        y_orig = y_out[,i1]
        y_mean = rowMeans(y_out[,(i1+1):i2])
        
        #plot(y_orig)
        #plot(y_mean)
        #plot(y_mean - y_orig)
        
        y_new_duo = cbind(y_orig, y_mean)
        
        # these should be the same
        labels_new_duo = c(labels_out[i1], labels_out[i1+1])
        subject_new_duo = c(subjects_out[i1], subjects_out[i1+1])
          
        if (sub == 1) {
          
          y_out2 = y_new_duo
          labels_out2 = labels_new_duo
          subjects_out2 = subject_new_duo
          
        } else {
          
          y_out2 = cbind(y_out2, y_new_duo)
          labels_out2 = c(labels_out2, labels_new_duo)
          subjects_out2 = c(subjects_out2, subject_new_duo)
          
        }
      }
      
      y_out = y_out2
      labels_out = labels_out2
      subjects_out = subjects_out2
      
    }
    
  }
  
  return(list(y_out, labels_out, subjects_out))
  
}


export.list.to.disk.for.deep.learning = function(t, y, err, classes, subject_codes,
                                                 time_col = 'time', pupil_col = 'pupil', error_col = 'error',
                                                 title_string = 'glaucoma_agematched',
                                                 path_out = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/for_deepLearning',
                                                 split_into_test_and_train = TRUE,
                                                 train_ratio = 0.8, seed_no = 12345) {
  
  # Easier to read in the file so that 1st column is the class label (numeric), with each
  # row presenting one subject (i.e. UCR_TS_Archive_2015 format) so that the developed model
  # can be easily compared to existing time series classification methods of which many are
  # developed using the UCR_TS_Archive 2015 as the benchmark
  
  # Convert the class strings into numeric values
  classes_unique = unique(classes)
  classes_numeric = classes
  train_indices = list()
  test_indices = list()
  
  set.seed(seed_no)
  
  for (c in 1 : length(classes_unique)) {
    
    indices = classes %in% classes_unique[c]
    classes_numeric[indices] = c - 1 # start from zero
    subject_codes_per_class = subject_codes[indices]
    no_of_indices = sum(indices)
    
    # Split all the traces into fixed  train and test sets here
    if (split_into_test_and_train) {
    
      y_temp = y[,indices]    
      rnd_indices = sample(1:no_of_indices, no_of_indices, replace=FALSE)
      
      index_limit = round(train_ratio*no_of_indices)
      indices_train = rnd_indices[1:index_limit]
      indices_test = rnd_indices[(index_limit+1):no_of_indices]
      
      if (c == 1) {
        
        y_train = y_temp[,indices_train]
        y_test = y_temp[,indices_test]
        subj_code_train = subject_codes_per_class[indices_train]
        subj_code_test = subject_codes_per_class[indices_test]
        classes_train = classes_numeric[indices][indices_train]
        classes_test = classes_numeric[indices][indices_test]
        
      } else {
        
        y_train = cbind(y_train, y_temp[,indices_train])
        y_test = cbind(y_test, y_temp[,indices_test])
        subj_code_train = c(subj_code_train, subject_codes_per_class[indices_train])
        subj_code_test = c(subj_code_test, subject_codes_per_class[indices_test])
        classes_train = c(classes_train, classes_numeric[indices][indices_train])
        classes_test = c(classes_test, classes_numeric[indices][indices_test])
      }
      
    }
    
  }
  classes_numeric = as.numeric(classes_numeric)
  classes_train = as.numeric(classes_train)
  classes_test = as.numeric(classes_test)
  
  # join the class label as the first column
  y_out = t(rbind(classes_numeric, y)) # all traces
  y_out_train = t(rbind(classes_train, y_train))
  y_out_test = t(rbind(classes_test, y_test))
  
  cat(paste0('   exporting to: ', path_out, '\n      3 files: all/train/test, for data = ', title_string))
  
  # export the data (PLR traces)
  write.table(y_out, 
              file = file.path(path_out, paste0(title_string, '_traces.csv'), fsep = .Platform$file.sep),
              sep = ",", row.names=FALSE, col.names=FALSE)
  
  write.table(y_out_train, 
              file = file.path(path_out, paste0(title_string, '_traces_train.csv'), fsep = .Platform$file.sep),
              sep = ",", row.names=FALSE, col.names=FALSE)
  
  write.table(y_out_test, 
              file = file.path(path_out, paste0(title_string, '_traces_test.csv'), fsep = .Platform$file.sep),
              sep = ",", row.names=FALSE, col.names=FALSE)
  
  # export the time vector (in case you need)
  write.table(t(t), 
              file = file.path(path_out, paste0(title_string, '_time.csv'), fsep = .Platform$file.sep),
              sep = ",", row.names=FALSE, col.names=FALSE)
  
  # export the subject codes (in case you need)
  write.table(subject_codes, 
              file = file.path(path_out, paste0(title_string, '_subjectCodes.csv'), fsep = .Platform$file.sep),
              sep = ",", row.names=FALSE, col.names=FALSE)
  
  write.table(subj_code_train, 
              file = file.path(path_out, paste0(title_string, '_subjectCodes_train.csv'), fsep = .Platform$file.sep),
              sep = ",", row.names=FALSE, col.names=FALSE)
  
  write.table(subj_code_test, 
              file = file.path(path_out, paste0(title_string, '_subjectCodes_test.csv'), fsep = .Platform$file.sep),
              sep = ",", row.names=FALSE, col.names=FALSE)
  
  # TODO! error
  
  
  
  
  # RETURN
  return(y_out)
  
}
