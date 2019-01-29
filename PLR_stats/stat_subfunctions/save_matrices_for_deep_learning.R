save.matrices.for.deep.learning = function(data_frame_feats, list_traces, subject_codes_out,
                                           list_agematched, master_indices_out, grouping_vars_out,
                                           combine_pathology = TRUE, 
                                           factors_kept = c('CONTROL', 'NTG', 'POAG', 'GLAUCOMA+', 'OTHER GLAUCOMA', 'PACG'),
                                           parameters, settings) {
  
  # DEBUG POINT DEEP
  # save.image(file = file.path(debug_path, 'debug_poinDEEP.Rdata')) # 176.9 MB
  # load(file = file.path(debug_path, 'debug_pointDEEP.Rdata'))
  
  # Combine POAG/NTG -> Glaucoma
  if (combine_pathology) {
    cat('Combining (sub)pathologies as one')
    factors_in = combine.pathologies(factors_in = grouping_vars_out, 
                                     factors_kept = factors_kept) # parameters[['factors_keep']][[parameters[['main_factor']]]])
    # grouped_list = split.into.groups.by.grouping(list_agematched, factors_in, parameters)
  }
  
  # Export the input data list to disk
  y_train_disk = export.list.to.disk.for.deep.learning(t = list_agematched$time[,1], 
                                                       y = list_agematched$pupil, 
                                                       err = list_agematched$error, 
                                                        classes = factors_in, 
                                                        subject_codes = subject_codes_out, # [master_indices_out],
                                                        title_string = 'glaucoma_SERI',
                                                        path_out = settings[['data_deep_path_out']],
                                                        split_into_test_and_train = TRUE)
  
  # save.image(file = file.path(debug_path, 'debug_point_deep.Rdata')) # 186.4 MB
  # load(file = file.path(debug_path, 'debug_point_deep.Rdata')) # 186.4 MB
  size_train = size(y_train_disk[['TrainData']]) # e.g. 241 x 1981(+1)
  labels_train = y_train_disk[['TrainData']][,1]
  y_train = unname(y_train_disk[['TrainData']][,2:size_train[2]])
  train_indices = y_train_disk[['TrainIndices']]
  
  # TODO! This now hard-coded
  labels_train[labels_train == 0] = 'Control'
  labels_train[labels_train == 1] = 'Glaucoma'
  
  # Synthesize data now for data augmentation
  augm_list = augment.traces.for.deep.learning(list_in = list_agematched,
                                              list_full = trim.list(list_traces, master_indices_out),
                                              t = list_agematched$time[,1],
                                              y = y_train,
                                              err = t(list_agematched$error[,train_indices]),
                                              classes = labels_train,
                                              subject_code_augm = subject_codes_out[train_indices], 
                                              method = '1st_try',
                                              train_indices = train_indices)

  y_augm = augm_list[[1]]
  classes_augm = augm_list[[2]]
  subjects_augm = augm_list[[3]]
  methods_augm = augm_list[[4]]
  
  # Combine POAG/NTG -> Glaucoma
  if (combine_pathology) {
    cat('Combining (sub)pathologies as one also for the augmented set')
    factors_augm = combine.pathologies(factors_in = classes_augm, 
                                       factors_kept = factors_kept) # parameters[['factors_keep']][[parameters[['main_factor']]]])
    # grouped_list = split.into.groups.by.grouping(list_agematched, factors_in, parameters)
  }
  
  # DEBUG POINT DEEPfinal
  # save.image(file = file.path(debug_path, 'debug_poinDEEPfinal.Rdata')) # 540.6 MB
  # load(file = file.path(debug_path, 'debug_pointDEEPfinal.Rdata'))
  
  # Export the augmented data list to disk
  test_file = file.path(path_out = settings[['data_deep_path_out']], 'glaucoma_SERI_traces_test.csv')
  test_subjects = file.path(path_out = settings[['data_deep_path_out']], 'glaucoma_SERI_subjectCodes_test.csv')
  y_agematched_aug = export.list.to.disk.for.deep.learning(t = list_agematched$time[,1],
                                                       y = y_augm,
                                                       err = list_agematched$error, # TODO! Not correct but not needed now
                                                       classes = factors_augm,
                                                       subject_codes = subjects_augm,
                                                       title_string = 'glaucoma_SERI_augm',
                                                       path_out = settings[['data_deep_path_out']],
                                                       split_into_test_and_train = FALSE,
                                                       export_augm = TRUE,
                                                       methods_augm = methods_augm,
                                                       test_file = test_file, 
                                                       test_subjects = test_subjects)
  
  
  
}


augment.traces.for.deep.learning = function(list_in, list_full, t, y, error,
                                            classes, subject_code_augm, method = '1st_try',
                                            increase_by_nr = 10, range = c(-0.3, 0.3),
                                            train_indices = train_indices) {
  
  cat('Augmentation method is = ', method, '\n')
  cat('  The size of the input training data is now = ', dim(y),'\n')
  cat('     Trim the incoming data in lists to training data indices\n\n')
  
  method_strings = list()
  method_str = list()
  
  for (nam in 1 : length(list_in)) {
    list_in[[nam]] = list_in[[nam]][,train_indices]
  }
  
  for (nam in 1 : length(list_full)) {
    list_full[[nam]] = list_full[[nam]][,train_indices]
  }
  
  # TODO! verify the correct lengths of all! n = 241 for the PLR dataset
  if (identical(method, '1st_try')) {
  
    for (subj in 1 : dim(y)[1]) {
      
      i = 1
      method_str[i] = 'Input'
    
      cat('    augmenting subject #', subj, '/', dim(y)[1], '\n')
      y_subj = y[,subj]
      # cat('       ... trace length = ', length(y_subj), '\n')
      
      
      hiFreq = list_full$hiFreq[,subj]
      loFreq = list_full$loFreq[,subj] 
      noise = rowSums(cbind(list_full$noiseNorm[,subj], list_full$noiseNonNorm[,subj]), na.rm = TRUE)
      base = list_full$base_osc[,subj] 
      
      # loFreq
      i = i + 1
      multip1 = runif(1, range[1], range[2])
      y_augm1 = rowSums(cbind(y_subj, (multip1*loFreq)), na.rm = TRUE)
      method_str[i] = 'loFreq'
      
      # loFreq + hiFreq
      i = i + 1
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      y_augm2 = rowSums(cbind(y_subj, (multip1*loFreq), (multip2*hiFreq)), na.rm = TRUE)
      method_str[i] = 'loFreq_hiFreq'
      
      # loFreq + noise
      i = i + 1
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      y_augm3 = rowSums(cbind(y_subj, (multip1*loFreq), (multip2*noise)), na.rm = TRUE)
      method_str[i] = 'loFreq_noise'
      
      # hiFreq
      i = i + 1
      multip1 = runif(1, range[1], range[2])
      y_augm4 = rowSums(cbind(y_subj, (multip1*hiFreq)), na.rm = TRUE)
      method_str[i] = 'hifreq'
      
      # hiFreq + noise
      i = i + 1
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      y_augm5 = y_subj + (multip1*hiFreq) + (multip2*noise)
      method_str[i] = 'hifreq_noise'
      
      # hiFreq + loFreq + noise
      i = i + 1
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      multip3 = runif(1, range[1], range[2])
      y_augm6 = y_subj + (multip1*hiFreq) + (multip2*loFreq) + (multip3*noise)
      method_str[i] = 'hifreq_lofreq_noise'
      
      # hiFreq + loFreq + noise + base
      i = i + 1
      multip1 = runif(1, range[1], range[2])
      multip2 = runif(1, range[1], range[2])
      multip3 = runif(1, range[1], range[2])
      multip4 = runif(1, range[1], range[2])
      y_augm7 = y_subj + (multip1*hiFreq) + (multip2*loFreq) + (multip3*noise) + (multip4*base)
      method_str[i] = 'hifreq_lofreq_noise_base'
      
      # Smooth1
      i = i + 1
      loess_model_deg2 = loess(hiFreq~t, span = 0.05, degree = 2)
      residual_hi_deg2 = hiFreq - loess_model_deg2$fitted
      y_augm8 = y_subj - residual_hi_deg2
      method_str[i] = 'loess_smooth1'
      
      # Smooth2
      i = i + 1
      loess_model_lo = loess(loFreq~t, span = 0.1, degree = 2)
      residual_lo = loFreq - loess_model_lo$fitted
      y_augm9 = y_augm8 - residual_lo
      method_str[i] = 'loess_smooth2'
      
      # Smooth3
      i = i + 1
      loess_smooth = loess(y_augm9~t, span = 0.1, degree = 2)
      residual_smooth = y_augm9 - loess_smooth$fitted
      y_augm10 = y_augm9 - residual_smooth
      method_str[i] = 'loess_smooth3'
      
      no_of_cols = i
      y_new_per_subject = cbind(y_subj, y_augm1, y_augm2, y_augm3, y_augm4, y_augm5,
                                y_augm6, y_augm7, y_augm8, y_augm9, y_augm10)
    
      
      labels_new_per_subject = rep(classes[subj], no_of_cols)
      subjectcode_new_per_subject = rep(subject_code_augm[subj], no_of_cols)
      
      if (subj == 1) {
        
        y_out = y_new_per_subject
        labels_out = labels_new_per_subject
        subjects_out = subjectcode_new_per_subject
        method_strings = method_str
        
      } else {
        
        y_out = cbind(y_out, y_new_per_subject)
        labels_out = c(labels_out, labels_new_per_subject)
        subjects_out = c(subjects_out, subjectcode_new_per_subject)
        method_strings = c(method_strings, method_str)
        
        cat('         synthetic dataset size = ', dim(y_out), '\n')
        cat('           ... synthetic labels = ', length(labels_out), '\n')
        cat('           ... ... synthetic subject codes = ', length(subjects_out), '\n')
        
      }
      
    } # end of for loop
    # from 241 subject, we should now have 2651 synthetic subjects (11x)
    cat('\nIN THE END WE HAVE A DATASET with the size of = ', dim(y_out)[1], ' samples x', dim(y_out)[2], ' synthetic subjects')
    
    # TODO! works only for binary labels
    no_of_glaucoma_labels =  sum(labels_out %in% 'Glaucoma')
    cat('\n   with', no_of_glaucoma_labels, 'glaucoma labels (', round(100*(no_of_glaucoma_labels/length(labels_out)), digits = 2), '% of all the labels )') 
    
    # Check that nothing funky happened above
    if ((no_of_cols*dim(y)[1]) != (dim(y_out)[2])) {
      warning('We do not have correct number of synthetic data out?')
    }
    
    if (no_of_cols*dim(y)[1] != length(labels_out)) {
      warning('We do not have correct number of synthetic labels out?')
    }
    
    if (no_of_cols*dim(y)[1] != length(subjects_out)) {
      warning('We do not have correct number of synthetic subjects out?')
    }
  }
  
  list_out = list(y_out, labels_out, subjects_out, method_strings)
  names(list_out) = c('y', 'labels', 'subjectCodes', 'method_strings')
  
  return(list_out)
  
}


export.list.to.disk.for.deep.learning = function(t, y, err, classes, subject_codes,
                                                 time_col = 'time', pupil_col = 'pupil', error_col = 'error',
                                                 title_string = 'glaucoma_agematched',
                                                 path_out = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/for_deepLearning',
                                                 split_into_test_and_train = TRUE,
                                                 train_ratio = 0.8, seed_no = 12345,
                                                 export_augm = FALSE, methods_augm = NA,
                                                 test_file = NA, test_subjects = NA) {
  
  # Easier to read in the file so that 1st column is the class label (numeric), with each
  # row presenting one subject (i.e. UCR_TS_Archive_2015 format) so that the developed model
  # can be easily compared to existing time series classification methods of which many are
  # developed using the UCR_TS_Archive 2015 as the benchmark
  
  # Convert the class strings into numeric values
  # Split all the traces into fixed  train and test sets here
  if (split_into_test_and_train) {
    
    classes_unique = unique(classes)
    class_labels = classes
    train_indices = list()
    test_indices = list()
    
    set.seed(seed_no)
    classes_numeric = classes
    
    for (c in 1 : length(classes_unique)) {
      
      classes_numeric[classes_numeric== classes_unique[c]] = c-1
      
      indices = classes %in% classes_unique[c]
      classes_per_label = class_labels[indices]
      classes_numeric_per_label = classes_per_label
      classes_numeric_per_label[classes_numeric_per_label == classes_unique[c]] = c-1
      classes_numeric_per_label = as.integer(classes_numeric_per_label)
      
      subject_codes_per_class = subject_codes[indices]
      no_of_indices = sum(indices)
      
      
      
        # all the PLR traces of this class (e.g. control traces)
        y_temp = y[,indices]    
        rnd_indices = sample(1:no_of_indices, no_of_indices, replace=FALSE)
        
        index_limit = round(train_ratio*no_of_indices) # take 80% first indices to be the train set
        indices_train = rnd_indices[1:index_limit]
        indices_test = rnd_indices[(index_limit+1):no_of_indices]
        
        if ((length(indices_train) + length(indices_test)) != length(subject_codes_per_class)) {
          warning('Something went wrong as we lost subjects when splitting the set into train and test?\n
                  we have = ', length(subject_codes_per_class), ' subject codes, and:\n\t\t',
                  length(indices_train), ' train indices, and \n\t\t',
                  length(indices_test), ' test indices')
        }
        
        if (c == 1) {
          
          y_train = y_temp[,indices_train]
          y_test = y_temp[,indices_test]
          
          subj_code_train = subject_codes_per_class [indices_train] 
          subj_code_test = subject_codes_per_class [indices_test]
          
          classes_train = classes_numeric[indices][indices_train]
          classes_test = classes_numeric[indices][indices_test]
          
          train_indices = indices_train
          test_indices = indices_test
          
        } else {
          
          y_train = cbind(y_train, y_temp[,indices_train])
          y_test = cbind(y_test, y_temp[,indices_test])
          
          subj_code_train = c(subj_code_train, subject_codes_per_class[indices_train])
          subj_code_test = c(subj_code_test, subject_codes_per_class[indices_test])
          
          classes_train = c(classes_train, classes_numeric[indices][indices_train])
          classes_test = c(classes_test, classes_numeric[indices][indices_test])
          
          train_indices = c(train_indices, indices_train)
          test_indices = c(test_indices, indices_test)
          
        }
    }
    
    classes_train = as.numeric(classes_train)
    classes_test = as.numeric(classes_test)
    
    classes_numeric = as.integer(classes_numeric)
    
    # join the class label as the first column
    y_out = t(rbind(classes_numeric, y)) # all traces
    y_out_train = t(rbind(classes_train, y_train))
    y_out_test = t(rbind(classes_test, y_test))
    
    cat(paste0('   exporting the split to: ', path_out, '\n      3 files: all/train/test, for data = ', title_string))
    
  
 
  } else {
    
    cat('Exporting augmented dataset to disk\n')
    
    # When writing augmented data, not much needed
    y_out = NA
    
    # Train data
    classes_train = factors_augm
    cat('   cbind() train data and label\n') # not sure why this is so slow?
    y_out_train = cbind(cbind(c(classes_train)), t(y))
    subj_code_train = subject_codes
    cat('   ... TRAIN dataset size is =', dim(y_out_train), '\n')
    
    # Test data
    # We did not do anything for these, so let's read them from the disk
    y_out_test = read.csv(test_file, header = FALSE)
    subj_code_test= read.csv(test_subjects, header = FALSE)
    
    cat('   ... TEST dataset size is =', dim(y_out_test), '\n')
    
    # Time
    t = t(t)
    
    # All data combined, let's say you want to test some cross-validation
    # or otherwise do the splits on the fly. Now to make our lives easier, we use
    # fixed splits so we have an idea on what DNN architecture changes lead
    # to improved performance and what do not
    cat('   rbind() train data and test data\n')
    y_out = rbind(y_out_train, y_out_test)
    subject_codes = c(subj_code_train, subj_code_test)
    
    cat('When combining test and train sets, the dataset size is =', dim(y_out), '\n')
    
    colVector = unlist(methods_augm)
    write.table(colVector, 
                file = file.path(path_out, paste0(title_string, '_augmMethods.csv'), fsep = .Platform$file.sep),
                sep = ",", row.names=FALSE, col.names=FALSE)
    
  }
  
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
  list_out = list(y_out_train, train_indices, test_indices)
  names(list_out) = c('TrainData', 'TrainIndices', 'TestIndices')
  
  
  return(list_out)
  
}
