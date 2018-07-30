resample.and.trim.SERI2017 = function(path_in = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_long_final_resampled',
                                      pupil_col = 'pupil',
                                      simplify = TRUE) {
  
  # INIT --------------------------------------------------------------------
  
    library(reshape2)
    library(ggplot2)
    library(gridExtra)
    library(grid)
  
    # Define computer-specific paths
    paths = list()
    paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
    
    path_out_ML = file.path(path_in, 'machineLearning', fsep = .Platform$file.sep)
  
    files = list.files(path=path_in, pattern='*.csv', recursive=FALSE, full.names = TRUE)
    
    diagnosis = read.table(list.files(path=path_in, pattern='*.txt', recursive=FALSE, full.names = TRUE),
                         stringsAsFactors = FALSE, sep = '\t', header = TRUE)
    
    # check that files and codes match
    diagnosis = check.files.and.diagnosis.match(files, diagnosis)
  
  # IMPORT FILES ------------------------------------------------------------  
    
    blue_PLR = list()
    red_PLR = list()
    diff_PLR = list()
    
    # counters
    red_found = 1
    blue_found = 1
    diff_found = 1
    
    for (i in 1 : length(files)) {
    
      cat('.')
        
      file_in = files[i]
      just_filename = tail(strsplit(files[i], .Platform$file.sep)[[1]],1)
      code_in = strsplit(just_filename, '_')[[1]][1]
      color_in = strsplit(just_filename, '_')[[1]][2]
      
      data_in = read.csv(file_in, stringsAsFactors = FALSE)
      vector_in = data_in[[pupil_col]]
      
      if (identical(color_in, 'blue')) {
        blue_PLR[[blue_found]] = vector_in
        blue_found = blue_found + 1
      } else if (identical(color_in, 'red')) {
        red_PLR[[red_found]] = vector_in
        red_found = red_found + 1
      } else if (identical(color_in, 'diff')) {
        diff_PLR[[diff_found]] = vector_in
        diff_found = diff_found + 1
      } else {
        warning('PROBLEM with COLOR field of the input file = ', color_in)
      }
      
    }
    
    # Convert lists to matrices
    x = 1:length(vector_in)
    blue_PLR_mat = do.call(cbind, blue_PLR)
    red_PLR_mat = do.call(cbind, red_PLR)
    diff_PLR_mat = do.call(cbind, diff_PLR)
    
    # rename the labels
    diagnoses = diagnosis$diagnosis
    diagnoses[grepl('Normals_Control', diagnoses)] = 'Control'
    
    # Combine NTG and POAG
    if (simplify) {
      diagnoses[grepl('Early', diagnoses)] = 'Early'
      diagnoses[grepl('Moderate', diagnoses)] = 'Moderate'
      diagnoses[grepl('Severe', diagnoses)] = 'Severe'
    }
    
    # time vector
    # should match
    # TODO! verify that they actually match
    time_vector = data_in$time_onsetZero
    write.table(time_vector, file=file.path(path_out_ML, 'time_vector.csv', fsep = .Platform$file.sep), 
                row.names=FALSE, col.names=TRUE, sep = ',')
    
    # Add diagnoses as column headers
    dimnames(blue_PLR_mat) = list(time_vector, diagnoses)
    dimnames(red_PLR_mat) = list(time_vector, diagnoses)
    dimnames(diff_PLR_mat) = list(time_vector, diagnoses)
    
    # write to disk the pupil vectors
    write.table(blue_PLR_mat, file=file.path(path_out_ML, 'SERI2017_blue.csv', fsep = .Platform$file.sep), 
                row.names=FALSE, col.names=TRUE, sep = ',')
    write.table(red_PLR_mat, file=file.path(path_out_ML, 'SERI2017_red.csv', fsep = .Platform$file.sep), 
                row.names=FALSE, col.names=TRUE, sep = ',')
    write.table(diff_PLR_mat, file=file.path(path_out_ML, 'SERI2017_diff.csv', fsep = .Platform$file.sep), 
                row.names=FALSE, col.names=TRUE, sep = ',')
    
    # TODO read from these .csv without slow import
    

  # VISUALIZE ---------------------------------------------------------------

    # Compute stats
    blue_stats = stats.of.melted(mat_in = blue_PLR_mat, diagnoses, time_vector)
    red_stats = stats.of.melted(mat_in = red_PLR_mat, diagnoses, time_vector)
    diff_stats = stats.of.melted(mat_in = diff_PLR_mat, diagnoses, time_vector)
    
    # Plot
    p = list()
    p = plot.per.condition(p, i = 1, group = 'Blue', stats = blue_stats, time_vector)
    p = plot.per.condition(p, i = 4, group = 'Red', stats = red_stats, time_vector)
    p = plot.per.condition(p, i = 7, group = 'Diff (Blue-Red)', stats = diff_stats, time_vector)
    
    no_of_cols = 3
    do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
  
}

plot.per.condition = function(p, i=1, group = 'blue', stats, time_vector) {

  df = melt(stats, level = 1)
  colnames(df) = c('Pupil', 'Diagnosis', 'Stat')
  df$Stat[df$Stat == 1] = 'Mean'
  df$Stat[df$Stat == 2] = 'SD'
  df$Stat[df$Stat == 3] = 'n'
  
  # take subset
  mean_ind = grepl('Mean', df$Stat)
  
  # Create subset
  mean_subset = list()
  col_names = colnames(df)
  
  for (c in 1 : length(col_names)) {
    mean_subset[[col_names[c]]] = df[[col_names[c]]][mean_ind]
  }
  
  df_subset = data.frame(mean_subset)
  factors = levels(df_subset$Diagnosis)
  no_of_factors = length(factors)
  df_subset$Time = rep(time_vector, no_of_factors)
  
  # Create custom legend
  leg_str = list()
  for (f in 1 : no_of_factors) {
    leg_str = c(leg_str, paste0(factors[f], ' n = ', stats[[3]][[factors[f]]]))
  }
  leg_str = unlist(leg_str)
  
  # color blind palette
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # Plot the traces
  p[[i]] = ggplot(df_subset, aes(x = Time, y = Pupil, colour = Diagnosis)) +
    geom_line() +
    labs(title = group) +
    scale_color_manual(labels = leg_str, values=cbPalette)

  # Plot the maximum constrictions
  max_constr = stats[[4]]
  max_df = melt(max_constr)
  colnames(max_df) = c('Max_Constriction', 'Diagnosis')
  
  # p[[i+1]] = ggplot(max_df, aes(x = Diagnosis, y = Max_Constriction, colour = Diagnosis)) +
  #   geom_violin(scale = "count")
  
  p[[i+1]] = ggplot(max_df, aes(x = Diagnosis, y = Max_Constriction, fill = Diagnosis)) +
              geom_boxplot(alpha = 0.55) +
              scale_fill_manual(labels = leg_str, values=cbPalette)
  
  p[[i+2]] = ggplot(max_df, aes(Max_Constriction, color = Diagnosis)) +
              geom_density(alpha = 0.05) +
              scale_color_manual(labels = leg_str, values=cbPalette)
  
  return(p)
  
  
}

stats.of.melted = function(mat_in, diagnoses, time_vector) {

   uniq_diagnoses = unique(diagnoses)
   
   mean_out = list()
   SD_out = list()
   n_out = list()
   max_constriction_out = list()
   
   i1 = which.min(abs(time_vector - 0))
   i2 = which.min(abs(time_vector - round(0.99*tail(time_vector,1))))
   max_constr_indices = c(i1, i2)
   
   for (i in 1 : length(uniq_diagnoses)) {
     
     # subset of matrix
     indices = diagnoses %in% uniq_diagnoses[i]
     
     matrix_per_diagnosis = mat_in[,indices]
     
     max_constriction_out[[uniq_diagnoses[i]]] =
       apply(matrix_per_diagnosis[i1:i2,], 2, min, na.rm = TRUE)
     
     # check first for outliers
     # valid_indices = check.for.outliers(vector_per_diagnosis = max_constriction_out[[uniq_diagnoses[i]]])
     # matrix_per_diagnosis = matrix_per_diagnosis[,valid_indices]
     
     mean_out[[uniq_diagnoses[i]]] = rowMeans(matrix_per_diagnosis, na.rm = TRUE)
     SD_out[[uniq_diagnoses[i]]] = apply(matrix_per_diagnosis, 1, sd, na.rm = TRUE)
     
     n_out[[uniq_diagnoses[i]]] = dim(matrix_per_diagnosis)[2]
     
   }
   
   return(list(mean_out, SD_out, n_out, max_constriction_out))
   
   
 }

 
check.files.and.diagnosis.match = function(files, diagnosis) {
 
  source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/scripts/SERI2017_re_postprocess.R') 
  unique_codes = unique(get.subjectcodes.of.listing(files, field_out = 'subjectcode'))
  
  codes_diagnoses = unique(toupper(diagnosis$subject_code))
  
  missing = !(codes_diagnoses %in% unique_codes)
  which_missing = codes_diagnoses[which(missing)] # 013C
  found = !missing
  
  diagnosis = diagnosis[found,]
  return(diagnosis)
  
}


check.for.outliers = function(vector_per_diagnosis) {
  
  # use the max constriction for checking the outlier indices
  sd_of_values = sd(vector_per_diagnosis)
  outlier_ind = !(abs(vector_per_diagnosis) > sd_of_values*1.96)
  
  return(outlier_ind)
  
}