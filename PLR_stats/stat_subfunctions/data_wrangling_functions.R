split.bin.and.global.feats = function(derived_feats_names) {
  
  global_names_ind = as.logical(lapply(derived_feats_names, function(ch) grep('global', ch)))
  global_names_ind[is.na(global_names_ind)] = FALSE
  
  global_names = derived_feats_names[global_names_ind]
  bin_names = derived_feats_names[!global_names_ind]
  
  return(list(bin_names, global_names))
  
}


find.value.and.uncert.indices = function(df, feat_to_plot) {
  
  col_names = colnames(df)
  
  feat_found = lapply(col_names, function(ch) grep(feat_to_plot, ch))
  all_feat_indices = unlist(lapply(feat_found, function(ch) length(ch) != 0))
  all_uncert_indices = unlist(lapply(col_names, function(ch) grepl('Uncertainty', ch)))
  
  feat_uncert_indices = all_feat_indices & all_uncert_indices 
  feat_indices = all_feat_indices != feat_uncert_indices
  
  return(list(feat_indices, feat_uncert_indices))
  
}

remove.rows.with.all.values.NA = function(df_sub1, no_of_vars_to_check_for_NAs, verbose) {
  
  # NOTE!
  # no_of_vars - is not necessary equal to all columns
  sum_NA_per_row = rowSums(is.na(df_sub1))
  subject_contains_only_NAs = sum_NA_per_row == no_of_vars_to_check_for_NAs
  df_out <- df_sub1[!subject_contains_only_NAs,]
  
  
  if (verbose) {
    cat(' .. .. Now we have a total of',  dim(df_sub1)[1], 'subjects in our data frame ("df_sub1")\n')
    cat(' .. .. .. some of these ( n=', sum(subject_contains_only_NAs), ') were subjects that had entries in Master Data sheet without approved PLR trace (no _BR label)\n')
    cat(' .. .. .. ... which leaves us with a total of', dim(df_out)[1], 'subjects to analyze and plot\n')
  }
  
  
  return(df_out)
  
}



trim.df.with.tidyr = function(df_subset, feats_to_keep, bin_names, global_names,
                              grouping_variable = NA, fixed_variables = NA,
                              verbose = TRUE, check_for_empty_subjects = TRUE) {
  
  # https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
  # https://github.com/STAT545-UBC/Discussion/issues/492
  # https://uc-r.github.io/tidyr

  # list with values [[1]], and uncertainties [[2]] to keep
  to_keep = get.feat.names.for.color.with.uncertainty(feats_to_keep, bin_names, global_names, verbose)
  
  values = to_keep[[1]]
  uncert = to_keep[[2]]
  feat_vars = c(values, uncert)
  all_vars_to_keep = c(fixed_variables, feat_vars)
  
  # Do a subset with dplyr package
  options(warn = -1)
  df_sub1 <- select(df_subset, one_of(all_vars_to_keep))
  options(warn = 0)
  
  if (verbose) {
    cat('Removing features from data frame not needed for plotting\n')
    cat(' .. going from',  length(df_subset), 'variables to', length(df_sub1), 'variables\n')
  }
  
  # TODO! check if your desired featured is not found, e.g. in case of a typo!
  if (check_for_empty_subjects) {
    no_of_var_in_init = length(colnames(df_sub1))
    no_of_vars_to_check_for_NAs = no_of_var_in_init - length(fixed_variables)
    
    # remove all the rows that have NAs in all the columns
    df_sub2 = remove.rows.with.all.values.NA(df_sub1, no_of_vars_to_check_for_NAs, verbose)
    
  } else {
    df_sub2 = df_sub1
  }
  
  return(df_sub2)
  
}

convert.to.long.melt.format = function(df, grouping_variable) {
  
  # Unique values for grouping variable, i.e. how many different
  # diagnosis codes you have
  no_of_bins = length(unique(df[[grouping_variable]]))
  
  # Convert to "Long Format", i.e. melt the data.frame
  feat_df_melt = melt(df, id.vars=c(grouping_variable))
  
}

select.subset.from.list = function(list_traces, subject_codes_traces, data_frame_feats,
                                   parameters, settings, data_type,
                                   std_naming_of_cols, handpicked = NA) {
 
  # The desired factors to keep
  grouping_variable = parameters[['main_factor']]
  factor_keep_names = parameters[['factors_keep']][[grouping_variable]]
  
  cat(paste0('Keeping the trace with the following values in "', grouping_variable, '"\n'))
  cat(paste0('  ', factor_keep_names),'\n')
  
  # keep these variables (E.g. time, pupil and the error)
  vars_to_keep =  parameters[['traces']]
  list_trim = trim_variables_from_list(list_traces, vars_to_keep)
  
  if (identical(parameters[['traces']][3], "error_fractional")) {
    warning('You chose to use fractional errors, so we convert that to absolute error')
      list_trim[[3]] =  abs(list_trim[[3]] *  list_trim[[2]])
  }
  
  # rename to standard names
  names(list_trim) = std_naming_of_cols
  
  # match the correct subjects (of whose traces you had) from dataframe
  # subject_codes_trace
  master_indices = get.master.data.indices(data_frame_feats, subject_codes_traces)
  
  # Save feats to disk
  save.intermediate.feats.to.disk(data_frame_feats, master_indices, settings) 
  
  # get the grouping variables from Master data sheet matching the PLR traces
  all_grouping_vars = data_frame_feats[[grouping_variable]][master_indices]
  
  if (is.na(handpicked)) {
  
    # keep only the desired variables in the grouping variable
    out = keep.the.factors.only.in.list(list_trim, all_grouping_vars,
                                        grouping_variable, factor_keep_names)
      list_out = out[[1]]
      keep_indices = out[[2]]
    
  } else {
    
    out = handpick.subjects(list_trim, data_frame_feats, parameters, master_indices)
    
      list_out = out[[1]]
      keep_indices = out[[2]]
    
  }
  
  # grouping variables out
  grouping_vars_out = all_grouping_vars[keep_indices]
  
  # correspondence between the master data sheet and the returned list
  master_indices_out = master_indices[keep_indices]
    
  return(list(list_out, master_indices_out, grouping_vars_out))
    
  
}

handpick.subjects = function(list_trim, data_frame_feats, parameters, master_indices) {
  
  handpick_indices = data_frame_feats[[parameters[['handpick_subjects']][['Column']]]]
  handpick_indices[is.na(handpick_indices)] = 0
  handpick_indices = as.logical(handpick_indices)
  
  no_of_handpicked_subjets = sum(handpick_indices)
  handpicked_subjects = data_frame_feats$`Subject code`[handpick_indices]
  all_kept_subject_codes = data_frame_feats$`Subject code`[master_indices]
  
  # match this to the found traces
  trimmed_indices = handpick_indices[master_indices]
  no_of_handpicked_subjets_left = sum(trimmed_indices)
  handpicked_subjects_left = all_kept_subject_codes[trimmed_indices]
  
  not_left = which(!(handpicked_subjects %in% handpicked_subjects_left))
  cat('HANDPICKED COLUMN: ', parameters[['handpick_subjects']][['Column']], '\n')
  cat('These subjects did not have traces recorded with _BR:\n')
  cat(handpicked_subjects[not_left])
  
  keep_indices = which(all_kept_subject_codes %in% handpicked_subjects_left)
  
  col_names = names(list_trim)
  list_out = list()
  for (i in 1 : length(col_names)) {
    mat = list_trim[[col_names[i]]]
    list_out[[col_names[i]]] = mat[,keep_indices]
  }
  
  
  return(list(list_out, keep_indices))
}



age.match.groups = function(list_out, master_indices_out, grouping_vars_out,
                                 main_factor = 'Diagnosis', 
                                 match_reference = c('POAG', 'PACG', 'NTG', 'DISC SUSPECT'),
                                 match_threshold = 6, 
                                 data_frame_feats) {
  

  # THE INIT (Boring Part) --------------------------------------------------

    cat('\n')
    cat('Age-matching the "main_factor" (', main_factor, 
        ') to the ages of "match_reference" (', match_reference, ')\n')
    
    ages_left_vector = data_frame_feats[['Age']][master_indices_out]
    
    # find indices corresponding to the match_reference
    ref_indices = grouping_vars_out %in% match_reference
    ref_ages = ages_left_vector[ref_indices]
    ref_NA_ages = is.na(ref_ages)
    ref_NA_count = sum(ref_NA_ages)
    ref_mean = round(mean(ref_ages, na.rm = TRUE), digits = 2)
    ref_SD = round(sd(ref_ages, na.rm = TRUE), digits = 2)
  
    cat(paste0('  Reference group had ', length(ref_ages), 
               ' subjects (mean +/- SD): ', ref_mean, ' +/- ', ref_SD, ' years\n'))
    
    if (ref_NA_count > 0) {
      cat('    .. The following', ref_NA_count, 'subjects had no age entered in Master Data sheet:\n')
      cat('    .. .  ', data_frame_feats$`Subject code`[which(ref_NA_ages)], '\n')
    }
    
    # Indices for the remaining subjects
    control_indices = !ref_indices
    control_ages = ages_left_vector[control_indices]
    control_NA_ages = is.na(control_ages)
    control_NA_count = sum(control_NA_ages)
    control_mean = round(mean(control_ages, na.rm = TRUE), digits = 2)
    control_SD = round(sd(control_ages, na.rm = TRUE), digits = 2)
    
    cat(paste0('   CONTROL group had ', length(control_ages), 
               ' subjects (mean +/- SD): ', control_mean, ' +/- ', control_SD, ' years\n'))
    
    if (ref_NA_count > 0) {
      cat('    .. The following', control_NA_count, 'subjects had no age entered in Master Data sheet:\n')
      cat('    .. .  ', data_frame_feats$`Subject code`[which(control_NA_ages)], '\n')
    }
    
    # Merge now the groups into a data frame
    # Combine POAG/NTG -> Glaucoma
    if (combine_pathology) {
      factors_in = combine.pathologies(factors_in = grouping_vars_out, 
                                       factors_kept = parameters[['factors_keep']][[parameters[['main_factor']]]])
    }
    
    labels_ref = factors_in[ref_indices]
    labels_control = factors_in[control_indices]
    
    sex_vector = data_frame_feats$Gender[master_indices_out]
    sex_vector[sex_vector == 1] = 'Male'
    sex_vector[sex_vector == 2] = 'Female'
    sex_ref = sex_vector[ref_indices]
    sex_control = sex_vector[control_indices]
    
    race_vector = data_frame_feats$Race[master_indices_out]
    race_ref = race_vector[ref_indices]
    race_control = race_vector[control_indices]
    
    df_ref = data.frame(Age = ref_ages, Pathology = labels_ref, Sex = sex_ref, Race = race_ref)
    df_control = data.frame(Age = control_ages, Pathology = labels_control, Sex = sex_control, Race = race_control)
    df_ages = rbind(df_ref, df_control)
    
    cat('Summary before any matching:\n')
    cat('----------------------------\n')
    summary(df_ages)

  # PROPENSITY SCORE --------------------------------------------------------

    # https://www.r-bloggers.com/how-to-use-r-for-matching-samples-propensity-score/
    # https://pareonline.net/getvn.asp?v=19&n=18
    # https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf
    # match.it <- matchit(Pathology ~ Age + Sex, data = df_ages, method = "optimal", ratio = 2) (getting an error?)
    
    # propensity score matching (PSM)
    # https://stats.stackexchange.com/questions/204561/matched-data-paired-t-test-vs-indpendent
    
      # "Autsin (2009) argues that you should account for the paired nature of the matched data. 
      # Others disagree. It is still an area of debate. The controversy is reported in Thoemmes & Kim (2011)."
      # http://dx.doi.org/10.2202/1557-4679.1146
      # http://dx.doi.org/10.1080/00273171.2011.540475
      
    # https://dx.doi.org/10.1016%2Fj.bbmt.2015.12.005 :
    
      # Our investigation shows that Cox regression model applied to the entire cohort is often a more powerful tool 
      # in detecting treatment effect as compared to a matched study. 
        
    # Demonstration Propensity Scores
    # How to conduct propensity scores in R | 
    # Steps that we follow when conducting propensity scores analysis
    # https://portfolio.du.edu/downloadItem/316002
    
    # TODO t-test or something
    limit_lo = round(ref_mean - match_threshold)
    limit_hi = round(ref_mean + match_threshold)
    
    indices_over_lo = ages_left_vector >= limit_lo
    no_over_lo = sum(indices_over_lo == TRUE)
    indices_under_hi = ages_left_vector <= limit_hi
    no_under_hi = sum(indices_under_hi == TRUE)
    
    # these are all ages within the defined range from both reference and main factor
    indices_in_range =  indices_over_lo & indices_under_hi
    no_in_range = sum(indices_in_range == TRUE)
    inrange_all_ages = ages_left_vector[indices_in_range]
    groupvars_left = grouping_vars_out[indices_in_range]

  # SELECT THE MATCHED SUBJECTS ---------------------------------------------

    # Now we get indices for main factor (e.g. CONTROL) group
    inrange_and_inMainFactor_group = !ref_indices & indices_in_range
    no_inMainFactor = sum(inrange_and_inMainFactor_group == TRUE)
    groupvars_left_now = grouping_vars_out[inrange_and_inMainFactor_group]
    
    ages_CONTROL_vector = ages_left_vector[inrange_and_inMainFactor_group]
    ages_CONTROL_mean = round(mean(ages_CONTROL_vector, na.rm = TRUE), digits = 2)
    ages_CONTROL_SD = round(sd(ages_CONTROL_vector, na.rm = TRUE), digits = 2)
    
    cat(paste0('    In the end we kept ', length(groupvars_left_now), 
               ' subjects from CONTROL (mean +/- SD): ', 
               ages_CONTROL_mean, ' +/- ', ages_CONTROL_SD, ' years\n'))
    
    # The indices in the end to keep
    to_keep_finally = ref_indices | inrange_and_inMainFactor_group
    
    list_names_in = names(list_out)
    list_agematched = list()
    for (i in 1 : length(list_names_in)) {
      list_agematched[[list_names_in[i]]] = list_out[[list_names_in[i]]][,to_keep_finally]
    }
    
    control_subjects_excluded = dim(list_out[[list_names_in[i]]])[2] -
                                dim(list_agematched[[list_names_in[i]]])[2]
                                
    cat('to SUMMARIZE: ', control_subjects_excluded, 'Control subjects were excluded\n')
    # cat('  ', ages_left_vector[!to_keep_finally])
    # cat('from following groups (ALL SHOULD BE CONTROL):\n', grouping_vars_out[!to_keep_finally], '\n')
    
    # grouping variables out
    grouping_vars_out = grouping_vars_out[to_keep_finally]
    
    # correspondence between the master data sheet and the returned list
    master_indices_out = master_indices_out[to_keep_finally]
    
    return(list(list_agematched, master_indices_out, grouping_vars_out))
  
  
}

get.master.data.indices = function(data_frame_feats, subject_codes_traces) {
  
  cat('\n')
  cat('Getting corresponding indices from Master Data sheet to the recorded PLR traces\n')
  
  all_subject_codes = data_frame_feats$`Subject code`
  all_indices = 1:length(all_subject_codes)
  
  unique_codes = unique(data_frame_feats$`Subject code`)
  no_of_unique_codes = length(unique(data_frame_feats$`Subject code`))
  
  first_unique_indices = match(unique(all_subject_codes), all_subject_codes)
  duplicate_indices = !(all_indices %in% first_unique_indices)
  duplicate_codes = all_subject_codes[duplicate_indices]
  
  # Now NA subject codes are counted as unique as well
  na_indices = is.na(duplicate_codes)
  number_of_NA_rows = sum(is.na(duplicate_codes))
  
  # Non-NA duplicates left
  duplicates_left = duplicate_codes[!na_indices]
  no_of_duplicates_left = length(duplicates_left)
  
  cat('  . number of subject codes (well rows from that column) found from Excel sheet: ', length(all_subject_codes), '\n')
  cat('  . . of which only', no_of_unique_codes, 'are unique, due to:\n')
  cat('  . . *', number_of_NA_rows, ' NA rows\n')
  cat('  . . *', no_of_duplicates_left, ' duplicate rows for the following subjects:\n')
  cat('   ')
  cat(duplicates_left, '\n\n')
  cat('  NOTE!', 'the information now was taken from the first instance of given subject code!\n\n')
  
  unique_trace_codes = unique(subject_codes_traces)
  no_of_unique_trace_codes = length(unique_trace_codes)
  # cat('  . . ', no_of_unique_trace_codes, 'of Unique subject codes were found from reconstruced PLR trace folder\n')

  # Keep track of non-idealities in matching
  PLR_found_ind = vector()
  twice_in_Master = vector()
  
  indices = vector()
  for (i in 1:length(subject_codes_traces)) {
    
    subj_code_of_trace = subject_codes_traces[i]
    match_ind = which(all_subject_codes %in% subj_code_of_trace)
    length_of_indices = length(match_ind)
    
    # check for problems
    if (length_of_indices == 0) {
      # cat('    - Subject = "', subj_code_of_trace, '" had a PLR trace, but it was not found from Master data sheet?\n')
      PLR_found_ind = c(PLR_found_ind, i)
      
    } else if (length_of_indices > 1) {
      
      # cat('    - You have the subject = "', subj_code_of_trace, '" ', length_of_indices, ' times in the Master data sheet? Keeping only the first one now\n\n')
      twice_in_Master = c(twice_in_Master, i)
      match_ind = match_ind[1]
    }
    
    indices = c(indices, match_ind)
    # cat(subj_code_of_trace, '\t')
    # cat(match_ind, '\n')
    
  }
  
  if (length(PLR_found_ind) > 0) {
    cat('There were', length(PLR_found_ind), 'PLR traces recorded that did not have associated entry in Master Data Sheet:\n')  
    cat('   ')  
    cat(subject_codes_traces[PLR_found_ind],'\n')  
  }
  
  if (length(twice_in_Master) > 0) {
    cat('There were', length(twice_in_Master), 'duplicate entries in the Master Data Sheet _FOR_ the recorded PLR traces:\n')  
    cat('   ')  
    cat(subject_codes_traces[twice_in_Master],'\n')  
    cat('   .. in other words,', no_of_duplicates_left-length(twice_in_Master), 
        'of the above found duplicates (n=', no_of_duplicates_left, ') did not have PLR trace yet\n')  
  }
  
  cat('\n')
  cat('SUMMARY: We found a total of', length(indices), 'PLR traces with associated Master data sheet trace\n')
  cat('         and we could not use', no_of_unique_trace_codes-length(indices), 'PLR traces due to missing Master sheet entries\n')
  
  return(indices)
  
}

keep.the.factors.only.in.list = function(list_trim, all_grouping_vars,
                                         grouping_variable, factor_keep_names) {
  
  cat('\n')
  cat('Getting rid now of the traces that are not given in "parameters[["factors_keep"]][[grouping_variable]]":\n')
  cat('  ', factor_keep_names, '\n')
  
  # Case INSENSITIVE matching
  keep_indices = which(tolower(all_grouping_vars) %in% tolower(factor_keep_names))
    # TODO! Add warning for mixed cases!
  col_names = names(list_trim)
  list_out = list()
  
  for (i in 1:length(col_names)) {
    col_name = col_names[i]
    matrix_trimmed = list_trim[[col_name]][,keep_indices]
    list_out[[col_name]] = matrix_trimmed
  }
  
  cat('    ... which left us with', length(keep_indices), 'PLR traces\n')
  return(list(list_out, keep_indices))
  
}

trim_variables_from_list = function(list_traces, vars_to_keep) {
  
  # get corresponding indices
  names_of_list = names(list_traces)
  indices_to_keep = which(names_of_list %in% vars_to_keep)
  
  cat('   Picking the following column as the y-variable to be plotted: "', vars_to_keep[2], '"', '\n')
  cat('      .. you could have chosen the following as well:', '\n')
  cat('          ', names_of_list, '\n')
  cat('\n')
  
  cat('    reducing the number of columns (variables), from', length(names_of_list), 'to', length(indices_to_keep), '\n')
  cat('      - following were left: ', vars_to_keep, '\n')
  
  list_trim = list() # TODO! more elegant way
  for (i in 1:length(indices_to_keep)) {
    column_name = names_of_list[[indices_to_keep[i]]]
    list_trim[[column_name]] = list_traces[[column_name]]
  }
  
  cat(paste0('         .. dimensions of matrices (n=', length(indices_to_keep),'): ', 
             dim(list_trim[[1]])[1], ' samples, and ', dim(list_trim[[1]])[2], ' subjects\n'))
  
  return(list_trim)
  
}

split.into.groups.by.grouping = function(list_to_split, grouping_vars_out) {
  
  unique_groups = unique(toupper(grouping_vars_out))
  no_of_unique_groups = length(unique_groups)
  
  cat('\n')
  cat('Splitting the data matrices with all the groups in one to matrices per grouping ("',  unique_groups , '")\n')
  
  col_names = names(list_to_split)
  
  no_of_subj = length(list_to_split[[col_names[1]]][1,])
  no_of_timepoints = length(list_to_split[[col_names[1]]][,1])
  
  time_res = list_to_split[['time']][2,1] - list_to_split[['time']][1,1]
  fps = round(1 / time_res, digits=0)
  
  cat(' . from', no_of_subj, 'subjects each having', no_of_timepoints, 'samples per trace (recorded at',  fps, 'fps)\n')
  
  list_temp = list()
  for (subj in 1:no_of_subj) {
    
    group_to_add = grouping_vars_out[subj]
    
    for (col in 1:length(col_names)) {
      
      col_name = col_names[[col]]
      
      # We might have "DISC SUSPECT" and "Disc Suspect" so we save in uppercase
      group_name_out = toupper(group_to_add)
      
      list_temp[[group_name_out]][[col_name]] = 
        cbind(list_temp[[group_name_out]][[col_name]], list_to_split[[col_name]][,subj])
      
    }
  }
  
  # Count the occurrences per group
  cat(' . . split into following groups, each containing', length(col_names), 'variables (', col_names, '):\n')
  group_names = names(list_temp)
  sum_n = 0
  for (gr in 1 : length(group_names)) {
    cat(paste0(' . . . ', group_names[gr],': n = ', dim(list_temp[[group_names[gr]]]$time)[2], ' subjects\n'))
    sum_n = sum_n + dim(list_temp[[group_names[gr]]]$time)[2]
  }
  
  if (sum_n != no_of_subj) {
    warning('Somehow you lost subjects when splitting, there is a bug somewhere now!')
    cat('Output list has only', sum_n, 'subjects, whereas in the input there were',
        no_of_subj, 'subjects')
  }
  
  return(list_temp)
  
}

calculate.the.stats.of.grouped.lists = function(grouped_list, std_naming_of_cols) {
   
  cat('\n')
  cat('Calculating the stats of these grouped lists\n')
  cat(' - Assuming that all the traces in the matrix have the same time vector (within the group)\n')
  
  no_of_groups = length(grouped_list)
  stats = list()
  list_temp_to_df = list()
  group_names = names(grouped_list)
  
  time_col = std_naming_of_cols[1]
  pupil_col = std_naming_of_cols[2]
  error_col = std_naming_of_cols[3]
  
  for (i in 1 : no_of_groups) {
   
    # TIME
    time_vector = grouped_list[[group_names[i]]][[time_col]][,1] # same time vector for all of the subjects
    no_of_subj = length(grouped_list[[group_names[i]]][[time_col]][1,])
    no_of_timep = length(grouped_list[[group_names[i]]][[time_col]][,1])
    stats[[group_names[i]]]['n'] = no_of_subj
    
    # PUPIL MATRIX
    temp_list = grouped_list[[group_names[i]]][[pupil_col]]
    pupil_matrix_per_group = matrix(unlist(temp_list), ncol= no_of_subj, byrow = FALSE)
    
      # Mean trace of all the subjects
      stats_vectors_pupilMean = rowMeans(pupil_matrix_per_group, na.rm = TRUE)
      
      # STDEV of all those subjects' means
      stats_vectors_pupilMeanStdevs = apply(pupil_matrix_per_group, 
                                            1, sd, na.rm = TRUE)
    
    # TODO! 
    # ERROR MATRIX
    # Now each subject has a stdev per each time point as well, and check that 
    # this is good actually
      
    # ERROR
    # http://www.nist.gov/itl/sed/training/upload/combine-1.pdf
    # "The variance of the sums is the sum of the variances"
    # https://stats.stackexchange.com/questions/21104/calculate-average-of-a-set-numbers-with-reported-standard-errors
    temp_list = grouped_list[[group_names[i]]][error_col]
    error_matrix_per_group = matrix(unlist(temp_list), ncol= no_of_subj, byrow = FALSE)
    variance_matrix_per_group = error_matrix_per_group^2
    variance_sum = apply(pupil_matrix_per_group, 1, sum, na.rm = TRUE)
    
    stats_vectors_error_mean = variance_sum / no_of_subj^2
    
    # Mean of SD's per time point (TODO!)
    # stats_vectors_pupilErrorMean = rowMeans(error_matrix_per_group, na.rm = TRUE)
    # Add together with the pupilMeanSTDevs Later
    
    # The total error?
    stats_vectors_stdev_total = sqrt(stats_vectors_pupilMeanStdevs^2 +
                                      stats_vectors_error_mean^2)
    
    # OUTPUT
    cat('  .. .',  group_names[i], ': n =', dim(pupil_matrix_per_group)[2], '\n')
    list_temp_to_df[[group_names[i]]][[time_col]] = time_vector
    list_temp_to_df[[group_names[i]]]$pupil = stats_vectors_pupilMean
    list_temp_to_df[[group_names[i]]]$error = stats_vectors_stdev_total
    list_temp_to_df[[group_names[i]]]$stdev_of_means = stats_vectors_pupilMeanStdevs
    list_temp_to_df[[group_names[i]]]$mean_of_errors = stats_vectors_error_mean
    
  }
  
  return(list(list_temp_to_df, stats))
}

long.df.stats.from.list = function(vector_stats) {
  
  no_of_groups = length(vector_stats)
  col_names = names(vector_stats)
  
  cat('\n')
  cat('Converting the list with', no_of_groups, 'entries (', col_names, ') into a long-format ("indexed") data frame for "ggplot"\n')
  
  # http://www.evanpickett.com/blog/2015/7/3/organising-data-for-graphs-in-r
  for (i in 1:no_of_groups) {
    
    # Covert the list element into a data frame
    group_name = col_names[i]
    entry_per_group = vector_stats[[group_name]]
    df_per_group = data.frame(entry_per_group)
    
    # Add new entries
    df_per_group[['group']] = rep(group_name, dim(df_per_group)[1])
    
    # Sort of placeholder if you have asymmetric errors
    df_per_group[['lo']] = entry_per_group$pupil - entry_per_group$error
    df_per_group[['hi']] = entry_per_group$pupil + entry_per_group$error
  
    # https://stackoverflow.com/questions/34420211/convert-long-to-wide-format-with-two-factors-in-r
    if (i == 1) {
      df_melt_per_group = melt(df_per_group, id=c('time', 'group'))
    } else {
      df_melt_per_group = rbind(df_melt_per_group, melt(df_per_group, id=c('time', 'group')))
    }
  }
  
  factor_names = levels(as.factor(df_melt_per_group$variable))
  group_names = unique(df_melt_per_group$group)
  
  cat('  .. We now have', dim(df_melt_per_group)[1], 'observations (rows) in long format\n')
  cat('  .. . with', length(factor_names), 'different factors ($variable): [', factor_names, ']\n')
  cat('  .. . . and', length(group_names), 'different groups ($group_names): [', group_names, ']\n')
  cat('  .. . . . in other words:', dim(df_melt_per_group)[1], 'rows / (', length(group_names), 'x', length(factor_names), 
      ') =', length(entry_per_group$pupil), ' samples per PLR trace\n')
  
  names(df_melt_per_group$variable)
  
  return(df_melt_per_group)
  
}

stats.df.from.list = function(vector_stats) {
  
  no_of_groups = length(vector_stats)
  col_names = names(vector_stats)
  
  df_stat_means = data.frame(time = vector_stats[[col_names[1]]]$time)
  df_stat_errors = data.frame(time = vector_stats[[col_names[1]]]$time)
  df_stats = data.frame(time = vector_stats[[col_names[1]]]$time)
  
  for (i in 1:no_of_groups) {
    
    # means and errors to their own dfs
    df_stat_means[[col_names[i]]] = vector_stats[[col_names[i]]]$pupil
    df_stat_errors[[col_names[i]]] = vector_stats[[col_names[i]]]$error
    
    # all variables to same df
    df_stats[[col_names[i]]] = vector_stats[[col_names[i]]]$pupil
    df_stats[[paste(col_names[i], '_error')]] = vector_stats[[col_names[i]]]$error
    df_stats[[paste(col_names[i], '_hi')]] = vector_stats[[col_names[i]]]$pupil +
                                             vector_stats[[col_names[i]]]$error
    df_stats[[paste(col_names[i], '_lo')]] = vector_stats[[col_names[i]]]$pupil -
                                             vector_stats[[col_names[i]]]$error
    
  }
  
  return(list(df_stat_means, df_stat_errors, df_stats))
  
  # df_means = stats_df_out[[1]]
  # df_errors = stats_df_out[[2]]
  # df_stats = stats_df_out[[3]]
  
}

select.subset.from.df = function(data_frame_feats, parameters, settings, data_type) {
  
  # See e.g. 
  # https://stackoverflow.com/questions/4935479/how-to-combine-multiple-conditions-to-subset-a-data-frame-using-or
  
  # The desired factors to keep
  factor_keep_names = names(parameters[['factors_keep']])
  
  # The desired matching variables
  match_names = names(parameters[['matched_by']])
  
  # The desired factors to plot
  factor_names = names(parameters[['factors']])
  
  # Trim based on factors keep
  cat(paste0('First getting rid of the subjects that do not have anything on column: "', factor_keep_names, '" \n'))
  df_keep = trim.df.from.factor.keep(data_frame_feats, parameters, factor_keep_names)
  cat(paste0('  .. which was the case for ', dim(data_frame_feats)[1] - dim(df_keep)[1], ' subjects\n'))
  cat(paste0('  .. .. we have now a total of ', dim(df_keep)[1], ' subjects left at this point\n'))
  
  # Match the data with some variable (typically age)
  df_matched = match.df(df_keep, parameters, match_names)
  
  return(df_matched)
  
  
}

trim.df.from.factor.keep = function(df_in, parameters, factor_keep_names) {
  
  # https://stackoverflow.com/questions/2125231/subsetting-in-r-using-or-condition-with-strings
  sub = list()
  for (factor_no in 1 : length(factor_keep_names)) {
    
    # On first pass getting rid of all the non-matching subject
    # --> Second pass, get rid of those based on some criteria
    factor_name = factor_keep_names[[factor_no]]
    
    # TODO! You need special magic here if you want to combine different names
    # e.g. "Minimal DR" and "Mild NPDR" for example mean the same thing in this
    # contect. You probably have to do some manually defined look-up table function 
    # here, and for example rename the all the variations to same name to make this 
    # part a bit easier
    values_to_keep = parameters[['factors_keep']][[factor_name]]
    
    if (factor_no == 1) {
      sub[[factor_no]] = subset(df_in, df_in[[factor_name]] %in% 
                                  values_to_keep, drop = TRUE)
      
    } else if (factor_no > 1) {
      sub[[factor_no]] = subset(sub[[factor_no-1]], sub[[factor_no-1]][[factor_name]] %in% 
                                  values_to_keep, drop = TRUE) 
    }
  }
  
  sub_df = data.frame(sub[[factor_no]])
  return(sub_df)
}

match.df = function(df_keep, parameters, match_names) {
  
  # This is e.g. "AD", 'list of glaucomas) so if these are all 60 yrs old,
  # you want to find the matching ages from your controls
  reference_variables = parameters[['match_reference']]
  
  cat('\n')
  cat('Age-matching your groups based on the ages from the following "',  
      parameters[['main_factor']], '" :',  reference_variables, '\n')
  
  # quick'n'dirty init to illustrate the idea
  primary_groups = parameters[['factors_keep']][[parameters[['main_factor']]]]
  
  # keep only those rows that match the criteria for determining
  # the desired "matched by"
  # TODO! Use the age-matching function
  main_factor = parameters[['main_factor']] # e.g. diagnosis
  keep_indices = df_keep[[main_factor]] %in% reference_variables
  no_of_kept_rows = sum(keep_indices == TRUE)
  
  match_means = vector(, length=length(match_names))
  nonkept_means = vector(, length=length(match_names))
  
  match_no = 1 # if we had more than one matching factor
               # e.g. we also wanted the same sex and same age
               # TODO add a loop
  
  match_variable =  names(parameters[['matched_by']])[[match_no]]
  match_vector_all = df_keep[[match_variable]]
  
  match_vector_kept = match_vector_all[keep_indices]
  match_vector_nonkept = match_vector_all[!keep_indices]
  # now "match_vector_kept" contains all the age values of your "reference_variables"
  # i.e. what are the ages of the subjects in all of the glaucoma conditions
  
  match_means[match_no] = mean(match_vector_kept, na.rm = TRUE) # ~65.7 yrs
  
  cat(' .. of which  ( n=', length(match_vector_kept),
      ') the average ', match_variable, ' is = ', match_means[match_no], ' years\n')
  
  nonkept_means[match_no] = mean(match_vector_nonkept, na.rm = TRUE) # ~50.6 yrs
  cat(' .. .. whereas the rest of the subjects ( n=', length(match_vector_nonkept),
      ') have average ', match_variable, ' of = ', nonkept_means[match_no], ' years\n')
  
  threshold = parameters[['matched_by']][match_variable]
  limit_lo = round(match_means[match_no] - threshold)
  limit_hi = round(match_means[match_no] + threshold)
  
  indices_over_lo = match_vector_all >= limit_lo
  no_over_lo = sum(indices_over_lo == TRUE)
  indices_under_hi = match_vector_all <= limit_hi
  no_under_hi = sum(indices_under_hi == TRUE)
  
  indices_in_range =  indices_over_lo & indices_under_hi
  no_of_subjects_in_range = sum(indices_in_range == TRUE)
  
  values_in_range = match_vector_all[indices_in_range]
  
  # now we keep all from the "references" (e.g. the glaucomas)
  indices_in_range_and_non_reference = !keep_indices & indices_in_range
  no_of_subjects_in_nonkept = sum(indices_in_range_and_non_reference == TRUE)
  
  final_non_reference_vector = match_vector_all
  nonkept_means_2 = mean(final_non_reference_vector[indices_in_range_and_non_reference], 
                         na.rm = TRUE) # ~66 yrs
  
  cat(' .. .. .. after age-matching, the rest of the subjects ( n=', no_of_subjects_in_nonkept,
      ') have average ', match_variable, ' of = ', nonkept_means_2, ' years\n')  
  
  # So in the end we want to keep all the people in the "reference_variables", i.e. the glaucoma people
    # keep_indices
  
  # And the age-matched non-"reference_variables" people, e.g. from Control
    # indices_in_range_and_non_reference
  
  to_keep_indices = keep_indices | indices_in_range_and_non_reference
  no_of_subjects_to_keep_in_the_end = sum(to_keep_indices == TRUE)
  
  # TODO! 
  # Add some t-test here to determine the cutoff from control group
  
  df_keep = df_keep[to_keep_indices,]
  
  return(df_keep)
}

keep.only.the.features.of.interest = function(df_subset, feats_to_keep, 
                                              bin_names, global_names, 
                                              derived_feats_names) {
  
  col.names = colnames(df_subset)
  
  # get the corresponding names with the color prefix, and the uncertainty suffix
  to_keep = get.feat.names.for.color.with.uncertainty(feats_to_keep, bin_names, global_names)
  
  # Now if we wanted to have e.g. 3 features to be plotted, then we would
  # have 6 + 6 variables here
  to_keep_values = is.na(match(col.names, to_keep[[1]]))
  to_keep_uncert = is.na(match(col.names, to_keep[[2]]))
  
  values_to_keep = col.names[!to_keep_values]
  uncert_to_keep = col.names[!to_keep_uncert]
  
  # And now actually, we keep the vectors as defined in the Master Excel Sheet
  # and only trim away the names from the derived_feats_names (i.e. the features
  # computed in the PLR_analysis)
  value_indices_to_drop = is.na(match(derived_feats_names, values_to_keep))
  uncert_indices_to_drop = is.na(match(derived_feats_names, uncert_to_keep))
  
  to_drop = !(!value_indices_to_drop | !uncert_indices_to_drop)
  feats_to_drop = derived_feats_names[to_drop]
  
  df_dropped = df_subset[ , !(names(df_subset) %in% feats_to_drop)]
  
}

get.feat.names.for.color.with.uncertainty = function(feats_to_keep, bin_names, global_names, verbose) {
  
  # remove the "blue_" and "red_" prefixes
  bin_names2 = gsub("blue_", '', bin_names)
  bin_names2 = gsub("red_", '', bin_names2)
  
  # remove the "global_" prefix
  global_names2 = gsub("global_", '', global_names)
  
  # case insensitive matching
  feats_to_keep_bins_ind = which(tolower(bin_names2) %in% tolower(feats_to_keep))
  feats_to_keep_global_ind = which(tolower(global_names2) %in% tolower(feats_to_keep))
  
  feats_to_keep_bins = bin_names2[feats_to_keep_bins_ind]
  feats_to_keep_global = global_names2[feats_to_keep_global_ind]
  
  if (verbose) {
    cat('Trying to keep the following features = ', c(feats_to_keep_bins, feats_to_keep_global), '\n')
  }
  
  # now the feats to keep are given as they are in the "bins.csv", but now we might have
  # have blue_ or red_ in front of them
  feats_to_keep_red = paste('red_', feats_to_keep_bins, sep='')
  feats_to_keep_blue = paste('blue_', feats_to_keep_bins, sep='')
  
  # Added the global variables, calculated for whole recording
  feats_to_keep_global = paste('global_', feats_to_keep_global, sep='')
  
  feats_to_keep_values = c(feats_to_keep_red, feats_to_keep_blue, feats_to_keep_global)
  
  # now we have the uncertainties there as variables as well
  feats_to_keep_uncert = paste(feats_to_keep_values, '_Uncertainty', sep='')
  
  return(list(feats_to_keep_values, feats_to_keep_uncert))
  
}


feature.fields.to.list = function(df_trim, feat_in, vars_to_plot) {
  
  # find instances of this features
  ind = as.logical(lapply(vars_to_plot, function(ch) grep(feat_in, ch)))
  ind[is.na(ind)] = FALSE
  ind_linear = which(ind)
  
  # which names were found
  feature_names_found = vars_to_plot[ind_linear]
  
  # split into red, blue and global
  ind_red = grepl('red', feature_names_found)
  ind_blue = grepl('blue', feature_names_found)
  ind_global = grepl('global', feature_names_found)
  
  # split value and uncertainty
  value_indices = !grepl('Uncertainty', feature_names_found)
  
  # gets a bit heavy now, the index now refers to 
  # df_trim
  n = length(ind_linear)
  value_red_ind = if.ind.nonempty(ind_linear[value_indices & ind_red], n)
  uncert_red_ind = if.ind.nonempty(ind_linear[!value_indices & ind_red], n)
  value_blue_ind = if.ind.nonempty(ind_linear[value_indices & ind_blue], n)
  uncert_blue_ind = if.ind.nonempty(ind_linear[!value_indices & ind_blue], n)
  value_global_ind = if.ind.nonempty(ind_linear[value_indices & ind_global], n)
  uncert_global_ind = if.ind.nonempty(ind_linear[!value_indices & ind_global], n)
  
  # Determine who many subplots we get per feature
  # with red_blue, we get 3 subplots, and with "global"
  # just one
  list_feats = list()
  if (is.na(value_global_ind)) {
    no_subplots = 3
    list_feats[['blue']][['mean']] = df_trim[[value_blue_ind]]
    list_feats[['blue']][['uncert']] = df_trim[[uncert_blue_ind]] 
    list_feats[['red']][['mean']] = df_trim[[value_red_ind]] 
    list_feats[['red']][['uncert']] = df_trim[[uncert_red_ind]] 
    list_feats[['diff']][['mean']] = list_feats[['blue']][['mean']] - list_feats[['red']][['mean']]
    list_feats[['diff']][['uncert']] = sqrt(list_feats[['blue']][['uncert']]^2 
                                            + list_feats[['red']][['uncert']]^2)
  } else {
    no_subplots = 1
    list_feats[['global']][['mean']] = df_trim[[value_global_ind]] 
    list_feats[['global']][['uncert']] = df_trim[[uncert_global_ind]] 
  }
  
  return(list(list_feats, no_subplots))
  
}

if.ind.nonempty = function(boolean_indices, n) {
  
  if (length(boolean_indices) == 0) {
    indices_out = NA
  } else {
    indices_out = boolean_indices
  }
  return(indices_out)
  
}


save.intermediate.feats.to.disk = function(data_frame_feats, master_indices, settings) {

  # This gives which PLR traces had an associated master data sheet
  traces_found_from_master = master_indices
  
  # save to disk 
  cat('\n')
  filename_out = 'MasterMatched_combined_features.csv'
  cat('Saving the found', length(master_indices), 'entries to disk as .csv\n')
  df_masterMatched = data_frame_feats[traces_found_from_master,]
  export.pupil.dataframe.toDisk(df_masterMatched, 
                                filename_out, settings[['data_path_out']], 'feat_stats')
  cat('\n')
  
  # Save Control means as well for report generation purposes, so one can for 
  # example compare how new subjects compare to the "normatitative" data
  control_TRUE = df_masterMatched$Diagnosis %in% 'Control'
  df_masterMatched_control = df_masterMatched[control_TRUE,]
  
  isnumeric_col = sapply(df_masterMatched_control, is.numeric) 
  df_onlyNumeric = df_masterMatched_control[,isnumeric_col]
  names_onlyNumeric = colnames(df_onlyNumeric)
  control_means = colMeans(df_onlyNumeric, na.rm = TRUE)
  control_SDs = apply(df_onlyNumeric, 2, sd, na.rm = TRUE)
  
  means_out = list()
  SDs_out = list()
  for (i in 1 : length(names_onlyNumeric)) {
    SDs_out[[names_onlyNumeric[i]]] = control_SDs[i]
    means_out[[names_onlyNumeric[i]]] = control_means[i]
  }
  
  df_means_out = data.frame(means_out)
  df_SDs_out = data.frame(SDs_out)
  
  export.pupil.dataframe.toDisk(means_out, 
                                'Control_Group_features.csv', settings[['data_path_out']], 'mean')
  export.pupil.dataframe.toDisk(SDs_out, 
                                'Control_Group_features.csv', settings[['data_path_out']], 'SD')
  
}