combine.uncertainties.for.boxplot = function(values, uncerts, grouping_variable) {
  
  # values = df_trim_value
  # uncerts = df_trim_uncert
  
  # UNCERTAINTIES
  col_names = colnames(uncerts)
  to_excl = grepl(grouping_variable, col_names)
  
  # now you should have only 2 colors left
  color_names = col_names[!to_excl]
  
  ## TODO!!!
  # include the grouping variable
  ## HERE AS WELL!
  
  # RMS of the uncertainties of all the subjects
  rms = list()
  
  for (col in 1:length(color_names)) {
    
    col_name = color_names[[col]]
    group_vars = unique(uncerts[[grouping_variable]])
    
    for (group in 1 : length(group_vars)) {
      
      group_name = group_vars[[group]]
      
      df_subset_temp = subset(uncerts, uncerts[[grouping_variable]] == group_name)
      vector = df_subset_temp[[col_name]]

      squared_sum_uncerts = sum(vector^2)  
      mean_uncerts = squared_sum_uncerts / length(vector)
      
      rms[[group_name]][[col]] = sqrt(mean_uncerts)
        
    }
  }
  
  # VALUES
  
  # Stdev and median of the values
  col_names = colnames(values)
  to_excl = grepl(grouping_variable, col_names)
  
  # now you should have only 2 colors left
  color_names = col_names[!to_excl]
  
  values_stdev = list()
  for (col in 1:length(color_names)) {
    
    col_name = color_names[[col]]
    group_vars = unique(uncerts[[grouping_variable]])
    
    for (group in 1 : length(group_vars)) {
      
      group_name = group_vars[[group]]
      
      df_subset_temp = subset(values, values[[grouping_variable]] == group_name)
      vector = df_subset_temp[[col_name]]
      
      vector_mean = mean(vector, na.rm = TRUE)
      vector_median = median(vector, na.rm = TRUE)
      
      values_stdev[[group_name]][[col]] = sd(vector, na.rm = TRUE)
      
    }
    
  }
  
  # SQUARED COMBINATION
    
    errors = list()
    for (group in 1 : length(group_vars)) {
      group_name = group_vars[[group]]
      errors[[group_name]] = sqrt(rms[[group_name]]^2 + values_stdev[[group_name]]^2)  
    }
    
  return(errors)
}

trim.to.melt.for.the.feat = function(df, feat_to_plot, 
                                     grouping_variable) {
  
  col_names = colnames(df)
  
  group_found = match(grouping_variable, col_names)
  if (is.na(group_found)) {
    warning('You would like to group with variable = "', grouping_variable, '" but it was not found from data frame!')
  }
  
  indices_list = find.value.and.uncert.indices(df, feat_to_plot)
  feat_indices = indices_list[[1]]
  uncert_indices = indices_list[[2]]
  
  # combined indices
  indices = feat_indices | uncert_indices
  indices[group_found] = TRUE
  
  # separately 
  feat_indices_w_group = feat_indices
  feat_indices_w_group[group_found] = TRUE
  uncert_indices_w_group = uncert_indices
  uncert_indices_w_group[group_found] = TRUE
  
  df2 <- subset(df, select = feat_indices_w_group)
  
  df_trim_value = df[,feat_indices_w_group]
  df_trim_uncert = df[,uncert_indices_w_group]
  
  errors = combine.uncertainties.for.boxplot(df_trim_value, df_trim_uncert, 
                                             grouping_variable)
  
  df_melt_value = melt(df_trim_value, id.vars=c(grouping_variable))
  df_melt_uncert = melt(df_trim_uncert, id.vars=c(grouping_variable))
  
  return(list(df_melt_value, df_melt_uncert, errors))
  
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

remove.rows.with.all.values.NA = function(df, no_of_vars_to_check_for_NAs) {
  
  # NOTE!
  # no_of_vars - is not necessary equal to all columns
  sum_NA_per_row = rowSums(is.na(df))
  subject_contains_only_NAs = sum_NA_per_row == no_of_vars_to_check_for_NAs
  df_out <- df[!subject_contains_only_NAs,]
  
  return(df_out)
  
}

get.number.of.diff.features.for.plot = function(df, define_by, 
                                                grouping_variable) {
  
  if (identical(define_by, 'features')) {
    
    col_names = colnames(df)
    red_instances_found = lapply(col_names, function(ch) grepl("red", ch))
    # red_indices = lapply(red_instances_found, function(ch) length(ch) != 0)
    red_indices = red_instances_found
    
    red_instances = col_names[unlist(red_indices)]
    red_feats_ind = !(grepl('Uncertainty', red_instances))
    red_feats = red_instances[red_feats_ind]
    no_red_feats = length(red_feats)
    
    return(list(red_feats, no_red_feats))
  }
}

trim.df.with.tidyr = function(df_subset, feats_to_keep, 
                              grouping_variable, fixed_variables) {
  
  # https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
  # https://github.com/STAT545-UBC/Discussion/issues/492
  # https://uc-r.github.io/tidyr
  
  # list with values [[1]], and uncertainties [[2]] to keep
  to_keep = get.feat.names.for.color.with.uncertainty(feats_to_keep)
  
  values = to_keep[[1]]
  uncert = to_keep[[2]]
  feat_vars = c(values, uncert)
  all_vars_to_keep = c(fixed_variables, feat_vars)
  
  # Do a subset with dplyr package
  df_sub1 <- select(df_subset, one_of(all_vars_to_keep))
  # TODO! check if your desired featured is not found, e.g. in case of a typo!
  no_of_var_in_init = length(colnames(df_sub1))
  no_of_vars_to_check_for_NAs = no_of_var_in_init - length(fixed_variables)
  
  # remove all the rows that have NAs in all the columns
  df_sub2 = remove.rows.with.all.values.NA(df_sub1, no_of_vars_to_check_for_NAs)
  
  return(df_sub2)
  
}

convert.to.long.melt.format = function(df, grouping_variable) {
  
  # Unique values for grouping variable, i.e. how many different
  # diagnosis codes you have
  no_of_bins = length(unique(df[[grouping_variable]]))
  
  # Convert to "Long Format", i.e. melt the data.frame
  feat_df_melt = melt(df, id.vars=c(grouping_variable))
  
}

select.subset.from.list = function(list_in, subject_codes_traces,  data_frame_feats,
                                   parameters, settings, data_type) {
 
  # The desired factors to keep
  grouping_variable = parameters[['main_factor']]
  factor_keep_names = parameters[['factors_keep']][[grouping_variable]]
  
  # keep these variables (E.g. time, pupil and the error)
  vars_to_keep =  parameters[['traces']]
  list_trim = trim_variables_from_list(list_in, vars_to_keep)
  
  # match the correct subjects (of whose traces you had) from dataframe
  # subject_codes_trace
  str(subject_codes_traces)
  master_indices = get.master.data.indices(data_frame_feats, subject_codes_traces)
  
  # get the grouping variables from Master data sheet matching the PLR traces
  all_grouping_vars = data_frame_feats[[grouping_variable]][master_indices]
  
  # keep only the desired variables in the grouping variable
  out = keep.the.factors.only.in.list(list_trim, all_grouping_vars,
                                      grouping_variable, factor_keep_names)
  
    list_out = out[[1]]
    keep_indices = out[[2]]
  
  # grouping variables out
  grouping_vars_out = all_grouping_vars[keep_indices]
  
  # correspondence between the master data sheet and the returned list
  master_indices_out = master_indices[keep_indices]
  
  return(list(list_out, master_indices_out, grouping_vars_out))
  
}

get.master.data.indices = function(data_frame_feats, subject_codes_traces) {
  
  all_subject_codes = data_frame_feats$`Subject code`
  unique_codes = unique(data_frame_feats$`Subject code`)
  no_of_unique_codes = length(unique(data_frame_feats$`Subject code`))
  
  unique_trace_codes = unique(subject_codes_traces)
  no_of_unique_trace_codes = length(unique_trace_codes)
  
  # indices = which(all_subject_codes %in% subject_codes_traces)
  # TODO! Length out is 288?!?
  
  indices = vector()
  for (i in 1:length(subject_codes_traces)) {
    
    subj_code_of_trace = subject_codes_traces[i]
    match_ind = which(all_subject_codes %in% subj_code_of_trace)
    length_of_indices = length(match_ind)
    
    # check for problems
    if (length_of_indices == 0) {
      warning('Subject = "', subj_code_of_trace, '" had a PLR trace, but it was not found from Master data sheet?')
    } else if (length_of_indices > 1) {
      warning('You have the subject = "', subj_code_of_trace, '" ', length_of_indices, ' times in the Master data sheet? Keeping only the first one now')
      match_ind = match_ind[1]
    }
    
    indices = c(indices, match_ind)
    # cat(subj_code_of_trace, '\t')
    # cat(match_ind, '\n')
  }
  
  return(indices)
  
}

keep.the.factors.only.in.list = function(list_trim, all_grouping_vars,
                                         grouping_variable, factor_keep_names) {
  
  keep_indices = which(all_grouping_vars %in% factor_keep_names)
  col_names = names(list_trim)
  list_out = list()
  
  for (i in 1:length(col_names)) {
    col_name = col_names[i]
    matrix_trimmed = list_trim[[col_name]][,keep_indices]
    list_out[[col_name]] = matrix_trimmed
  }
  
  return(list(list_out, keep_indices))
  
}

trim_variables_from_list = function(list_in, vars_to_keep) {
  
  # get corresponding indices
  names_of_list = names(list_in)
  indices_to_keep = which(names_of_list %in% vars_to_keep)
  
  list_trim = list() # TODO! more elegant way
  for (i in 1:length(indices_to_keep)) {
    column_name = names_of_list[[indices_to_keep[i]]]
    list_trim[[column_name]] = list_in[[column_name]]
  }
  
  return(list_trim)
  
}

split.into.groups.by.grouping = function(list_out, grouping_vars_out) {
  
  unique_groups = unique(grouping_vars_out)
  no_of_unique_groups = length(unique_groups)
  
  col_names = names(list_out)
  
  no_of_subj = length(list_out[[col_names[1]]][1,])
  no_of_timepoints = length(list_out[[col_names[1]]][,1])
  
  list_temp = list()
  
  for (subj in 1:no_of_subj) {
    
    group_to_add = grouping_vars_out[subj]
    
    for (col in 1:length(col_names)) {
      
      col_name = col_names[[col]]
      to_add = list_out[[col_name]][,subj]
      
      list_temp[[group_to_add]][[col_name]] = cbind(list_temp[[group_to_add]][[col_name]], to_add)
      
    }
  }
  
  return(list_temp)
  
}

calculate.the.stats.of.grouped.lists = function(grouped_list) {
   
  no_of_groups = length(grouped_list)
  stats = list()
  
  list_temp_to_df = list()
  
  group_names = names(grouped_list)
  
  for (i in 1 : no_of_groups) {
   
    # TIME
    # Should be the same as we interpolated these with the same
    # time vectors, so just take the first one
    time_vector = grouped_list[[group_names[i]]]$time[,1]
    no_of_subj = length(grouped_list[[group_names[i]]]$time[1,])
    no_of_timep = length(grouped_list[[group_names[i]]]$time[,1])
    stats[[group_names[i]]]['n'] = no_of_subj
    
    # PUPIL
    temp_list = grouped_list[[group_names[i]]]$pupil
    pupil_matrix_per_group = matrix(unlist(temp_list), ncol= no_of_subj, byrow = FALSE)
    
    stats_vectors_pupilMean = rowMeans(pupil_matrix_per_group)
    stats_vectors_pupilMeanStdevs = apply(pupil_matrix_per_group, 1, sd, na.rm = FALSE)
    
    # ERROR
    # http://www.nist.gov/itl/sed/training/upload/combine-1.pdf
    # "The variance of the sums is the sum of the variances"
    # https://stats.stackexchange.com/questions/21104/calculate-average-of-a-set-numbers-with-reported-standard-errors
    temp_list = grouped_list[[group_names[i]]]$error
    error_matrix_per_group = matrix(unlist(temp_list), ncol= no_of_subj, byrow = FALSE)
    variance_matrix_per_group = error_matrix_per_group^2
    variance_sum = apply(pupil_matrix_per_group, 1, sum, na.rm = FALSE)
    stats_vectors_error_mean = variance_sum / no_of_subj^2
    
    # The total error?
    stats_vectors_stdev_total = sqrt(stats_vectors_pupilMeanStdevs^2 +
                                      stats_vectors_error_mean^2)
    
    # OUTPUT
    list_temp_to_df[[group_names[i]]]$time = time_vector
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
  
  # http://www.evanpickett.com/blog/2015/7/3/organising-data-for-graphs-in-r
  stats_list = list()
  
  # TODO! There must be an easier/elegant way
  time_list = vector()
  pupil_list = vector()
  error_list = vector()
  lo_list = vector()
  hi_list = vector()
  group_list = vector()
  
  for (i in 1:no_of_groups) {
    
    time = vector_stats[[col_names[i]]]$time
    pupil = vector_stats[[col_names[i]]]$pupil
    error = vector_stats[[col_names[i]]]$error
    lo = vector_stats[[col_names[i]]]$pupil -
         vector_stats[[col_names[i]]]$error
    hi = vector_stats[[col_names[i]]]$pupil +
         vector_stats[[col_names[i]]]$error
    group = rep(col_names[i], length(hi))
    
    time_list = c(time_list, time)
    pupil_list = c(pupil_list, pupil)
    error_list = c(error_list, error)
    lo_list = c(lo_list, lo)
    hi_list = c(hi_list, hi)
    group_list = c(group_list, group)
    
  }
  
  df = data.frame(time = time_list, pupil = pupil_list,
                  error = error_list, lo = lo_list,
                  hi = hi_list, group = group_list)
  
  return(df)
  
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

select.subset.from.df = function(df_in, parameters, settings, data_type) {
  
  # See e.g. 
  # https://stackoverflow.com/questions/4935479/how-to-combine-multiple-conditions-to-subset-a-data-frame-using-or
  
  # The desired factors to keep
  factor_keep_names = names(parameters[['factors_keep']])
  
  # The desired matching variables
  match_names = names(parameters[['matched_by']])
  
  # The desired factors to plot
  factor_names = names(parameters[['factors']])
  
  # Trim based on factors keep
  df_keep = trim.df.from.factor.keep(df_in, parameters, factor_keep_names)
  
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
  
  # This is e.g. "Control" if you want to match your pathology groups
  # to your controls which you probably have more than exotic pathologies
  reference_variable = parameters[['match_reference']]
  
  # quick'n'dirty init to illustrate the idea
  primary_groups = parameters[['factors_keep']][[parameters[['main_factor']]]]
  
  # Exclude the reference from these
  to_keep = is.na(match(primary_groups, reference_variable))
  vars_without_reference = primary_groups[to_keep]
  
  # Quick n dirty to compute the average of the desired variable (e.g. age)
  # from the desired groups
  var_of_interest = parameters[['main_factor']]
  sub_match = subset(df_keep, df_keep[[var_of_interest]] 
                     %in% vars_without_reference)
  
  # TODO! Now actually all the values are zeroes
  # match_average = mean(sub_match[[names(parameters[['matched_by']])[[1]]]])
  
  for (match_no in 1 : length(match_names)) {
    
    # match_variable =  names(parameters[['matched_by']])[[match_no]]
    
    # TODO! find for example the Controls that are 
    # some threshold away from the match_average
    
  }
  
  return(df_keep)
}

keep.only.the.features.of.interest = function(df_subset, feats_to_keep, 
                                              derived_feats_names) {
  
  col.names = colnames(df_subset)
  
  # get the corresponding names with the color prefix, and the uncertainty suffix
  to_keep = get.feat.names.for.color.with.uncertainty(feats_to_keep)
  
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

get.feat.names.for.color.with.uncertainty = function(feats_to_keep) {
  
  # now the feats to keep are given as they are in the "bins.csv", but now we might have
  # have blue_ or red_ in front of them
  feats_to_keep_red = paste('red_', feats_to_keep, sep='')
  feats_to_keep_blue = paste('blue_', feats_to_keep, sep='')
  feats_to_keep_values = c(feats_to_keep_red, feats_to_keep_blue)
  
  # now we have the uncertainties there as variables as well
  feats_to_keep_uncert = paste(feats_to_keep_values, '_Uncertainty', sep='')
  
  return(list(feats_to_keep_values, feats_to_keep_uncert))
  
}
