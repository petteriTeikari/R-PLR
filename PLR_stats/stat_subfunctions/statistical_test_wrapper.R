statistical.test.wrapper = function(df_trim, parameters, parameters_stats,
                                    subject_codes) {

  # Get data characteristics 
  # ------------------------------------------------

    vars_to_plot = colnames(df_trim)
    vars_to_plot[(vars_to_plot %in% parameters[['main_factor']])] = NA
    features = parameters[['features']]
    factors_in = toupper(df_trim[[grouping_variable]])
    factors_kept = parameters[['factors_keep']][[parameters[['main_factor']]]]
    
    i = 1 # TODO! give this outside, as you may want to loop different sort of combos
    groups_in = parameters_stats[['categorical_vars']][i] # placeholder now
  
  # Convert features to a list ------------------------------------------------
  
    list_feats = list()
    for (f in 1 : length(features)) {
      list_feats[[features[f]]] = feature.fields.to.list(feat_in = features[[f]], vars_to_plot)
    }
  
  # Preliminary Tests ------------------------------------------------
  # for each feature per each group per each color (e.g. 8 x 1 x 3)
    
    # In other words, who does values differ between the groups (and length = 1, when only e.g. you want to do
    # Diagnosis, and length = 2 when you want to do both Sex and Diagnosis for example, etc.)
    prelim_tests = list()
    for (g in 1 : length(groups_in)) {
      group_name = parameters_stats[['categorical_var_names']][g]
      
      for (f in 1 : length(features)) {
        conditions = names(list_feats[[features[f]]]) # e.g. BLUE/RED/DIFF or GLOBAL
        
        for (c in 1 : length(conditions)) {
        
          prelim_tests[[group_name]][[features[f]]][[conditions[c]]] = 
             compute.prelim.stat.tests(x1 = factors_in,
                                       x2 = rep(NA, length(factors_in)), # add when you implement the second independent variable
                                       y = list_feats[[features[f]]][[conditions[c]]]$mean,
                                       error = list_feats[[features[f]]][[conditions[c]]]$uncert,
                                       subj = subject_codes,
                                       p_threshold = parameters_stats[['prelim_tests']][['p_threshold']])
        }
      }
      
    }
    
  # Do pairwise comparisons ------------------------------------------------
  
    pairwise_tests = list()
    for (g in 1 : length(groups_in)) {
      group_name = parameters_stats[['categorical_var_names']][g]
      
      for (f in 1 : length(features)) {
        conditions = names(list_feats[[features[f]]]) # e.g. BLUE/RED/DIFF or GLOBAL
        
        # At the moment we are comparing whether a "feature" is different
        #  * i.e. is pupillary constiction larger in group A than in group B
        # And if the "condition" is different 
        # * i.e. is pupillary constiction larger under blue light compared to red light
        pairwise_tests[[group_name]][[features[f]]] =
          compute.pairwise.tests(x1 = factors_in,
                                 x2 = rep(NA, length(factors_in)), # add when you implement the second independent variable
                                 y_list = list_feats[[features[f]]],
                                 subj = subject_codes,
                                 conditions = conditions,
                                 p_threshold = parameters_stats[['prelim_tests']][['p_threshold']],
                                 g_ind = g, f_ind = f, prelim_test = prelim_tests[[group_name]][[features[f]]])
      }
    }
    
  # IF NORMAL
  
    # Pearson's Chi-squared test
      # Method of Chi Square p value adjustment: Benjamini–Hochberg
    # Fisher's Exact Test for Count Data
      # Method of Fisher's Test p value adjustment: Benjamini–Hochberg
 
  # IF NON-NORMAL
  
    # Kruskal-Wallis rank sum test
      # Dunn's Post-hoc Test:
  
  # IF (Levene's Test is insignificant: the variances of x groups are assumed to be homogenous)
  
    # One-way ANOVA
      # Tukey multiple comparisons of means
      # 95% family-wise confidence level
}

compute.pairwise.tests = function(x1, x2, y_list, subj, conditions,
                                   p_threshold, g_ind, f_ind, prelim_test) {
  
  for (c in 1 : length(conditions)) {
    for (x in 1 : length(unique(x1))) {
      cat(c, ' ', x, '\n')  
      # CONTINUE
    }
  }
  
  return(results_out)
}

