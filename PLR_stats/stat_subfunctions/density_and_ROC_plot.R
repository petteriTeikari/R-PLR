density.and.ROC.plot = function(df_trim, df_trim_stats, features, var_name_to_plot = 'mean',
                                  grouping_variable, combine_pathology = FALSE, 
                                  select_groups = NA,
                                  parameters, settings) {
  
  # Go through every feature in the df_trim and plot the distribution of it 
  vars_to_plot = colnames(df_trim)
  vars_to_plot[(vars_to_plot %in% parameters[['main_factor']])] = NA
  
  # Init the plot
  p = list()
  pCount = 1
  
  factors_in = toupper(df_trim[[grouping_variable]])
  
  if (!is.na(select_groups[1])) {
    factors_keep = factors_in %in% select_groups
    df_trim = df_trim[factors_keep,]
    factors_in = factors_in[factors_keep]
  }
  
  factors_kept = parameters[['factors_keep']][[parameters[['main_factor']]]]
  
  if (combine_pathology) {
    factors_in = combine.pathologies(factors_in, factors_kept)
  }
  
  for (f in 1 : length(features)) {
    
    out_list = feature.fields.to.list(df_trim, feat_in = features[[f]], vars_to_plot)
      list_feats = out_list[[1]]
      no_subplots = out_list[[2]]
    
    # Go through all these subplots
    for (sp in 1 : no_subplots) {
      # cat(features[f], ' ', sp, no_subplots, ' ', names(list_feats), '\n')
      feat_type = names(list_feats)[[sp]]
      p = density.plot.subfunction(p, feat_vectors = list_feats[[feat_type]], factors_in,
                                   feature_name = features[f], var_name_to_plot, feat_type,
                                   grouping_variable, parameters, settings, pCount)
      pCount = pCount + 1
    }
  }
  
  no_of_plot_cols = round(pCount / 3)
  no_of_plot_rows = ceiling(pCount / no_of_plot_cols)
  
  # For display
  do.call(grid.arrange, c(p, list(ncol=no_of_plot_cols, nrow=no_of_plot_rows)))
  
  # This for saving for disk
  # https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
  ml <- marrangeGrob(p, ncol=no_of_plot_cols, nrow=no_of_plot_rows)
  
}


# ACTUAL PLOTTING FUNCTION
density.plot.subfunction = function(p, feat_vectors, factors_in,
                                   feature_name, var_name_to_plot, feat_type, 
                                   grouping_variable, parameters, settings, pCount) {
            
  df_feats = data.frame(feat_vectors)
  
  # keep only the "var_name"
  df_feats = df_feats[which(colnames(df_feats) %in% var_name_to_plot)]
  df_feats[[grouping_variable]] = factor(factors_in)
  
  # Compute the ROC Statistics
  ROC_out = compute.ROC.statistics.of.data.frame(y = df_feats[[var_name_to_plot]], 
                                                  group_factors = df_feats[[grouping_variable]], 
                                                  to_compare_out = 'CONTROL')

  # TODO! Add this to other plots as well!
  # create new column names with the AUC score in it
  levelnames_from = levels(df_feats$Diagnosis)
  no_of_factors = length(levelnames_from)
  levelnames_to = vector(, length=no_of_factors)
  
  for (i in 1 : no_of_factors) {
    
    if (i == 1) {
      levelnames_to[[i]] = levels(df_feats$Diagnosis)[i]
    } else {
     
      saved_in_ROC_list = ROC_out[[i-1]]
      # diff = abs(ROC_out[[i-1]]$cutoffs - (-49))
      # sensitivity = ROC_out[[i-1]]$TPR[cutoff_quickndirty_ind]
      # specificity = ROC_out[[i-1]]$FNR [cutoff_quickndirty_ind]
      # cat(feature_name, ' | ', levels(df_feats$Diagnosis)[i], ': spec = ', specificity, ', sens =', sensitivity, '\n')
      
      AUC_value = round(ROC_out[[i-1]]$AUC, digits=2)
      levelnames_to[[i]] = paste0(levels(df_feats$Diagnosis)[i],
                                 ' | AUC = ', AUC_value)
    }
  }
  # remap with plyr's function
  df_feats$Diagnosis = mapvalues(df_feats$Diagnosis, 
                                 from = levelnames_from, to = levelnames_to)
  
  # and plot actually the distributions
  p[[pCount]] = ggplot(df_feats, aes(x = mean)) +
                # geom_dotplot(aes(fill = sex), alpha=0.4, binwidth = 1.25)
                geom_density(aes(color = Diagnosis), alpha=0.4) +
                labs(title=feature_name, subtitle=feat_type) +
                theme(legend.text=element_text(size=6))
  

  return(p)
  
}


compute.ROC.statistics.of.data.frame = function(y, group_factors, to_compare_out = 'CONTROL') {

  # Do pairwise, and multiclass ROC?
  # http://scikit-learn.org/stable/auto_examples/model_selection/plot_roc.html
  
  n = 2 # 2 for pairwise
  factor_names = levels(group_factors)
  comb_indices = get.possible.combinations(group_factors, to_compare_together=n)
  
  # Go through all the combinations
  ROC_OUT = list()
  no_of_comb = dim(comb_indices)[2]
  contains_to_compare_to = vector(, length = no_of_comb)
  contains_other_factor = vector(, length = no_of_comb)
  
  for (c in 1 : no_of_comb) {
    
    level1 = factor_names[comb_indices[1,c]] # e.g. Control
    level2 = factor_names[comb_indices[2,c]] # e.g. POAG
    
    ind1 = group_factors %in% level1 
    ind2 = group_factors %in% level2
    
    # We want to determine if this pair contains for example Control,
    # as it easier to display on the plot, just control compared to other group
    contains_to_compare_to[[c]] = as.logical(sum(c(level1, level2) %in% to_compare_out))
    contains_other_factor[[c]] = level2
    
    y1 = y[ind1] # subset of y-values
    y2 = y[ind2] # subset of y-values
    
    # TODO! Make the ROC work
    ROC_OUT[[c]] = pairwise.ROC(y1, y2, level1, level2)
    
  }
  
  return(ROC_OUT)  
  
}

pairwise.ROC = function(y1, y2, level1, level2, package='NONE', norm__p_thr = 0.05) {

  # Test whether the distributions are normal (Shapiro–Wilk test)  
  is.normal.y1 = shapiro.test(y1)$p.value < norm__p_thr
  is.normal.y2 = shapiro.test(y2)$p.value < norm__p_thr
  # y1log = log(abs(y1))
  # y2log = log(abs(y2))
  
  df = data.frame(y = c(y1, y2), factor = c(rep(level1, length(y1)), rep(level2, length(y2))))
  df[['factor']] = as.integer(df[['factor']])
  # ggplot(df, aes(x=y)) + geom_density(aes(color = factor,fill = factor), alpha=0.1)
  
  # "easyROC: An Interactive Web-tool for ROC Curve Analysis Using R Language Environment" 
  # The R Journal, 8(2):213-230 if you use easyROC in your analysis.
  # http://www.biosoft.hacettepe.edu.tr/easyROC/
  
  # IMPLEMENT THE ABOVE MAYBE?
  
  if (identical(package, 'MAMSE')) {
    # from MAMSE package
    # https://www.rdocumentation.org/packages/MAMSE/versions/0.2-1
    # https://doi.org/10.1002/cjs.11351
    # Empirical ROC curve based on Ledger (1994)
    # https://doi.org/10.1093/oxfordjournals.humrep.a138307
    roc(y1,y2,AUC=TRUE)
  
  } else if (identical(package, 'pROC')) {
    # https://cran.r-project.org/web/packages/pROC/pROC.pdf  
    
  } else if (identical(package, 'optimalCutpoints')) {
    # https://www.jstatsoft.org/article/view/v061i08/v61i08.pdf
    optimal.cutpoint.Youden <- optimal.cutpoints(X = df, status = "factor", tag.healthy = 1, 
                                                 methods = "Youden", data = df, pop.prev = NULL, 
                                                 control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)
  } else if (identical(package, 'NONE')) {
  
    # these are the possible cutoffs 
    unique_y = sort(unique(c(y1,y2)))
    
    # preallocate
    tp = vector(,length(unique_y)); fp = vector(,length(unique_y))
    tn = vector(,length(unique_y)); fn = vector(,length(unique_y))
    
    # which is larger
    y1_median = median(y1)
    y2_median = median(y2)
    
    for (u in 1 : length(unique_y)) {
      tp[u] = sum(y1 <= unique_y[u])
      fp[u] = sum(y2 < unique_y[u])
      tn[u] = sum(y2 >= unique_y[u])
      fn[u] = sum(y1 > unique_y[u])
    }
    
    # http://arogozhnikov.github.io/2015/10/05/roc-curve.html
    
    # true positive rate (part of correctly classified signal, also known as 
    # recall, sensitivity or signal efficiency).
    TPR = tp / (tp + fn)
    
    # background efficiency
    FPR = fp / (fp + tn)
    
    # tnr
    TNR = tn / (fp + tn)
    
    # specificity
    FNR = fn / (tp + fn)
   
    # AUC
    AUC = trapz(FPR, TPR)
    
    
    list_out = list()
      list_out[['cutoffs']] = unique_y
      list_out[['TPR']] = TPR
      list_out[['FPR']] = FPR
      list_out[['TNR']] = TNR
      list_out[['FNR']] = FNR
      list_out[['AUC']] = AUC
    
    return(list_out)
      
  }
  
  
  
  
  # TODO! each of the means come with uncertainties as well, that you could use
  #       for weighing the ROC computation as the MAMSE is short for
  #       "Minimum Averaged Mean Squared Error (MAMSE) Weights"
  
}

multiclass.ROC = function() {
  # 'response' has more than two levels. Consider setting 'levels' explicitly or using 'multiclass.roc'  
}

get.possible.combinations = function(factors, to_compare_together = 2) {

  no_of_unique_groups = length(levels(factors))
  to_compare_together = 2 # we do pairwise now
  
  # Get all possible comparison options
  comb_indices = combn(no_of_unique_groups, to_compare_together) 
  no_of_comb = dim(comb_indices)[2]
  
  return(comb_indices)

}



combine.pathologies = function(factors_in, factors_kept) {
  
  factors_out = factors_in
  str(factors_out)
  str(factors_in)
  
  for (i in 1 : length(factors_kept)) {
    
    group_name_in = factors_kept[i]
    group_name_out = pathology.lookup.table(group_name_in)
    factor_indices = toupper(factors_in) %in% toupper(group_name_in)
    factors_out[factor_indices] = group_name_out
  }
  
  return(factors_out)
  
}

pathology.lookup.table = function(group_name_in) {
  
  str(group_name_in)
  
  group_name_in = toupper(group_name_in)
  
  if (identical(group_name_in,'POAG') |
      identical(group_name_in,'NTG') |
      identical(group_name_in,'DISC SUSPECT')) {
    
    group_name_out = 'Glaucoma'
    
  } else if (identical(group_name_in,'MILD NPDR') | 
             identical(group_name_in, 'MODERATE NPDR') |
             identical(group_name_in, 'DM')) {
    
    group_name_out = 'Diabetes'
    
  } else if (identical(group_name_in,'CONTROL')) {
    
    group_name_out = 'Control'
    
  } else {
    warning('Do not know how to map this "', group_name_in, '" to "umbrella pathology"')
  }
  
  return(group_name_out)
  
}