process.folder.of.IMFs = function(path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/recon_temp',
                                  pattern = ".*.csv") {
  
  source(file.path(source_path, 'post_process_decomposition_IMFs.R', fsep = .Platform$file.sep))
  
  imf_files = list.files(path=path, pattern=pattern, recursive=FALSE, full.names = TRUE)
  param_decomp = list()
  
  for (i in 1 : length(imf_files)) {
    filecode = strsplit(tail(strsplit(imf_files[i], .Platform$file.sep)[[1]],1), '_')[[1]][1]
    df_IMFs = read.csv(imf_files[i])
    df_imf_out[i] = post.process.imf.decomposition(df_IMFs, filecode, param_decomp) 
  }
}

get.stats.of.IMFs.on.disk = function(path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/recon_temp',
                                     pattern = ".*.csv") {
  
  imf_files = list.files(path=path, pattern=pattern, recursive=FALSE, full.names = TRUE)
  
  no_of_IMFs = vector(, length = length(imf_files))
  
  for (i in 1 : length(imf_files)) {
    filecode = strsplit(tail(strsplit(imf_files[i], .Platform$file.sep)[[1]],1), '_')[[1]][1]
    cat(filecode, ' ')
    df_IMFs = read.csv(imf_files[i])
    no_of_IMFs[i] = length(df_IMFs)
  }
  
  summary(no_of_IMFs) # min 10, and max 12
 
}

IMF.indices.into.radiobutton.indices = function(IMF_index_estimates, df_IMFs, input_type) {
  
  no_of_IMFs = length(df_IMFs)
  selected_vector = vector(, length=no_of_IMFs)
  names_in = names(IMF_index_estimates)
  
  for (name_ind in 1 : length(names_in)) {
    indices_per_name = IMF_index_estimates[[names_in[name_ind]]]
    
    for (value_ind in 1 : length(indices_per_name)) {
      value = IMF_index_estimates[[names_in[name_ind]]][value_ind]
      selected_vector[value] = name_ind
    }
  }
  
  return(selected_vector)
}

IMF.indices.from.radiobutton.indices = function(IMF_radiobutton_indices, df_IMFs, components, input_type) {
  
  indices = list()
  
  # init
  for(c in 1 : length(components)) {
    indices[[components[c]]] = NA
  }
  
  for (i in 1 : length(IMF_radiobutton_indices)) {

    components
    comp_i = components[IMF_radiobutton_indices[i]]
    match_ind = indices[[comp_i]]
    
    something_added_already_per_component = !is.na(match_ind)[1]
    # cat(something_added_already_per_component, '\n')
    
    # nothing yet added
    if (!something_added_already_per_component) {
      indices[[components[IMF_radiobutton_indices[i]]]] = i
      # cat('  NEW', '\n')
    } else {
      indices[[components[IMF_radiobutton_indices[i]]]] = c(indices[[components[IMF_radiobutton_indices[i]]]], i)
      # cat('  to ADD', '\n')
    }
  }
  return(indices)
}

indices.from.component.names = function(component_names, components) {
  
  indices = vector(, length(component_names))
  for (i in 1 : length(indices)) {
    indices[i] = which(components %in% component_names[i])
  }
  
  return(indices)
  
}

generate.signals.from.indices = function(df_IMFs, indices, components) {
  
  # Init guess for the component signals
  signals = list()
  no_of_samples = length(df_IMFs[[1]])
  str(indices)
  
  options(warn = -1)
  for (comp_ind in 1 : length(components)) {
    
    comp_name = components[[comp_ind]]
  
    if (!is.na(indices[[comp_name]])) {
      if (length(indices[[comp_name]]) > 1) {
        signals[[comp_name]] = rowSums(df_IMFs[,indices[[comp_name]]])
      } else {
        signals[[comp_name]] = df_IMFs[,indices[[comp_name]]]
      }
    } else {
      signals[[comp_name]] = vector(, length=no_of_samples)
      signals[[comp_name]][] = NA
      
    }
  }
  
  options(warn = 1)
  
  return(signals)
  
}

estimate.imf.combination.indices = function(df_IMFs, input_type = '1stPass', path = NA,
                                            filecode = NA, param_decomp = NA, verbose = TRUE) {
  
  names = colnames(df_IMFs)
  samples = length(df_IMFs[[1]])
  vectors = matrix(nrow = samples, ncol = length(names))
  
  # fixed indices
  no_of_IMFs = length(df_IMFs)
  
  noise_ind = vector()
  noise_nonNorm_ind = vector()
  spike_ind = vector()
  high_osc_ind = vector()
  lo_osc_ind = vector()
  base_ind = c(no_of_IMFs-1, no_of_IMFs)
  
  if (grepl('loFreq', path)) {
    noise_ind_max = 5
  } else if (grepl('noise', path)) {
    noise_ind_max = no_of_IMFs-2
    
  } else {
    noise_ind_max = 4
  }
  
  for (i in 1 : length(names)) {
    
    colname = names[i]
    
    # stats of IMF
    vector = df_IMFs[[colname]]
    stats = compute.stats.of.vector(vector)
    
    # Guess noise
    if (i <= noise_ind_max) {
      
      if (grepl('1stPass', input_type)) {
        if (stats$is.normal) {
          noise_ind = c(noise_ind, i)
        } else {
          noise_nonNorm_ind = c(noise_nonNorm_ind, i)
        }  
        
    } else {
        
      noise_ind = c(noise_ind, i)
      
    }
      
    # Guess hi and low
    } else if (i < no_of_IMFs - 1) {
      
      if (grepl('1stPass', input_type)) {
      
        if (stats$SD > 2) {
          lo_osc_ind = c(lo_osc_ind, i)
          
        } else {
          high_osc_ind = c(high_osc_ind, i)
        }
        
      } else if (grepl('loFreq', input_type)) {
        
        if (abs(stats$mean) > 0.2) {
          lo_osc_ind = c(lo_osc_ind, i)
          
        } else {
          high_osc_ind = c(high_osc_ind, i)
        }
        
      } else if (grepl('noise', input_type)) {
        
          spike_ind = c(spike_ind, i)
        
      }
      
    }
    
    # Print stats
    if (verbose) {
      cat(colname, ': mean =', round(stats$mean,2), 
                   ', SD =', round(stats$SD, 2),
                   ', min =', round(stats$min, 2), 
                   ', max =', round(stats$max, 2), 
                   ', is_normal =', stats$is.normal, 
                   ', kurtosis =', round(stats$kurtosis, 2),
                   ', skewness =', round(stats$skewness, 2), 
                   '\n')
    }
      
    # ggplot(data.frame(vector), aes(x=(1:length(vector))/30, y=vector)) + geom_line()
    vectors[,i] = vector
    
  }
  
  # t = 1:length(vector)
  
  # noise = rowSums(vectors[,noise_ind])
  # noise_nonNorm = rowSums(vectors[,noise_ind])
  # high_osc = rowSums(vectors[,high_osc_ind])
  # lo_osc = rowSums(vectors[,lo_osc_ind])
  # base = rowSums(vectors[,base_ind])
  
  # orig_signal = rowSums(vectors)
  
  # TODO! Add non-normal noise
  # plot.IMFs(t, orig_signal, noise, high_osc, lo_osc, base, filecode)
  
  # pack for output
  indices = list()
  
  if (grepl('loFreq', path)) {
    indices[['noise']] = noise_ind
    indices[['loFreq_hi']] = high_osc_ind
    indices[['loFreq_lo']] = lo_osc_ind
    indices[['base']] = base_ind  

  } else if (grepl('hiFreq', path)) {
    indices[['noise']] = noise_ind
    indices[['hiFreq_hi']] = high_osc_ind
    indices[['hiFreq_lo']] = lo_osc_ind
    indices[['base']] = base_ind  
    
  } else if (grepl('noise', path)) {
    indices[['noise']] = noise_ind
    indices[['spikes']] = spike_ind
    indices[['base']] = base_ind  
        
  } else {
    indices[['noiseNorm']] = noise_ind
    indices[['noiseNonNorm']] = noise_nonNorm_ind
    indices[['hiFreq']] = high_osc_ind
    indices[['loFreq']] = lo_osc_ind
    indices[['base']] = base_ind  
  }
      
  # signals = list()
    
  # check the output still if you have empty indices, and if so 
  # add NA
  index_names = names(indices)
  for (i in 1:length(index_names)) {
    no_of_entries_per_component = length(indices[[index_names[i]]])
    if (no_of_entries_per_component == 0) {
      indices[[index_names[i]]] = c(NA)
    }
  }
  
  return(indices)  
  
}

compute.stats.of.vector = function(vector, p_thr = 0.05) {
  
  library(moments)
  
  stats = list()
  
  # The common ones
  stats$mean = mean(vector)
  stats$SD = sd(vector)
  stats$max = max(vector)
  stats$min = min(vector)
  stats$kurtosis = kurtosis(vector)
  stats$skewness = skewness(vector)
  
  # Test for normality
  if (length(unique(vector)) > 1) {
    s = shapiro.test(vector)
    stats[['shapiro']]$statistic = s$statistic
    stats[['shapiro']]$p.value = s$p.value
    stats$is.normal = s$p.value < p_thr
  # Shapiro test throws an error if all the values are the same
  # So we handle that here
  } else {
    stats[['shapiro']]$statistic = NA
    stats[['shapiro']]$p.value = NA
    stats$is.normal = FALSE
  }
  
  return(stats)
  
}

plot.IMFs = function(t, orig_signal, noise, high_osc, lo_osc, base, filecode) {
  
  # create df
  df = data.frame(time = t, OrigSignal = orig_signal, denoised = orig_signal - noise,
                  noise = noise, high_osc = high_osc,
                  lo_osc = lo_osc, base = base)
  
  p = list()
  
  p[[1]] = ggplot(df, aes(x=t, y=OrigSignal)) + geom_line() + labs(title='Original Signal', subtitle=filecode)
  p[[2]] = ggplot(df, aes(x=t, y=denoised)) + geom_line() + labs(title='Denoised')
  p[[3]] = ggplot(df, aes(x=t, y=noise)) + geom_line() + labs(title='Noise')
  p[[4]] = ggplot(df, aes(x=t, y=high_osc)) + geom_line() + labs(title='HiFreq Osc')
  p[[5]] = ggplot(df, aes(x=t, y=lo_osc)) + geom_line() + labs(title='LoFreq Osc')
  p[[6]] = ggplot(df, aes(x=t, y=base)) + geom_line() + labs(title='Base Signal')
  
  no_of_cols = 3
  do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
  
  # export.pupil.dataframe.toDisk(df, files_to_process_recon[1], '/home/petteri/', 'decomposed')
  
}

estimate.input.noise.amplitude.for.EMD = function(t, y, plot_on = FALSE) {
  
  model <- loess(y ~ t, span = 0.025) # estimate the local mean with LOESS
  noise.amp.init <- sd(model$residuals, na.rm = TRUE)
  
  # How about estimate the low amplitude mean only, and get rid of the outliers
  # coming from sharp transitions that LOESS cannot handle
  residuals = model$residuals
  residual_mean = mean(residuals , na.rm = TRUE) # mean of residual signal
  residual_distance_from_mean = abs(residuals  - residual_mean)
  residual_outliers = residual_distance_from_mean > (1.96*noise.amp.init) # if values deviate more than 1.96sd from mean
  no_of_outliers = sum(residual_outliers == TRUE)
  residuals[residual_outliers] = NA # assign NAs to outliers
  
  # And then recompute the noise.amp from the SD
  noise.amp <- sd(residuals, na.rm = TRUE)
  
  # Plot 
  if (plot_on) {
    
    df = data.frame(t = t, denoised = y, loess_model = model$fitted, 
                    residuals_init = model$residuals,
                    residuals = residuals)
    
    p = list()
    p[[1]] = ggplot(df, aes(t)) +
      geom_line(aes(y = denoised, colour = "Denoised")) +
      geom_line(aes(y = loess_model, colour = "LOESS")) +
      labs(title = filecode)
    
    p[[2]] = ggplot(df, aes(t)) +
      geom_line(aes(y = residuals_init, colour = "Init Residuals")) +
      labs(subtitle=paste0('Noise amplitude init = ', noise.amp.init))
    
    p[[3]] = ggplot(df, aes(t)) +
      geom_line(aes(y = residuals, colour = "Residuals")) +
      labs(subtitle=paste0('Noise amplitude = ', noise.amp))
    
    no_of_cols = 1
    do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
    
  }
  
  return(noise.amp)
  
}

pick.IMF.subset.from.df = function(df_in) {
  
  cols_in = colnames(df_in)
  
  vars_to_incl = c('time_onsetZero')
  wildcards = c("IMF", "residue", "hinstfreq", "hamp")

  # Add with each keyword  
  indices_IMF = grep(vars_to_incl, cols_in)
  for (i in 1 : length(wildcards)) {
    indices_IMF = c(indices_IMF, grep(wildcards[i], cols_in))
  }
  df_in = df_in[indices_IMF]
  
  # make the time_onsetZero just time  
  names(df_in) <- gsub(x = names(df_in), pattern = "\\_onsetZero", replacement = "") 
  cols_in_out = colnames(df_in)
  
  # return indices also for the different components for easier manipulating later
  indices_comp = list()
  for (i in 1 : length(wildcards)) {
    indices_comp[[tolower(wildcards[i])]] = grep(wildcards[i], cols_in_out)
  }
  
  return(list(df_in, indices_comp))
}

from.signals.make.emd.result = function(original.signal, df_subset, indices_comp) {
  
  # returns 14 elements in a list
  emd.result.out = list()
  
  emd.result.out[['original.signal']] = original.signal
  emd.result.out[['residue']] = df_subset[['residue']] 
  emd.result.out[['tt']] = df_subset[['time']] 
  
  # These are trickier if you used non-default values
  emd.result.out[['max.sift']] = 200
  emd.result.out[['tol']] = 5
  emd.result.out[['stop.rule']] = "type5"
  emd.result.out[['boundary']] = "wave"
  emd.result.out[['sm']] = "none"
  emd.result.out[['smlevels']] = 1
  emd.result.out[['max.imf']] = 10
  
  key = "hinstfreq"
  emd.result.out[[key]] = get.key.matrix(df_subset, indices_comp, key)
  key = "hamp"
  emd.result.out[[key]] = get.key.matrix(df_subset, indices_comp, key)
  key = "imf"
  emd.result.out[[key]] = get.key.matrix(df_subset, indices_comp, key)
  
  emd.result.out[['nimf']] = dim(emd.result.out[[key]])[2]
 
  return(emd.result.out)
  
}

get.key.matrix = function(df_subset, indices_comp, key) {
  
  key_indices = indices_comp[[key]]
  df_key = df_subset[key_indices]
  key_matrix = matrix(, nrow = length(df_key[[1]]), ncol = length(df_key))
  for (i in 1 : length(df_key)) {
    key_matrix[,i] = df_key[[i]]
  }
  
  return(key_matrix)
  
}