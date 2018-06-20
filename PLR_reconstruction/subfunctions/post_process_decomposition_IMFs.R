process.folder.of.IMFs = function(path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/recon_temp',
                                  pattern = ".*.csv") {
  
  imf_files = list.files(path=path, pattern=pattern, recursive=FALSE, full.names = TRUE)
  param_decomp = list()
  
  for (i in 1 : length(imf_files)) {
    df_imfs = read.csv(imf_files[i])
    df_imf_out[i] = post.process.imf.decomposition(df_imfs, param_decomp) 
  }
}


post.process.imf.decomposition = function(df_imfs, param_decomp) {
  
  names = colnames(df_imfs)
  samples = length(df_imfs[[1]])
  vectors = matrix(nrow = samples, ncol = length(names))
  
  # fixed indices
  noise_ind = c(1,2,3,4)
  high_osc_ind = c(5,6,7)
  lo_osc_ind = c(8,9)
  base_ind = c(10,11)
  
  for (i in 1 : length(names)) {
    
    colname = names[i]
    vector = df_imfs[[colname]]
    vector_mean = mean(vector)
    vector_SD = sd(vector)
    cat(colname, ': mean =', vector_mean, ', SD =', vector_SD, '\n')
    
    ggplot(data.frame(vector), aes(x=(1:length(vector))/30, y=vector)) + geom_line()
    
    vectors[,i] = vector
    
  }
  
  noise = rowSums(vectors[,noise_ind])
  high_osc = rowSums(vectors[,high_osc_ind])
  lo_osc = rowSums(vectors[,lo_osc_ind])
  base = rowSums(vectors[,base_ind])
  
  orig_signal = rowSums(vectors)
  
  plot.IMFs(t, orig_signal, noise, high_osc, lo_osc, base)
  
  return(df_imf_out)  
  
  
}

plot.IMFs = function(t, orig_signal, noise, high_osc, lo_osc, base) {
  
  # create df
  df = data.frame(time = t, OrigSignal = orig_signal, denoised = orig_signal - noise,
                  noise = noise, high_osc = high_osc,
                  lo_osc = lo_osc, base = base)
  
  p = list()
  
  p[[1]] = ggplot(df, aes(x=t, y=OrigSignal)) + geom_line() + labs(title='Original Signal', subtitle='PLR1002')
  p[[2]] = ggplot(df, aes(x=t, y=denoised)) + geom_line() + labs(title='Denoised')
  p[[3]] = ggplot(df, aes(x=t, y=noise)) + geom_line() + labs(title='Noise')
  p[[4]] = ggplot(df, aes(x=t, y=high_osc)) + geom_line() + labs(title='HiFreq Osc')
  p[[5]] = ggplot(df, aes(x=t, y=lo_osc)) + geom_line() + labs(title='LoFreq Osc')
  p[[6]] = ggplot(df, aes(x=t, y=base)) + geom_line() + labs(title='Base Signal')
  
  no_of_cols = 3
  do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
  
  export.pupil.dataframe.toDisk(df, files_to_process_recon[1], '/home/petteri/', 'decomposed')
  
  
}