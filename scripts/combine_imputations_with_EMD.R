# Now we have three different folder containing columns that we want to combine


path_main = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/'
subfolder_paths = c('imputation_final',
                    'recon_EMD',
                    file.path('recon_EMD', 'IMF_fusion', fsep = .Platform$file.sep))

patterns = c('imputation_final',
              'recon_EMD',
              file.path('recon_EMD', 'IMF_fusion', fsep = .Platform$file.sep))

filelisting = list()
for (i in 1 : length(subfolder_paths)) {
  
  fullpath = file.path(path_main, subfolder_paths[i])
  colname = paste0('ind', i)
  files[[colname]] = list.files(path=fullpath, pattern=patterns[i], recursive=FALSE, full.names = TRUE)
  
}