# Input folder
folder = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/FinalOUT'
folder_tf = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/timeFrequency'
folder_out = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/FinalOUT_smooth'

# Create outlier_free
if (dir.exists(folder_out) == FALSE) {
  dir.create(folder_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# List files of that folder
files_fullpath = list.files(path=folder, pattern='*', recursive=FALSE, full.names = TRUE)
files_names = list.files(path=folder, pattern='*', recursive=FALSE, full.names = FALSE)
files_tf_fullpath = list.files(path=folder_tf, pattern='*', recursive=FALSE, full.names = TRUE)
files_tf_names = list.files(path=folder_tf, pattern='*', recursive=FALSE, full.names = FALSE)



# go through the files
for (i in 1 : length(files_fullpath)) {
  
  subject_code = gsub('PLR', '', strsplit(files_names, split = '_')[[1]][1])
  data_in = read.csv(file = files_fullpath[i])
  
  # 
  
  
  # Find the corresponding TF
  # file_tf_ind = grep(subject_code, files_tf_names)
  # if (length(file_tf_ind) > 0) {
  #   load(files_tf_fullpath[file_tf_ind])
  # } else {
  #   cat('No TF file found for = ', files_names[i], '\n')
  # }
  # 
  # # Get the dominant freq (based on power) on each time point
  # power_matrix = hgram$hhg_img$z
  # max_indices = apply(power_matrix,1,which.max)
  # max_values = apply(power_matrix,1,max)
  
  # write back to disk (replaces the old)
  # write.table(data_out, 
  #             file = file.path(folder_out, files_names[i], fsep = .Platform$file.sep), 
  #             sep = ',',
  #             row.names = FALSE, col.names = FALSE)
  
}

colMax = function(data) sapply(data, max, na.rm = TRUE)


smooth.decomposition = function(data_in) {
  
  
}