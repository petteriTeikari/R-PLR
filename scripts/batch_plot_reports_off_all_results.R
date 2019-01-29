source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/plot/report_visualization.R')  

data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/reconstructed/'
pattern_to_find = "*.csv"
files_to_process = list.files(path=data_path, pattern=pattern_to_find, recursive=FALSE, full.names = TRUE)
plot_param = list()

All = lapply(files_to_process, function(filepath){
  report.visualization(filepath, plot_param)
})
