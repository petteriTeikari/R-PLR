import.computedFeats = function(data_path_feats, pattern_to_find, dataset_type, masterXLS_data_path, XLS_filename) {

  # Get a file listing of the files found from the data folder
  files = list.theFeatFiles(data_path_feats, pattern_to_find, dataset_type)
  
  # Read in the "Master Excel file" containing the experiment settings
  master_data = read.theMasterExcel(masterXLS_data_path, XLS_filename)
  
  # Read in the found files, and combine with the variables (features, Excel columns)
  # from the Master Excel file
  feats_out = combine.featsWithExcelMaster(files, master_data)
  
    full_feats_df = feats_out[[1]]
    derived_feats_names = feats_out[[2]]
  
  return(feats_out)
  
}









