export.PLRwithFeatures = function(data_path_out, just_filename, export_as,
                                  data_frame_in, bins, features_blue, features_red) {

  if (identical(export_as, 'CSV')) {
    filename_CSV = sub('.csv', '_features.csv', just_filename) 
    filepath_CSV = file.path(data_path_out, filename_CSV, fsep = .Platform$file.sep)
    export.PLRfeats.asCSV(data_path_out, filename_CSV, filepath_CSV, 
                          data_frame_in, bins, features_blue, features_red)
    
  } else if (identical(export_as, 'HDF5')) {
    filename_h5 = sub('.csv', '_features.h5', just_filename) 
    filepath_h5 = file.path(data_path_out, filename_h5, fsep = .Platform$file.sep)
    export.PLRfeats.asHDF5(data_path_out, filename_h5, 
                           data_frame_in, bins, features_blue, features_red)
  } else {
    warning('Exporting as ', export_as, ' not supported')
  }
}
 
  




  