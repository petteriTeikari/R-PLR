export.PLRfeats.asHDF5 = function(data_path_out, just_filename, 
                                  data_frame_in, bins, features_blue, features_red) {
  
  cat('\nExport now the features as HDF5 file\n')
  cat(' Tabular text file way too painful at this point to write\n')
  cat('  .. easier to use this structured format\n')
  cat('       https://fisheye.net/q-archiving-in-data-in-text-csv-vs-hdf5-files/\n')
  cat(' Problems this as well:')
  cat('   http://cyrille.rossant.net/moving-away-hdf5/')
  
  # Well now gets a bit tricky to use text files for these, so just use HDF5
  # allowing now key-value style approach
  # https://cran.r-project.org/package=h5
  # https://github.com/mannau/h5
  
  # install.packages("h5")
  # On MAC, "brew install homebrew/science/hdf5 --enable-cxx"
  # Windows should come with HDF5
  # Ubuntu, "sudo apt-get install libhdf5-dev"
  library(h5)
  
  # Metadata definitions
  str(bins$Name)
  str(features_blue[1])  
  
  # Initialize the H5 file
  fileH5 <- h5file("test.h5")
  
  # Write the input data frame in (it would be the reconstructed)
  # ["data_frame_in"] = data_frame_in
    # TODO!
    # Error in GetTypechar(data) : 
    # All elements of list must be vectors of the same data type.
  
  # Write each color using a subfunction
  base_str = "features"
  write.hdf5.per.color(base_str, features_blue, "blue", bins, fileH5)
  write.hdf5.per.color(base_str, features_red, "red", bins, fileH5)
  
  h5close(fileH5)
  
}

write.hdf5.per.color = function(base_str, features, color, bins, fileH5) {
  
  columnnames = colnames(bins)
  if (!identical(columnnames[1], 'Name')) {
    warning('First column should be named Name, otherwise this HDF5 writing fails')
  }
  
  for (i in 1:length(bins)) { # Bin Names (e.g. baseline, max constriction, etc.)
    
    # https://stackoverflow.com/questions/7201341/how-can-2-strings-be-concatenated
    save_path_base = paste(base_str, color, bins$Name[1], sep="/") # e.g. "features/blue/Baseline"
    
    for (j in 1:length(columnnames)) { # Name, Method, Start, End, StartString

      if (j == 1) {
        # skipping Name  
      } else if (j > 1) {
        save_path = paste(save_path_base, columnnames[j], sep="/")
        data_to_write = bins[[columnnames[j]]][i]
        cat(i, ' ', j, '\n')
        str(save_path)
        str(data_to_write)
        fileH5[save_path] = data_to_write
      } 
      
    } # end of j
    
  } # end of i
  
  # fileH5[]
  
}