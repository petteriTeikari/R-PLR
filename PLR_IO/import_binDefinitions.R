import.binDefinitions = function (config_path) {
  
  # hardcoded, so do not change the filename on the disk
  filename = 'bins.csv'
  
  # https://stackoverflow.com/questions/23543825/r-read-table-how-can-i-read-the-header-but-also-skip-lines
  dat = read.table(file.path(config_path, filename, fsep = .Platform$file.sep), 
                   header=TRUE, sep=',', stringsAsFactors=FALSE)
  
  # How to access the name
  # bin_names = dat$Name
  # methods = dat$Method
  # start_times = dat$Start
  # start_times = dat$End
  # descriptions = dat$Description
  
  # TODO! See later how this could be defined a bit more elegantly, e.g.
  # dat$Name$Start would give you the start time

  return(dat)
  
}