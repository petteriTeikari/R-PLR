# BATCH PROCESSING OF PLRs

# Initialize --------------------------------------------------------------

# Define Paths
script.dir <- dirname(sys.frame(1)$ofile)
data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
IO_path = file.path(script.dir, 'PLR_IO', fsep = .Platform$file.sep)

# Libraries needed
library(forecast)
# install.packages("forecast")
library(ggplot2)
# library(shiny)

# SOURCE SUBFUNCTIONS
source(file.path(source_path, 'process_artifact_file.R', fsep = .Platform$file.sep))
source(file.path(IO_path, 'define_pupillometerFilesToProcess.R', fsep = .Platform$file.sep))

# Debugging the "SERI syntax"
pattern_to_find = "*_BR_video.csv"
data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/VIDEO'
data_path_out = file.path(data_path, '..', 'outlier_free', fsep = .Platform$file.sep)
if (dir.exists(data_path_out) == FALSE) {
  cat('Creating the directory for DATA output')
  dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# DEFAULT PARAMETERS
param = list()
param['blink_hard_threshold'] = 9 # smaller values than this are blinks
param['sigma_multip'] = 1.96 # times stdev for spline filter for outlier rejection
param['debug_clean_on'] = FALSE # if you want to see intermediate steps (not implemented yet)
param['plot_output'] = FALSE # if you want to save the plotted graphs

# Go through the files ----------------------------------------------------------------

  # Get all the files in the data_path
  # For GUI selector (if you want/need), see:
  # https://www.rdocumentation.org/packages/utils/versions/3.4.1/topics/choose.dir
  files_to_process = define.pupillometerFilesToProcess(data_path, pattern_to_find, 'artifacts')
  
  # The real advantage of this "loopless approach" is that you can easily run 
  # it in parallel using mclapply (from multicore package) 
  # instead of lapply. Or parLapply from snow.
  All = lapply(files_to_process, function(filepath){
    process.artifact.file(data_path, data_path_out, IO_path, filepath, param)
  })

  
