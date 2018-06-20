# INIT ----------------------------------------------------------------

  library(ggplot2)

  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, 'PLR_IO', fsep = .Platform$file.sep)
  
  # SOURCE SUBFUNCTIONS
  source(file.path(IO_path, 'define_pupillometerFilesToProcess.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'define_pupillometerFileformat.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'import_pupildata.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'process_videoFile.R', fsep = .Platform$file.sep))
  
  # Debugging the "SERI syntax"
  pattern_to_find = "*_BR.csv"
  data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA'
  main_data_path_out = file.path(data_path, '..', 'DATA_OUT', fsep = .Platform$file.sep)
  if (dir.exists(main_data_path_out) == FALSE) {
    cat('Creating the directory for DATA output')
    dir.create(main_data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  video_data_path_out = file.path(main_data_path_out, 'VIDEO', fsep = .Platform$file.sep)
  
  if (dir.exists(video_data_path_out) == FALSE) {
    cat('Creating the directory for DATA output')
    dir.create(video_data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }

# Go through the video files ----------------------------------------------------------------

  videofiles_to_process = define.pupillometerFilesToProcess(data_path, pattern_to_find, 'video')
  
  # Go through the video files
  All = lapply(videofiles_to_process, function(filepath){
    process.videoFile(data_path, video_data_path_out, filepath, IO_path)
  })
