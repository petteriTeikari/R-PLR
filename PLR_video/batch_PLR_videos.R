batch.PLR.videos = function(data_path = NA, RPLR_video_path = NA,
                            parameters, process_only_unprocessed = FALSE,
                            path_check_for_done, RPLR_paths) {
  
  
  # INIT ----------------------------------------------------------------
  
    library(ggplot2)
  
    # Define Paths
    if (is.na(RPLR_video_path)) {
      script.dir <- dirname(sys.frame(1)$ofile)
    } else {
      script.dir = RPLR_video_path    
    }
    
    source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
    IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  
    # SOURCE SUBFUNCTIONS
    source(file.path(IO_path, 'define_pupillometerFilesToProcess.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'define_pupillometerFileformat.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'import_pupildata.R', fsep = .Platform$file.sep))
    source(file.path(IO_path, 'export_pupil_dataframe_toDisk.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'process_videoFile.R', fsep = .Platform$file.sep))
    
    # Debugging the "SERI syntax"
    pattern_to_find = "*_BR.csv"
   
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
    
    if (process_only_unprocessed) {
      indices_undone = check.for.done.filecodes(videofiles_to_process, path_check_for_done)
      videofiles_to_process = videofiles_to_process[indices_undone]
    }
    
    # Go through the video files
    All = lapply(videofiles_to_process, function(filepath){
      process.videoFile(data_path, video_data_path_out, filepath, IO_path)
    })
    
    cat('ALL VIDEO FILES IMPORTED')

}