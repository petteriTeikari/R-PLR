accumulate.nans <- function(df, nan_indices, verbose_acc, time_point_list) {
  
  time = df$time

  if(missing(time_point_list)) {
    # cat(" NaN timepoint list initialized\n")
    # Instead of an empty list, better way to have an upper bound which is the size
    # of raw PLR in our case 
    # https://stackoverflow.com/questions/26508519/how-to-add-elements-to-a-list-in-r-loop#comment65195521_26508680
    # time_point_list = vector("list", length(time))
    
    # although we won't be adding that many entries, so this approach kinda ok
    time_point_list = list()
    
    new_time_points = time[nan_indices]
    if (verbose_acc == 1) {
      cat("  Adding ", length(new_time_points), " NA time values to the list")
    }
    time_point_list_out = c(time_point_list, new_time_points)
    if (verbose_acc == 1) {
      cat(" - New length = ", length(time_point_list_out), "\n")
    }
    return(time_point_list_out)
    
  } else {
    new_time_points = time[nan_indices]
    if (verbose_acc == 1) {
      cat("  Adding ", length(new_time_points), " NA time values to the list")
    }
    time_point_list_out = c(time_point_list, new_time_points)
    if (verbose_acc == 1) {
      cat(" - New length = ", length(time_point_list_out), "\n")
    }
    return(time_point_list_out)
  }
  
  # https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions
  
}