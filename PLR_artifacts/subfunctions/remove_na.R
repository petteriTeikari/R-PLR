remove.na <- function(df, nan_indices) {
  
  time_trimmed = df$time[!nan_indices]
  pupil_trimmed = df$pupil[!nan_indices]
  
  df_out = data.frame(time=time_trimmed, pupil=pupil_trimmed)
  
}