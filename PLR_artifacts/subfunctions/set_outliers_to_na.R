set.outliers.to.na <- function(df, nan_indices) {
  
  df_out = df
  df_out$pupil[nan_indices] = NA
  
}