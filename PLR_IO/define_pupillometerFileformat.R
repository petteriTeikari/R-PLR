define.pupillometerFileformat <- function(path, filename) {
  
  # Basically a placeholder at the moment
  # TODO! Detect all possible column/row formats, so that they can be "abstractified"
  # correctly on the next step
  
  if_BRV = length(grep('_BR.csv', filename))
  
  if (if_BRV == 1) {
    file_format = "_BR"
  } else {
    cat("only '2column_test' defined now for debugging\n")
    file_format = "2column_test"
  }
  
}