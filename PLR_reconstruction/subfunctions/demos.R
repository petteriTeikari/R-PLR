# AMELIA II ---------------------------------------------------------------

  # https://cran.r-project.org/web/packages/Amelia/vignettes/amelia.pdf  

  require(Amelia)
  data(freetrade)
  
  # To create multiple imputations in Amelia, we can simply run
  a.out <- amelia(freetrade, m = 5, ts = "year", cs = "country")
  