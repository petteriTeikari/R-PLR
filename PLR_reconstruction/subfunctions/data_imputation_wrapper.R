data.imputation.wrapper = function(pupil_df, t, y, error_frac, weights_norm, filecodes, param_imp, master_data, found_from_master) {
  
  # BACKGROUND and Packages -------------------------------------------------------------
  
    # Comparison of different Methods for Univariate Time Series Imputation in R
    # Steffen Moritz et al. (2015)
    # https://arxiv.org/abs/1510.03924
  
      # Furthermore, we experimentally compare the R functions on different time series using four different ratios of missing data. 
      # Our results show that either an interpolation with seasonal kalman filter from the zoo package or a linear interpolation on 
      # seasonal loess decomposed data from the forecast package were the most effective methods for dealing with missing data 
      # in most of the scenarios assessed in this paper.
  
    # imputeTS: Time Series Missing Value Imputation in R
    # Steffen Moritz et al. (2015)
    # https://pdfs.semanticscholar.org/cf38/7a44ef973ac37568a8ca482b4add12b646eb.pdf
  
      # The imputeTS package specializes on univariate time series imputation. It offers multiple
      # state-of-the-art imputation algorithm implementations along with plotting functions for time series
      # missing data statistics. While imputation in general is a well-known problem and widely covered by R
      # packages, finding packages able to fill missing values in univariate time series is more complicated.
  
      # On CRAN there are several packages solving the problem of imputation of multivariate data. Most
      # popular and mature (among others) are AMELIA (Honaker et al., 2011), mice (van Buuren and Groothuis-Oudshoorn, 2011), 
      # VIM (Kowarik and Templ, 2016) and missMDA (Josse and Husson, 2016). However, since these packages are designed 
      # for multivariate data imputation only they do not work for univariate time series.
  
    # 'mstdi' 
    # https://cran.r-project.org/web/packages/mtsdi/index.html
  
      # This is an EM algorithm based method for imputation of missing values in multivariate
      # normal time series. The imputation algorithm accounts for both spatial and temporal correlation
      # structures.
  
    # jeffwong/imputation
    # https://github.com/jeffwong/imputation
  
      # Missing data imputation (also known as matrix completion) is an extremely difficult science 
      # that tries to fill in missing values of a dataset with the best guess. Recently, it was 
      # popularized by the Netflix Challenge, where a matrix of Netflix users and their movie ratings 
      # were presented to the data science community to see if algorithms could be developed to predict 
      # how a user would rate a certain movie that the user has not yet seen.
  
    # Amelia II: A Program for Missing Data (2011)
    # https://gking.harvard.edu/amelia
    # https://cran.r-project.org/web/packages/Amelia/vignettes/amelia.pdf
  
      # Amelia II "multiply imputes" missing data in a single cross-section (such as a survey), 
      # from a time series (like variables collected for each year in a country), or from a 
      # time-series-cross-sectional data set (such as collected by years for each of several countries). 
      # Amelia II implements our bootstrapping-based algorithm that gives essentially the same answers as the 
      # standard IP or EMis approaches, is usually considerably faster than existing approaches and can handle many more variables. 
  
    # Model Agnostic Time Series Analysis via Matrix Estimation, Anish Agarwal et al. (2018)
    # https://arxiv.org/abs/1802.09064
  
      # We propose an algorithm to interpolate and forecast a time series by transforming the observed time series 
      # into a matrix, utilizing matrix estimation to recover missing values and de-noise observed entries, and 
      # performing linear regression to make pre- dictions. This algorithm is a consequence of a surprising and 
      # powerful link that we establish between (a single) time series data and matrix estimation. 
      # Subsequently, our algorithm is model agnostic with respect to the time dynamics and noise in the observations 
      # (similar to the recent matrix estimation literature). 
    
      # Using synthetic and real-world data, we exhibit the efficacy of our algorithm with
      # respect to the state-of-art software implementation available through R (AMELIA). Our finite sample analysis
      # agrees with these experimental results. Lastly, we demonstrate that our method can provably recover
      # the hidden state of dynamics within HMMs, which could be of interest in its own right.

  # INPUT CHECK -------------------------------------------------------------

    # param_imp = param[['imputation']]
    number_of_nas_in = sum(is.na(y))
    
    # Input is a vector
    if (length(dim(t)) == 0) {
      na_ratio = number_of_nas_in / length(y)
    } else { # matrix
      na_ratio = number_of_nas_in / (dim(y)[1] * dim(y)[2])
    }
    
    
    cat('The input matrix for IMPUTATION / MATRIX COMPLETION has', 
        round(100*na_ratio, digits=2), '% NAs of all the values\n')

  # IMPUTATION(s) -----------------------------------------------------------------------

    no_of_different_imputation_methods = length(param_imp[['methods']])
    imp_list = list()
    
    for (i in 1 : no_of_different_imputation_methods) {
      
      method_name = param_imp[['methods']][i]
      
      # AMELIA II https://gking.harvard.edu/amelia
      if (identical(method_name, 'AMELIA')) {
        imp_list[[method_name]] = imp.amelia.wrapper(pupil_df, t, y, error_frac, weights_norm, filecodes, param_imp)
      
      # imputeTS  https://github.com/SteffenMoritz/imputeTS 
      } else if (grepl('imputeTS', method_name)) {
        imp_list[[method_name]] = imp.imputeTS.wrapper(pupil_df, t, y, error_frac, weights_norm, filecodes, method_name, param_imp)
        
      } else if (grepl('none', method_name)) {
        cat('Skipping imputation at this point\n')
        imp_list[[method_name]][['pupil']] = y
        imp_list[[method_name]][['error']] = error_frac
        imp_list[[method_name]][['weights']] = weights_norm
        
      } else {
        warning('You have typo for imputation method? "', param_imp[['methods']][i], '" not implemented yet')
        
      }
      
    }
    
  # Placeholder
  return(imp_list)
  
}
