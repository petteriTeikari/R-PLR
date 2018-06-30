  fractal.MFDFA.wrapper = function(t, y, plot_on = FALSE) {
  
    # For background, see:
    # "Introduction to multifractal detrended fluctuation analysis in Matlab"
    # https://doi.org/10.3389/fphys.2012.00141
    # and see the examples in Matlab
    
    # the fractaldata.mat of Ihlen (2012) is found from fractaldata.csv
    # fractaldatapath = file.path(source_path, 'test_data', fsep = .Platform$file.sep)
    # df_fractal = read.csv(file.path(fractaldatapath, 'fractaldata.csv', fsep = .Platform$file.sep))
    # y_multifractal = df_fractal$multifractal
    
    scmin=16;
    scmax=1024;
    scres=19;
    exponents=seq(from=log2(scmin), to=log2(scmax));
    scale=round(2.^exponents);
    
    q = seq(from=-5, to=5, length.out=101);
    m = 1
   
    # To test the implementation with "known results"
    # mfdfa = MFDFA(y_multifractal, scale, m=m, q)
    
    mfdfa = MFDFA(y, scale, m=m, q)
    
      # Output consists of
      #   • Hq Hurst exponent.
      #   • tau_q Mass exponent.
      #   • spec Multifractal spectrum (α and f(α))
      #   • Fq Fluctuation function.
    
    # How to define?
    error = NA
    
    # We can get some scalars to describe the signal
    n = 2
    list_of_dfs_out = vector("list", n) 
    
    list_of_dfs_out[[1]] = data.frame(value = max(mfdfa$spec$hq) - min(mfdfa$spec$hq),
                                     uncertainty = error,
                                     name = 'MFDFA_spectrum_width_hq',
                                     bin_start = 1, bin_end = length(y),
                                     offset = NA,
                                     stringsAsFactors = FALSE)
    
    max_index = which.max(mfdfa$spec$Dq)
    list_of_dfs_out[[2]] = data.frame(value = mfdfa$spec$hq[max_index],
                                      uncertainty = error,
                                      name = 'MFDFA_spectrum_peak_hq',
                                      bin_start = 1, bin_end = length(y),
                                      offset = NA,
                                      stringsAsFactors = FALSE)
    
    
    if (plot_on) {
      plot.mfdfa(mfdfa)
    }
    
    return(list(mfdfa, list_of_dfs_out))
    
  }
  
  
# Plot the MFDFA results
plot.mfdfa = function(mfdfa) {
  
  dev.new()
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),heights=c(4, 4))
  b = mfdfa
  par(mai=rep(0.8, 4))
  
  ## 1st plot: Scaling function order Fq (q-order RMS)
  
    # Corresponds to Figure 8a of 
    # https://doi.org/10.3389/fphys.2012.00141
    
    p1<-c(1,which(q==0),which(q==q[length(q)]))
    plot(log2(scale),log2(b$Fqi[,1]), pch=16, col=1, axes = F, xlab = "scale",
         ylab=expression('log'[2]*'(F'[q]*')'), cex=1, cex.lab=1.6, cex.axis=1.6,
         main= "Fluctuation functionFq",
         ylim=c(min(log2(b$Fqi[,c(p1)])),max(log2(b$Fqi[,c(p1)]))))
    lines(log2(scale),b$line[,1], type="l", col=1, lwd=2)
    grid(col="midnightblue")
    axis(2)
    lbl<-scale[c(1,floor(length(scale)/8),floor(length(scale)/4),
                 floor(length(scale)/2),length(scale))]
    att<-log2(lbl)
    axis(1, at=att, labels=lbl)
    for (i in 2:3){
      k<-p1[i]
      points(log2(scale), log2(b$Fqi[,k]), col=i,pch=16)
      lines(log2(scale),b$line[,k], type="l", col=i, lwd=2)
    }
    legend("bottomright", c(paste('q','=',q[p1] , sep=' ' )),cex=2,lwd=c(2,2,2),
           bty="n", col=1:3)
  
  ## 2nd plot: q-order Hurst exponent
  
    # Corresponds to Figure 8d of 
    # https://doi.org/10.3389/fphys.2012.00141
    
    plot(q, b$Hq, col=1, axes= F, ylab=expression('h'[q]), pch=16, cex.lab=1.8,
         cex.axis=1.8, main="Hurst exponent", ylim=c(min(b$Hq),max(b$Hq)))
    grid(col="midnightblue")
    axis(1, cex=4)
    axis(2, cex=4)
  
  ## 3rd plot: q-order Mass exponent
  
    # Corresponds to Figure 9b of 
    # https://doi.org/10.3389/fphys.2012.00141
    
    plot(q, b$tau_q, col=1, axes=F, cex.lab=1.8, cex.axis=1.8,
         main="Mass exponent",
         pch=16,ylab=expression(tau[q]))
    grid(col="midnightblue")
    axis(1, cex=4)
    axis(2, cex=4)
  
  ## 4th plot: Multifractal spectrum
  
    # Note! The confusing naming of the labels in the original example if you are 
    # following the paper by Ihlen (2012)
    
    # We plot hq as x, and Dq as y, but they are labeled as alpha and f(alpha)
    # q-order singularity exponent (hq)
    # q-order singularity dimension (Dq)
    # difference between the maximum and minimum hq, is called the multifractal spectrum width
    
    # Corresponds to Figure 9c of 
    # https://doi.org/10.3389/fphys.2012.00141
    plot(b$spec$hq, b$spec$Dq, col=1, axes=F, pch=16, main="Multifractal spectrum",
         ylab="Dq",cex.lab=1.8, cex.axis=1.8,
         xlab="hq")
    
    grid(col="midnightblue")
    axis(1, cex=4)
    axis(2, cex=4)
    x1=b$spec$hq
    y1=b$spec$Dq
    rr<-poly_fit(x1,y1,4)
    mm1<-rr$model1
    mm<-rr$polyfit
    x2<-seq(0,max(x1)+1,0.01)
    curv<-mm[1]*x2^4+mm[2]*x2^3+mm[3]*x2^2+mm[4]*x2+mm[5]
    lines(x2,curv, col="red", lwd=2)
  
}
  
# Polyfit subfunction for the plot.mfdfa
poly_fit<-function(x,y,n){
  
  formule<-lm(as.formula(paste('y~',paste('I(x^',1:n,')', sep='',collapse='+'))))
  res1<-coef(formule)
  poly.res<-res1[length(res1):1]
  allres<-list(polyfit=poly.res, model1=formule)
  return(allres)
}