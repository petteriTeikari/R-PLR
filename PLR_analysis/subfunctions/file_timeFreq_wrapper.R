file.timeFreq.wrapper = function(filename_path, data_path_out, param, debug = FALSE, 
                                 from_dataframe = FALSE, df_in = NA, filecode = NA) {

  # INITIALIZE --------------------------------------------------------------

    if (!from_dataframe) {
      
      # Check input
      just_filename = tail(strsplit(filename_path, .Platform$file.sep)[[1]], 1)
      filecode = strsplit(just_filename, '_')[[1]][1]
      cat(filecode, ' ')
      
      # READ IN
      df_in = read.csv(filename_path)
      
    } else {
      
      # df_in given as input
      
    }

    if (sum(is.na(df_in$pupil))) {
      warning('Your input "y" has NA values!, More exactly = ', 
              100*sum(is.na(df_in$pupil))/length(df_in$pupil), '% of values are NA')
    }
    
    # take a subset with only the IMF
    subset_out = pick.IMF.subset.from.df(df_in)
      df_subset = subset_out[[1]]
      indices_comp = subset_out[[2]]
    
    # As we only saved time, residue, imfs, instantaneous amplitudes and frequencies to
    # disk, and it is easier to use the tools from hht, we fill the missing values back to the list
    # to make the computations a bit easier
    CEEMD.result = from.signals.make.emd.result(df_in$missForest, df_subset, indices_comp)
    
  # COMPUTATIONS ------------------------------------------------------------

    # Render Hilbert Spectrogram
    # https://www.rdocumentation.org/packages/hht/versions/2.1.3/topics/HHRender
    
    # Calculate spectrogram
    dt = 0.4 # time step
    dfreq = 0.02 # frequency step
    hgram <- HHRender(CEEMD.result, dt, dfreq, verbose = FALSE) # ~2.17 seconds per file at home AMD
    
    # custom field
    hgram[['dt']] = dt
    
    # Hilbert spectrum
    hspec = HHSpectrum(CEEMD.result, dfreq, verbose = FALSE)
    hspec$amplitude.summed = rowSums(hspec$amplitude) # sum all IMFs
    # HHSpecPlot(hspec, show.fourier = TRUE, scale.fourier = TRUE)
    # ind = hspec$frequency > 0.5 # Hz
    # plot(hspec$frequency[ind], hspec$amplitude.summed[ind], type='l')
    
    # Plot it
    # time.span <- c(5, 10)
    # freq.span <- c(0, 1.5)
    # amp.span <- c(1e-6, 2.5e-5)
    # HHGramImage(hgram, freq.span = freq.span)
    
    file_out = paste0(filecode, '_hgram.RData')
    save(hgram, hspec, file = file.path(data_path_out, file_out, fsep = .Platform$file.sep))
    
    return('f')
    
  # THEORY ------------------------------------------------------------------
    
    # Some practical intro from Matlab
    # https://www.mathworks.com/help/signal/examples/practical-introduction-to-time-frequency-analysis.html#d119e6123
    
    # See for example "Wavelet analysis of heart rate variability: 
    # Impact of wavelet selection" Alexander Tzabazis et al. (2018)
    # on how wavelet selection impacts HRV Analysis
    # https://doi.org/10.1016/j.bspc.2017.09.027
    
    # SOUNDS RATHER FAMILIAR, THE REMARKS: 
    # "Kernels used for wavelet analysis need to be reported to make results comparable."
    # "Specific kernels might perform better depending on application."
    
    # They used the MODWPT
    # which can be found e.g. from
    # https://www.rdocumentation.org/packages/wmtsa/versions/2.0-3/topics/wavMODWPT
    # and they used the package RHRV:
    # http://rhrv.r-forge.r-project.org/
    
    # For more pupillometry papers, see e.g.
    # https://doi.org/10.1109/ICMEAE.2017.33 : 
    
      # "The PD recordings, as bio-signals, are ‘3N’ –
      # Nonstationary, Nonlinear, and Noisy. The signal's
      # statistical characteristics change with time so the
      # pupil activity is essentially non-stationary. The
      # sources of the observed non-stationarity in a
      # pupillogram are varied and of origins difficult to
      #  determine. Casual influences of the external stimuli,
      # reflections of switching of the inherent metastable
      # states of neural assemblies during brain functioning,
      # or transmissions of deeper brain structures with
      # different and changeable intensity"
    
      # Morlet wavelet compared to HHT/EMD:
    
      # HHT is a complete, adaptive or data driven, 
      # highly efficient method which can
      # determine the precise instantaneous frequency
      # without imposing prior assumptions of linearity and
      # stationarity on the data as in Fourier based methods. 
    
      # On the other hand, DWT is a complete, a priori and
      # orthogonal representation which is not able to reveal
      # the instantaneous frequency, but can be used to detect
      # the relative change in the signal´s frequency content.
      # It has advantages over traditional Fourier transforms
      # in representing functions that have discontinuities
      # and sharp peaks and for accurately decomposing and
      # reconstructing finite, non-periodic, and nonstationary
      # signals. However, because it is not an
      # adaptive method, it can yield misleading timefrequency
      # representations of the signal when the
      # mother wavelet does not meet the signal. 
    
    # https://doi.org/10.1016/j.protcy.2014.10.237 : 
    
      # "We analyze high-resolution, non-invasive 
      # pupillometric signals, both in a basal condition and as a response to visual flashes 
      # or to the application of a cold stimulus. We use Singular Spectrum Analysis (SSA) to 
      # identify the frequencies of interest related to the actions of the SNS and PsNS. 
      # In addition, and as a complementary means of frequency analysis, we also use 
      # the more classical wavelets analysis. We find out that SSA is an ideal tool for the identification 
      # of the desired frequencies in stake."
      
      # "Unlike Fourier analysis, which assumes sinusoidal bases, or Wavelet analysis, 
      # for which we need to determine a priori a suitable set of bases, SSA estimates these 
      # directly from the data’s dynamics. This renders SSA a good candidate for data exploration, 
      # when little information exists on the sought dynamics. The outcome of SSA can as well be 
      # used to optimize the choice of wavelet bases. Another advantage of SSA is that only 
      # two parameters need to be determined beforehand: in the decomposition stage, 
      # the window length; in the reconstruction, the grouping  strategy 
      # for the estimated components."
      
      # "Interestingly, and although it is not designed for that purpose, pupillometric information 
      # could also estimate other frequencies of interest. In fact, in the basal experimental setup, 
      # we found that respiration had a frequency in the range 0.2-0.3 Hz [19], 
      # and the heart rate between 1 and 1.5 Hz [20]. The latter results were validated via simultaneous
      # electrocardiographic recordings. There was a clear agreement between the electrophysiological measurements
      # (1.47±0.22 Hz ) and the SSA processed pupillometric signals (1.40±0.19 Hz)."
      
      # [19] L. Sherwood, Fundamentals of Physiology: A Human Perspective, 3rd ed. Thomson Brooks/Cole, 2006, p. 380.
      # [20] V. Fuster, R. A. O’Rouke, and R. W. Alexander, Hurst’s the Heart, 10th ed. McGraw-Hill, 2001, pp. 78–79.
    
    # https://doi.org/10.1007/978-3-319-00846-2_200
    
    # https://doi.org/10.1177/154193121005400315 : 
    
      # "PUPIL WITH SKIN CONDUCTANCE.
      # Nakayama and Shimizu (2002 & 2004) found the power spectrum density of 
      # PD between 0.1- 0.5 Hz and 1.6 - 3.5 Hz 
      # to increase with difficult mental arithmetic."
    
    # https://doi.org/10.1145/968363.968381 :
    
    # https://doi.org/10.1109/EMBC.2015.7319797 : 
    
      # "For personal experience,
      # we chose a standard symmetric wavelet (’sym10’)."
    
    
}
