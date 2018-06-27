library(ggplot2)
library(grid)
library(gridExtra)

trials = 100
nimf = 15
noise.amp = estimate.input.noise.amplitude.for.EMD(t, y)

# test how the lag affects the results
# https://www.rdocumentation.org/packages/hht/versions/2.1.3/topics/Sig2IMF
# -> https://www.rdocumentation.org/packages/hht/versions/2.1.3/topics/InstantaneousFrequency
# ---->  https://www.rdocumentation.org/packages/base/versions/3.5.0/topics/diff
lag_vector = c(1,2,3)

emd.results = list()
for (l in 1 : length(lag_vector)) {
  
  col_name = paste0('lag', lag_vector[l])
  emd.results[[col_name]] = CEEMD(y, t, noise.amp, max.imf = nimf,
                                  trials, diff.lag = lag_vector[l],
                                  verbose = TRUE)
  
}

# PLOT NOW
par(mar=c(1,1,1,1))
PlotIMFs(emd.results[[1]])

# Check the noise removed
IMFs = emd.results[[2]]$imf
noise = rowSums(IMFs[,1:3])
denoised_2 = rowSums(IMFs[,4:10]) + emd.results[[2]]$residue

# where the residual is the biggest
noise_ROI_ind = which.max(abs(noise))
margin = 20
n1 = noise_ROI_ind - margin
n2 = noise_ROI_ind + margin

# Plot
df_plot = data.frame(t = t, input = y, noise = noise, denoised = denoised_2)

p = list()

p[[1]] = ggplot(df_plot, aes(t)) + 
          geom_line(aes(y = input, colour='Input')) +
          geom_line(aes(y = denoised_2, colour='Denoised'))

p[[2]] = ggplot(df_plot, aes(t)) + 
          geom_line(aes(y = noise, colour='noise'))

p[[3]] = ggplot(df_plot, aes(t)) + 
          geom_line(aes(y = input, colour='Input')) +
          geom_line(aes(y = denoised_2, colour='Denoised')) + 
          xlim(t[n1], t[n2])

p[[4]] = ggplot(df_plot, aes(t)) + 
          geom_line(aes(y = noise, colour='noise')) + 
          xlim(t[n1], t[n2])

no_of_cols = 2
do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
