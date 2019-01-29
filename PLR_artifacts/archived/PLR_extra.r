PLR

# library(ggplot2) # plot

# Plot the raw input (ggplot2)
# ggplot(data=df, aes(x=frame, y=pupil, group=1)) + geom_point()

install.packages("devtools") # https://github.com/twitter/AnomalyDetection
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)

# BCP, this is massively slow
# library(bcp) # Bayesian Change Point detection
# values = bcp(y = df_hard_thr$pupil, x = df_hard_thr$frame)
