# PLR_analysis

Go to command prompt and go to where you want this repository to be stored and write

 `git clone https://github.com/petteriTeikari/PLR_analysis.git`
 
 `git submodule update --recursive --remote` 
 
 `PLR_IO` defined as submodule as shared across the different blocks 
 
 and run `batch_PLR_traces.R` from your RStudio IDE
 
 ## For modifying the files
 
You could [create your own **branch**](https://github.com/Kunena/Kunena-Forum/wiki/Create-a-new-branch-with-git-and-manage-branches) with 
 
`git checkout -b maxDevelBranch` 
 
In practice this means that I and Max for example can independently work on this project and handle the merge at some point in some sort of organized manner. For a short background what branches means in [What's a branch? | Git Beginner's Guide for Dummies | Backlog](https://backlog.com/git-tutorial/stepup/stepup1_1.html), 
 
 # Short intro
 
 So this script will either go through :
 
 1) Single file
 2) Whole folder
 
 And the function [process_singlePLR.R](https://github.com/petteriTeikari/PLR_analysis/blob/master/subfunctions/process_singlePLR.R) is the "main function" for each "Reconstructed PLR" trace as outputted by the "PLR_artifacts" script.
 
 This script calls after I/O operations the script  [compute_PLR_features.R](https://github.com/petteriTeikari/PLR_analysis/blob/master/subfunctions/compute_PLR_features.R) that cycles through each hand-crafted features defined in the .CSV file [bins.csv](https://github.com/petteriTeikari/PLR_analysis/blob/master/config/bins.csv) which has at the moment just a couple of dummy examples of different kind of computations to be expected
 
Most of those values then define how the "data_bins" are defined there:
 [get_datapoints_per_bin.R](https://github.com/petteriTeikari/PLR_analysis/blob/master/subfunctions/get_datapoints_per_bin.R)

So you can see that define indices depend whether you define the time from the start of the file, in relation to the light onset or offset. 

Finally the features are computed for the defined "data bin" by: [compute_indiv_feature.R](https://github.com/petteriTeikari/PLR_analysis/blob/master/subfunctions/compute_indiv_feature.R) in which the `median/mean/min/max` operations are quite straightforward (NOTE! `min` is corresponds now to maximum constriction).

The definition of **latency** might need some additional smoothing (Even though the reconstructed PLR now is now a simple cross-validated spline fit that will be replaced eventually by the `deep learning method`, so that you have the PLR trace with uncertainties for each timepoint, as uncertainty should be bigger on outlier parts). So now the used uncertainties are "dummy ones" in preparation for the future.

The **slope of the linear fit** is defined simply, i.e. linear regression for the data bin which might result in suboptimal results if someone has a very steep constriction with many "flat time points"

![Linear Fit](https://github.com/petteriTeikari/PLR_analysis/blob/master/images/blind_linearFit.png "Example of suboptimal linear fit")
 
### Output example

Check the example of the output of the features computed based on the **bins.csv** file for all the files in the folder. 

![Example of features](https://github.com/petteriTeikari/PLR_analysis/blob/master/images/features_computed.png "Example of features")

Uncertainty value is a bit of a placeholder at the moment as they are not necessarily correctly computed (check the code for details). Now just add more rows to the **bins.csv** to define as many as features as you like and they should be automatically computed.

Now if you define something *funky*, you need to go to the code and define your computing **"method"** (defined now median, min, max, mean, and linearfit and timing). For example if you would like to analyze the fractal dimension of your trace, do some other decomposition for nice scalar values, you should define them first. See for example [Şen et al. 2014](https://doi.org/10.1007/s10916-014-0018-0), for EEG features.

![Example of EEG features](https://github.com/petteriTeikari/PLR_analysis/blob/master/images/EEG_features_table.png "Example of EEG features")

Also **note** that the code is split into many functions hopefully bringing modularity for code and **Easy customizing** if you think of new ways to *torture your data*, and see how does that affect the outcome. Like :
1) Normalizing to baseline using either **divisive** or **subtractive** method
2) Normalizing individually **both colors** to their corresponding pre-"light onset" periods rather than just for the **blue baseline**
3) And your imagination is the limit here :P
 
 
