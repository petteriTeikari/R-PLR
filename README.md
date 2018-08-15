# rPLR

The coming R package to be published. Remember the high-level overview there [PLR_Overview, formerly known as PLR Toolbox](https://github.com/petteriTeikari/PLR_Overview)

![Idea](https://github.com/petteriTeikari/rPLR/blob/master/documentation/images/scheme.png "Idea")

R packages tend to go to [Journal of Statistical Software](https://github.com/petteriTeikari/PLR_Overview), but you could publish this in some Elsevier journal as well and make pupillometry more easy to pick up by novices ultimately:

* [Computer Methods and Programs in Biomedicine](https://www.journals.elsevier.com/computer-methods-and-programs-in-biomedicine), IF = 2.503, e.g. [R-HRV](http://rhrv.r-forge.r-project.org/publications.html)
* [Computers in Biology and Medicine](https://www.journals.elsevier.com/computers-in-biology-and-medicine), IF = 1.836, e.g. [ETHOWATCHER](https://www.journals.elsevier.com/computers-in-biology-and-medicine/most-downloaded-articles)
* [Journal of Neuroscience Methods](https://www.journals.elsevier.com/journal-of-neuroscience-methods/), IF = 2.554, e.g [EEGLAB](https://doi.org/10.1016/j.jneumeth.2003.10.009)
* [Frontiers in Neuroinformatics](https://www.frontiersin.org/journals/neuroinformatics), IF=3.870, e.g. [OpenElectrophy](https://www.frontiersin.org/journals/neuroinformatics#articles)
* [HRV software](https://doi.org/10.3389/fphys.2016.00557) also published in [Frontiers in Physiology](https://www.frontiersin.org/journals/physiology), IF=4.134
* [SoftwareX](https://www.elsevier.com/about/press-releases/research-and-journals/elsevier-announces-the-launch-of-softwarex), IF=3.801, e.g. [RCrawler: An R package for parallel web crawling and scraping](https://doi.org/10.1016/j.softx.2017.04.004), see [Silicon Valley Scraping](https://youtu.be/FNyi3nAuLb0?t=2m10s) for non-hot dog / hot dog detection.

# Quick installation Guide

## How to start using

`git clone --recurse-submodules https://github.com/petteriTeikari/R-PLR`

## How-to-update the repo with the submodules

`git submodule update --recursive --remote`
 
# How-to-use

In theory the PLR files could be analyzed end-to-end from you putting the _"BR"_ to the Dropbox folder and then running that script:

**`https://github.com/petteriTeikari/R-PLR/blob/master/clean_and_reconstruct_all_PLR.R`**

You need to tweak the 3 paths manually as they now refer to my paths:

```R
paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
paths[['data_in']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA'
paths[['data_out']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT'
```
## Use the TEST DATA

If you do not access to full "database" (folder containing all the recording folders), extract the `test_PLR_data_180815.zip` from the folder `test_data` for example to folder `../TEST_IN` (can be whatever location, but now the initial path is set to this making your life slightly easier)

![Location](https://github.com/petteriTeikari/R-PLR/blob/master/test_data/test_data.png "Location")

Your data folders can then be made relative to the base folder (only need to change one directory)

```R
paths[['data_in']][['base']] = file.path(paths[['RPLR']][['base']], '..', 'TEST_IN', fsep = .Platform$file.sep)
paths[['data_out']][['base']] = file.path(paths[['RPLR']][['base']], '..', 'TEST_OUT', fsep = .Platform$file.sep) 
```

## 1) Import the traces

Basically just reads in the results and get rid of bunch of redundant R,G,B columns
`batch.PLR.videos()`

## 2) Reduce artifacts

Not the most intelligent algorithm at the moment
`batch.PLR.artifacts()`

### 2b) Check the quality of the artifact removal

**Make sure that the machine learning classifier gets only the good quality "ground truth" traces** The crappy ones without cleaning are easy to mix later on 

**TODO!** You could try to read all the paths from some config file as now you need to again define the path manually (line 17):

```R
path = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/TEST_OUT/outlier_free'
```

Run `R-PLR/apps_Shiny/inspect_outliers/server.R`

#### Example walk-throughs

Have a look of the videos.

* SERI 2018 paradigm: https://youtu.be/okPJ8fn_17Q
* SERI 2017 dataset and paradigm: https://youtu.be/yVGh8p-3Ko0

1st column sets the ROI Zoom, 2nd column either includes red points, or excludes blue points, 3rd column could be updated dynamically with the line connection the points (TODO!)

## 3) Resample to the same time vector

Not actually interpolating/resampling, just re-arranging the samples to common time vector with bunch of NAs still here

`batch.PLR.resample()`

## 4) Imputes the missing values (NAs)

Imputes the missing values with MissForest. Takes some time for many files

`batch.AnalyzeAndReImpute()`

### 4b) Check the quality of the imputation

Again if the imputation have hallucinated weird stuff especially for long durations of missing data, the machine learning will have harder time coping with this, so **please check again**

Run `R-PLR/apps_Shiny/inspect_outliers/server.R` with the `mode` set to `imputation`

## 4c) Re-impute the values after manual check

`batch.AnalyzeAndReImpute()`

## 5) Decompose the traces with EMD 

This decomposition is good for denoising, and then the loFreq / hiFreq decomposition is useful for data augmentation later on as well 

`batch.EMD.decomposition()`

## 5b) Again check for how to combine the IMFs

TODO! This should be automagicated, should be rather simple, rather than wasting someone's time for this

Run `R-PLR/apps_Shiny/inspect_EMD/server.R`

## 6) Combine different files together

Now the results are scattered to different folders and we combine them to have only one trace file per subject code

`combine.data.from.multiple.folders()`

## 7) Augment data for machine learning and compute 1st/2nd derivatives of trace

"Intelligently" guess how we could distort the signal in other words to have the machine learning be a bit more robust. And as we smooth the signal slightly more, the 1st (velocity) and 2nd order derivatives (differences) become a bit more robust to compute

`batch.data.decompose.for.augmentation()`

## 8) Compute the hand-crafted features from the outlier-free and denoised traces

Finally compute the hand-crafted features such as max constriction, slope, PIPR along with the Hilbert spectrum (time-frequency from EMD) and the fractal features.

`batch.PLR.analyze.reconstructions()`
