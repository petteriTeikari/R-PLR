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

You can go line-by-line with `Run Selected Line(s) Ctrl+Enter`

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

And make sure that the file `Master_File_De-Identified_Copy_For_Petteri.xlsx` is found from the `../` (one folder down) in relation to the `TEST_IN` 

### How to contribute to the development

The required packages are not at the moment collected under one subfunction (i.e. `import.and.install.libraries = function()` in https://github.com/petteriTeikari/R-PLR/blob/master/clean_and_reconstruct_all_PLR.R) so you could collect the packages that you needed to install to make this repo work and provide that?


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

Basically do your corrections, or push directly `Save to Disk` if there are no errors, and then push the `Reload` (upper panel). The disk saving automatically moves the processed away from input, and the Reload just reads in the next undone file.  

## 3) Resample to the same time vector

Not actually interpolating/resampling, just re-arranging the samples to common time vector with bunch of NAs still here

`batch.PLR.resample()`

## 4) Imputes the missing values (NAs)

Imputes the missing values with MissForest. Takes some time for many files

`batch.AnalyzeAndReImpute()`

### 4b) Check the quality of the imputation

Again if the imputation have hallucinated weird stuff especially for long durations of missing data, the machine learning will have harder time coping with this, so **please check again**

Run `R-PLR/apps_Shiny/inspect_outliers/server.R` with the `mode` set to `imputation`

#### IDEA

Exactly the same as before with the outlier inspection. Now again, you need to check the path, **line 22**

```R
} else if (identical(mode, 'imputation')) {
path = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/TEST_OUT/outlier_free/imputation_final'
```

, and **uncomment** the following line

```R
mode = 'imputation'
```
Basically do your corrections, or push directly `Save to Disk` if there are no errors, and then push the `Reload` (upper panel). The disk saving automatically moves the processed away from input, and the Reload just reads in the next undone file.  


## 4c) Re-impute the values after manual check

`batch.AnalyzeAndReImpute()`

## 5) Decompose the traces with EMD 

This decomposition is good for denoising, and then the loFreq / hiFreq decomposition is useful for data augmentation later on as well 

`batch.EMD.decomposition()`

**NOTE!** This is quite heavy computation and requires some time. Each file can be computed using individual core/thread so you save some time if you process multiple files.

## 5b) Again check for how to combine the IMFs

TODO! This should be automagicated (do a simple Random Forest classifier or something), should be rather simple, rather than wasting someone's time for this.

Run `R-PLR/apps_Shiny/inspect_EMD/server.R`

**Note** that now the decomposition label selection is quite arbitrary, see for example:

![EMD Example](https://github.com/petteriTeikari/R-PLR/blob/master/documentation/images/example_EMD.png "EMD Example")

**NOTE2** The signals on the right do not get updated dynamically (TODO!), And the normally distributed and non-normally distributed noise separation is done automatically. And in practice the denoised signal is the input signal - (all the noise components), and the low frequency, high frequency and base are used for data augmentation purposes with machine learning.

## 6) Combine different files together

Now the results are scattered to different folders and we combine them to have only one trace file per subject code

`combine.data.from.multiple.folders()`

## 7) Augment data for machine learning and compute 1st/2nd derivatives of trace

"Intelligently" guess how we could distort the signal in other words to have the machine learning be a bit more robust. And as we smooth the signal slightly more, the 1st (velocity) and 2nd order derivatives (differences) become a bit more robust to compute

`batch.data.decompose.for.augmentation()`

## 8) Compute the hand-crafted features from the outlier-free and denoised traces

Finally compute the hand-crafted features such as max constriction, slope, PIPR along with the Hilbert spectrum (time-frequency from EMD) and the fractal features.

`batch.PLR.analyze.reconstructions()`

### Possible problem! You need to get the `rgl` package working

#### Linux

`Configure: error: missing required header GL/gl.h`
`ERROR: configuration failed for package ‘rgl’ `

Solution:

`sudo apt-get install libglu1-mesa-dev`

#### Mac

Solution: https://stackoverflow.com/questions/33634871/installing-rgl-package-in-r-mac-osx-el-captian

Needs the `XQuartz` (on OSX), https://cran.r-project.org/web/packages/rgl/index.html

#### Windows

?

## THE END

You should get the following output folders from intermediate steps (as `.csv` files)

![OUTPUT Example](https://github.com/petteriTeikari/R-PLR/blob/master/test_data/test_data_OUT.png "OUTPUT Example")

And you can check out the contents of the `TEST_OUT_180815.zip`

**NOTE!** Now you can probably notice that the `AD01` was not processed at all as the `PLR_video` part had an input pattern for `PLRxxxx`, so you either has to rename the input folders (EASIER) or modify the code to accommodate all these variations in input files (MORE TIME-CONSUMING)

**NOTE2** You probably noticed that there are a lot of steps that could have been optimized more to make your life less annoying, like not having to change all the paths each time you run the script on another computer.

## UPGRADE

Also there are additional pains involved in making this cross-platform (Windows/Linux/Mac) and forcing clinicians to install packages that can be a bit tricky to get working (namely the Mac experience).

### 1) SSH Option
You could put this to one lab desktop and run everything over a SSH (https://cran.r-project.org/web/packages/ssh/vignettes/intro.html) 

### 2) Shiny Web App
or at some point even deploy this as web application (would be debugged a lot better than the current version, https://shiny.rstudio.com/articles/deployment-web.html)


