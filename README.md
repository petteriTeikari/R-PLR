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

See the [Prerequisites for R and Github](https://github.com/petteriTeikari/R-PLR/wiki/Prerequisites-for-R-and-Github) if R and Git(hub) are new beasts to you

## How to start using

Go to the folder on your local machine where you want this repository to be placed, and open terminal there (Command propmpt in Windows, Terminal in Linux/[Mac](https://macpaw.com/how-to/use-terminal-on-mac)

`git clone --recurse-submodules https://github.com/petteriTeikari/R-PLR`

## How to make sure that you have the latest repository

Go to the directory on your command window / Git Bash / Git-Cola / etc:

`git pull`

### Possible error here

If you have edited some of your files after your last pull, and you are trying to pull "on top of it" (i.e. replacing the contents of your changes), the Git will stop and warn about a conflict a there is no way to automatically do conflict resolving. You can delete for example that single conflicting file from your *local folder* if you do not care about your own changes and just want the latest version.

*Note!* Even an extra space or a line change is considered a change

### How-to-update the repo with the submodules (if we would have any)

`git submodule update --recursive --remote`
 
# "MODULES"

* [Clean single PLR recordings](https://github.com/petteriTeikari/R-PLR/wiki/Clean-the-recordings-(single-file)), i.e. remove artifacts, resample to same length, impute the missing values, denoise the recordings, and decompose for data augmentation purposes

* [Statistical analysis for the PLR Recordings](https://github.com/petteriTeikari/R-PLR/wiki/Stat-analysis-for-PLR), i.e. plot your density and box plots, and the averaged PLR traces.
