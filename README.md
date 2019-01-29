# rPLR

Collection of R functions for processing Pupillary Light Reflex (PLR) recordings.

Available *as it is*, not yet ready as usable package, coming with some glitches still.

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
