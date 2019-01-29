# PLR_video

Basically a placeholder for the coming upgraded video pupil segmentation/tracker algorithm.

Reads now the raw files, and does quick'n'dirty estimation of the discretization noise (i.e. finite resolution of pupillary changes due to discrete pixels in the sensor)

![Example of features](https://github.com/petteriTeikari/PLR_video/blob/master/images/discrete_pixels.png "Example of features")

_http://www.sci.utah.edu/~arpaiva/classes/UT_ece6962/image_representation_and_discretization.pdf_

## How to start using

`git clone --recurse-submodules https://github.com/petteriTeikari/PLR_video`

### How-to-update the repo with the submodules

`git submodule update --recursive --remote`
