### Cloud_Labeler

Find Labels(index_sets) in a 3D boolean field presuming connectivity of a 7pt stencil and cyclic boundary conditions

The task is to find and label connected patches in a 3D array with boolean values.
It may be used to label cloud patches from e.g. LES output and use the index sets to compute statistics for each patch.

This repo contains fortran code for the labeler as well as a python interface, created with f2py.

#### Installation:
```
git clone https://github.com/jakubfabian/cloud_labeler.git

mkdir cloud_labeler/build
cd cloud_labeler/build

cmake .. -DENABLE_PYTHON=YES && make
```
