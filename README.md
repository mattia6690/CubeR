[![Build Status](https://travis-ci.org/mattia6690/CubeR.svg?branch=master)](https://travis-ci.org/mattia6690/Cube_R) 
[![CRAN](http://www.r-pkg.org/badges/version/CubeR)](https://cran.r-project.org/package=CubeR)

# CubeR
## An R-package to access data cubes via WCS and WCPS queries.

This R-Package provides several functions for interacting with databases WCS/WCPS based on OGC standards. We use the [Rasdaman implementation](http://saocompute.eurac.edu/rasdaman/ows) in order to host Copernicus Sentinel Data in multidimensionla arrays (Data Cubes) as used for the [Sentinel Alpine Observatory](http://sao.eurac.edu/)
The package can be directly imported in R by typing:

```r
library(devtools)
devtools::install_github("mattia6690/CubeR")
```

This package offers several possibilities to interact with Data Cubes as listed below. If you need more information feel free to contact us or consult the two HTML Files located in the [Documentary](https://gitlab.inf.unibz.it/earth_observation_public/CubeR/tree/master/Documentation) directory

#### 1. Discovery

It is possible to discover the whole Rasdaman environment (Capabilities) as well as the Coverages (Data Cubes) by calling the `getCapailities` functionality followed by the respecive URLs. The function automatically calls and parses the XML response from the server and collects the data necessary to describe the datasets desired.

#### 2. Describe

All the data returned by the server is accessible with the functions beginning with **coverage_get_**. These are explicitly for retrieving metadata corresponding to each of the coverage.

#### 3. Request/Return

Each function has an automatic query handler translating the input in WCPS queries and hands them over to the Rasdaman Server. Additionally, all of these functions are functional for subsetting the Data Cube in two to three dimensions (x-y-z).
The Functions **image_from_coverage** returns either an image or a subset of an image. The **pixel_history** function returns the time series for one single Pixel and the **geocoded_pixel_buffer** function for a specific area surrounding the Pixel. 

#### 4 Processing

The WCPS expansion allows the Data Cube to become not only a data provider but also a powerful tool for direct computation on the fly. The queries can be expanded by mathematical operator to perform operation between multiple spatial subsets. We implemented a functionality to calculate the NDVI (Normalized Difference Vegetation Index) directly on the Cube.
The **norm_diff_raster** function is calculating on a raster or a subset of a raster at one given time and returns a TIFF raster. The **norm_diff_pixel** returns the calculated NDVI from one or multiple pixel over time as a Data Frame

### Contributors

[Mattia Rossi](https://github.com/mattia6690)  <br>
[Daniel Frisinghelli](https://gitlab.inf.unibz.it/Daniel.Frisinghelli)  <br><br>

![](http://www.eurac.edu/Style%20Library/logoEURAC.jpg)

