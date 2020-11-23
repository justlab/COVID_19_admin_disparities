This repository holds the data-processing and analysis code for the research project "Assessing capacity to social distance and neighborhood-level health disparities during the COVID-19 pandemic".

Instructions
============================================================
 
All data collection, cleaning, and analysis are conducted via code/NYC_COVID_Disparities.R 

Packages and Installation
============================================================
This analysis was conducted in R version 3.6.2. No specialized hardware is required. 

The following packages should be loaded with tidyverse 1.3.0 via CRAN: dplyr 0.8.4; forcats 0.4.0; ggplot2 3.3.1;	purrr 0.3.3; readr 1.3.1;	stringr 1.4.0; tibble 3.0.1; tidyr 1.0.2

The following packages, and their dependencies, are required and available via CRAN: sf 0.9-3; lubridate 1.7.8;	tidycensus 0.9.6; ggExtra 2.3;	ggridges 0.5.2;	rstan 2.19.3;	drc 3.0-1; spdep 1.1-3;	mgcv 1.8-31; broom 0.5.4;	MASS 7.3-51.5; spatialreg 1.1-5; here 0.1;
pdftools 2.2;	matrixStats 0.55.0;	egg 0.4.5; ggpubr 0.2.5; scales 1.1.1

The following packages are available via remotes::install_github(“justlab/Just_universal”, ref = “78812f519da11502706a5061e7b8bc4812e5c3b5”) and remotes::install_github(“justlab/MTA_turnstile”, ref = “6c8bd7690dfa6036bf991cb4504f42631e8f6756”): Just.universal; MTA.turnstile 

Example output
============================================================

Example output from code/NYC_COVID_Disparities.R is located: **INSERT LINK HERE**.

| Processing and analysis time on a personal computer was **INSERT TIME HERE**. 
| On a server (2x Intel Xeon E5-2640 v3) connected to an enterprise 1 Gbit/s network, total processing time was 52 minutes, 19 minutes of which was predominantly downloading data.  

License
============================================================

This program is copyright 2020 Daniel Carrión, Elena Colicino, Nicolo Foppa Pedretti, Kodi B. Arfer, Johnathan Rush, and Allan C. Just.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License`_ for more details.

.. _`GNU General Public License`: http://www.gnu.org/licenses/

