This repository holds the data-processing and analysis code for the research project "Assessing capacity to social distance and neighborhood-level health disparities during the COVID-19 pandemic".

Instructions
============================================================
 
All data collection, cleaning, and analysis are conducted via code/NYC_COVID_Disparities.R 

Example output
============================================================

Example output from code/NYC_COVID_Disparities.R is `located here <https://justlab.github.io/COVID_19_admin_disparities/code/NYC_COVID_Disparities.html>`_.  

| Processing and analysis time on a personal computer was **INSERT TIME HERE**.  
| On a server (2x Intel Xeon E5-2640 v3) connected to an enterprise 1 Gbit/s network, total processing time was 52 minutes, 19 minutes of which was predominantly downloading data.  

Packages and Installation
============================================================
This analysis was conducted in R version 4.0.2. No specialized hardware is required. 

When you open the project in RStudio or start an R session in the project directory, the :code:`renv` package will be installed if it is not already available. You can then install the same versions of R packages as was used for the analysis by running :code:`renv::restore()`.  

License
============================================================

This program is copyright 2020 Daniel Carrión, Elena Colicino, Nicolo Foppa Pedretti, Kodi B. Arfer, Johnathan Rush, and Allan C. Just.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License`_ for more details.

.. _`GNU General Public License`: http://www.gnu.org/licenses/

