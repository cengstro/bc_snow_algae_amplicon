# bc_snow_algae_amplicon
rbcL and 18S amplicon surveys of BC snow algae communities.

# Directory overview

* archive/ 
* cedar/ contains initial bioinformatic processing steps done on a HPC cluster, including CUTADAPT, DADA2 scripts, and reference databases used for taxonomy assignment. This is a clone of my compute canada Cedar project directory.
* code/ contains all scripts used for data wrangling, and exploratory data analysis. Does not include scripts used to generate final figures for presentations and manuscript
* data/ contains all HPC output including relative abundance tables, taxonomy assignment,
* manuscript/ contains manuscript, all code used to generate figs and supplementary materials, and original copies of all photographs used in figures
* pres/ includes all materials used in presentations of this project, including all photos/ used in presentations, and a file for each presentation containing the scripts and figures specific to that event. The actual presentations are located on google drive


what is the distinction between data and output? I think this contains a subcategory of data, it has just been processed some additional steps.

todo:
* code contains exploratory/ wrangle/ (wrangel contains the scripts used to generate the final product in each of the data subdirs), exploratory/ contains Rmds and htmls. sprinkle archive/ or etc/ bins throughout
* data subdirs: taxonomy/ rel_abund/ cellct/ field/ each contains the final usable product in the main directory, and all other intermediates are in subdirectories (dada2_out/, tidied/ etc)

* sprinkle helpful READMEs throughout, eg when running web apps, etc
* best way to get georeferenced maps/images into r?