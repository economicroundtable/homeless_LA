# homeless_LA
This repository contains individual-year and multi-year data sets prepared and documented by the Economic Roundtable, based on raw data supplied by the Los Angeles Homeless Services Authority (LAHSA). The repository also includes code for projects based on the data -- a method for estimating the size of the annualized homeless population and a shiny app to visualize the data. 

THANKS to Paul Beeman, intern at the Economic Roundtable, for creating the cleaned data files that made this work possible. Thanks also to the 40+ volunteers who participated in our data dive with DataKind, where we first shared this data. The weekend-long, volunter-driven data hackathon produced valuable data visualizations and strategies for annualized population estimation that have been incorporated in the projects in this repository. We're very glad that you chose to share your skills with us, and we hope that you find this repository useful.

---------

Outline of the repository (folders listed alphabetically)

1. Annualized Population - Report on a method to estimate the number of people who experience homelessness in LA in a year (annualized population estimate) based on point-in-time data, with 2017 as an example. Report compares our method to the method currently used by LAHSA. Folder contains the report's R markdown file, data processing script called from the markdown, and markdown output PDF. 

2. Final Data and Codebooks - Cleaned versions of individual-year data about sheltered people (from the Homeless Management Information System (HMIS)) and unsheltered people (based on demographic surveys). This data augments the results of the overnight count of homeless people with descriptive information about the population. The multi-year data set combines individual-year sheltered and unsheltered data, but only includes variables which are present in all or nearly all data sets (about 30). Codebooks are provided for each data set (files that end _CB in the relevant folders). See the KEY NOTES ABOUT THE FINAL DATA section below for more important information.

3. Methodology Reports - For each year of data there is a methodology report from the group that oversaw that year's Homeless Count (affiliated with UNC in 2011, 2013, 2015 and 2015, and USC in 2017). Contains information about data collection, sampling, analysis techniques, and population estimates. 

4. R Files for Data Cleaning - R files detailing the process of cleaning the Raw Data Files. They should only be necessary to consult if a variable's meaning isn't clear from the year's codebook. File paths may be out of date. 

5. Raw Data Files - A plethora of raw data files that were supplied to the Economic Roundtable by LAHSA, which we drew from to build our final data files. They date back to 2007, whereas our cleaned data starts at 2011. We recommend using our cleaned data files wherever possible, but the raw files do contain some additional variables or detail within variables that we condensed for interpretability. 

6. Shiny - The code behind our shiny app to visualize and compare data: (link TBA). Many thanks to Lisa Ann Yu, a volunteer at our data dive for creating the framework for this app, and our intern Paul Beeman for turning it into a polished final product.

7. User Projects - This is a folder where users of the data can share code to analyze, visualize, or otherwise use the data. Please share your work from the data dive and beyond.

Key notes about the final data:
-   Geography: Data pertains to the Los Angeles Continuum of Care (CoC) which includes all of Los Angeles County except the cities of Pasadena, Glendale and Long Beach.
-   Sampling and weights:

    --  Data about unsheltered people represents a SAMPLE of the point-in-time unsheltered population. Demographic street surveys are conducted each year in a sample of census tracts in the CoC around the time of the annual overnight count. Sampling methods are described in the corresponding methodology reports. 

    --  Data about sheltered people represents a (near) COMPLETE census of the sheltered population as recorded in HMIS in the month leading up to the month when the overnight count is taken. The wider time frame is used to increase the amount of data about the sheltered population. Due to the time frame it may include more entries than the number of people estimated to be in a shelter on a given night. On the other hand, the HMIS data excludes people in non-HMIS service projects and shelters for victims of domestic violence due to privacy concerns.

    --  Weights: given the considerations above, the individual-year data should be weighted when examining population demographics. Weights are available for 2016 and 2017 data, which include "Weights" and "Weights_rescale" variables. See the corresponding codebook section Weights for description.

-   In order to create the multi-year data set it was necessary to make the variable levels coarser in some cases. Thus, while the multi-year data set is a valuable resource for tracking year-to-year changes in the population, it does not always offer the finest level of detail available in the data.
-   In 2016 and 2017 homeless youth (age 18-24) were given a separate, shorter survey, so many variables available for the general population are NA for that age group.
-   Starting in 2016, people who spent the majority of their time indoors were no longer surveyed for the unsheltered datasets.
-   Codebooks are organized by highlighted topics, e.g. LIVING SITUATION. The topics are the same from year to year and among data sets. However, the survey instruments changed from year to year so the availability of certain variables and the interpretation of seemingly equivalent variables is not constant. The codebooks list the question each variable responds to, and the meaning of the recorded levels. 
 

If you have questions or concerns about the data you can use the Issues section of this repository to post them publicly. Thanks for reading!
