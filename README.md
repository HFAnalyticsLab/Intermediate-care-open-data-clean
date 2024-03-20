![plot](https://github.com/HFAnalyticsLab/Intermediate-care-open-data-clean/blob/main/ndlbanner.png)

# Analysis of open data on intermediate care

## Project Description

For our first Networked Data Lab output on intermediate care [The challenges and potential of intermediate care](https://www.health.org.uk/publications/long-reads/the-challenges-and-potential-of-intermediate-care), we analysed openly available data on discharges from acute and community hospitals, local authority-provided reablement, and urgent commmunity response services. All datasets detailed below cover England only. 

## Data sources
* [Adult Social Care Activity and Finance Report, NHS Digital](https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report)
* [Measures from the Adult Social Care Outcomes Framework](https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-outcomes-framework-ascof)
* [Discharge Delays (Acute) SitRep dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/)
* [Discharge Delays (Community) SitRep dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/)
* [2-hour Urgent Community Response dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/2-hour-urgent-community-response/)

## How does it work?

This repository includes three separate folders: Scripts, Outputs, and Technical Appendix. 

**Scripts**

The first 3 scripts (00, 00b and 00c), load all necessary packages, create a local folder structure, and define functions for downloading and wrangling the required data from their online sources. These do not need to be run independently - when any of the analytical scripts (01-05) are run, these scripts and functions are loaded and run as necessary. 

* **`00. Setup_and_packages.R`** This script installs any required packages which are not currently installed in your local directory, and loads the libraries of all required packages. It then creates a folder structure on your local directory which will host the raw data downloaded by the scraping and downloading functions defined in script 00b.   
* **`00b. Scraping_and_download_functions.R`** This script defines functions which scrape and download the required data for the below analysis scripts into the Raw_data folder created by the Setup script above. This script defines functions but does not run them - they are run as necessary in the analytical scripts below (01-05).
* **`00c. Wrangling_functions.R`** Much of the data used in this project is sourced from Excel sheets which are not in amenable formats for analysis. This script defines functions which automate the process of feeding in each edition of the datasets after they're downloaded by the functions in script 00b, and transforms them into formats more easily analyzed. As above, this script defines but does not run its functions. 
* **`01. Acute_Sitrep_analysis.R`** This script uses the functions defined in scripts 00b and 00c to scrape and download monthly Delayed Discharge (Acute) SitRep datasets from their online source and wrangles them into amenable formats for analysis with R. Analysis as detailed in the publication and technical appendix is then performed on this data.  
* **`02. Community_Sitrep_analysis.R`** This script uses the functions defined in scripts 00b and 00c to scrape and download monthly Delayed Discharge (Community) SitRep datasets from their online source and wrangles them into amenable formats for analysis with R. Analysis as detailed in the publication and technical appendixis then performed on this data.  
* **`03. ASC_data_analysis.R`** This script uses the functions defined in scripts 00b and 00c to scrape and download the annual Adult Social Care Activty and Finance Report and Measures from the Adult Social Care Outcomes Framework publications from their online sources and wrangles them into amenable formats for analysis with R. Analysis as detailed in the publication and technical appendix is then performed on this data.  
* **`04. Expenditure_analysis.R`** This script uses the functions defined in scripts 00b and 00c to scrape and download the expenditure tables from the annual Adult Social Care Activty and Finance Report publication and the latest GDP deflator (December 2023 as of publication) from their online sourcs and wrangles them into amenable formats for analysis with R. As detailed in the technical appendix, unit costs are derived using the total number of reablement episodes and total local authority expenditure on reablement, which are then transformed to real terms (2022-23 prices) using the GDP deflator.  
* **`05. UCR_analysis.R`** This script downloads the latest 2-hour Urgent Community Response dataset (December 2023 as of publication) and derives average monthly UCR referrals for 2023. 


**Outputs**

The outputs folder contains a series of csv files containing the data used to construct the 5 figures in the long read. These csv files are produced by the analysis scripts detailed above.  

**Technical appendix**

* **`Technical Appendix.md`** This file contains further details on the data sources and methods used in our analysis of open data on intermediate care in England. 


### Requirements

These scripts were written in R version 4.0.2.

The following packages are required for this project:
* tidyverse (2.0.0)
* readxl (1.4.3) 
* rvest (1.0.3)
* here (1.0.1)

These packages will be installed and loaded by the **`00. Setup_and_packages.R`** script if not present on your local machine. 

## Authors

* Caroline Fraser
* Tom Prendergast

## License

This project is licensed under the MIT License.

## Acknowledgments

Thank you to Hannah Knight for her advice and guidance throughout this analysis.
