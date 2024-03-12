# Analysis of open data on intermediate care

#### Project Status: In progress

## Project Description

For our first Networked Data Lab output on intermediate care, we analysed data from ____.

## Data sources
* [Adult Social Care Activity and Finance Report, NHS Digital](https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report)
* [Measures from the Adult Social Care Outcomes Framework](https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-outcomes-framework-ascof)
* [Discharge Delays (Acute) SitRep dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/)
* [Discharge Delays (Community) SitRep dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/)
* [2-hour Urgent Community Response dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/2-hour-urgent-community-response/)

## How does it work?

This repository includes three separate folders: Scripts, Outputs, and Technical Appendix. The first 3 scripts (00, 00b and 00c), load all necessary packages, create a local folder structure, and define functions for downloading and wrangling the required data from their online sources. These do not need to be run independently - when any of the analytical scripts (01-05) are run, these scripts and functions are loaded and run as necessary. 

**Scripts**

* **`00. Setup_and_packages.R`** This script installs any required packages which are not currently installed in your local directory, and loads the libraries of all required packages. It then creates a folder structure on your local directory  
* **`00b. Scraping_and_download_functions.R`** This script defines functions which scrape and download the required data for the below analysis scripts into the Raw_data folder created by the Setup script above.   
* **`00c. Wrangling_functions.R`** Much of the data used in this project is sourced from Excel sheets which are not in amenable formats for analysis. This script defines functions which automate the process of feeding in each edition of the datasets after they're downloaded by the functions in script 00b, and transforms them into formats more easily analyzed. 
* **`01. Acute_Sitrep_analysis.R`** 
* **`02. Community_Sitrep_analysis.R`**
* **`03. ASC_data_analysis.R`**
* **`04. Expenditure_analysis.R`**
* **`05. UCR_analysis.R`**


**Outputs**


**Technical appendix**

* **`1. Technical appendix.Rmd`**


### Requirements

These scripts were written in R version 4.0.2.

The following packages are required for this project:

## Authors

* Caroline Fraser
* Tom Prendergast

## License

This project is licensed under the MIT License.

## Acknowledgments

Thank you to Hannah Knight for her advice and guidance throughout this analysis.
