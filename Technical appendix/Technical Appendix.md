# Technical appendix

## Data sources
We used the [Discharge Delays (Acute) SitRep dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/) from NHS England (referred to as ‘acute discharge data’ in the analysis) to provide information on the pathways into intermediate care and the discharge delays from acute hospitals between January and December 2023. [Concerns have been expressed around the quality of SitRep data](https://www.hsj.co.uk/quality-and-performance/former-nhse-chief-most-hospital-discharge-data-is-useless/7033794.article), as it is not validated. In addition, some providers report patients discharged to domiciliary care as receiving reablement. Therefore, these numbers may be overestimates.  

We used the [Adult Social Care Activity and Finance Report](https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report) and the [Measures from the Adult Social Care Outcomes Framework](https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-outcomes-framework-ascof) (referred to as ‘adult social care data’ in the analysis) from NHS England for data on local authority funded intermediate care. Data are submitted by 152 [councils with adult social services responsibilities](https://www.nuffieldtrust.org.uk/news-item/who-organises-and-funds-social-care-1) (referred to as ‘local authorities’ in the analysis), the local bodies that commission social care in England. We refer to ‘short term care to maximise independence’ from these datasets as local authority funded intermediate care. Local authorities were excluded if they had not submitted data for a given field. For national measures, NHS England use the equivalent value from the most recent year available for each excluded local authority.

We used the [urgent community response](https://www.england.nhs.uk/statistics/statistical-work-areas/2-hour-urgent-community-response/) dataset from NHS England for information on crisis response services between April and December 2023. New technical guidance was applied to the April 2023 data; therefore, older data are not comparable.

We used the [Discharge Delays (Community) SitRep data](https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/) from NHS England (referred to as ‘community discharge data’ in the analysis) for information on the pathways out of bed based intermediate care and the discharge delays in community hospitals between June and December 2023. The community SitRep data cover adult inpatients in NHS commissioned bedded services within communities for the purposes of physical health recovery and rehabilitation. They include bed based intermediate care as well as other types of physical recovery and rehabilitation. 

We used results from the Intermediate Care Benchmarking Report 2022/23 produced by the [NHS Benchmarking Network](https://www.nhsbenchmarking.nhs.uk/) (referred to as the Intermediate Care Benchmarking Report 2022/23) for information on intermediate care not captured in the open data. The NHS Benchmarking intermediate care project collected a range of information on intermediate care from 46 organisations in England and Wales in 2023. The data are not nationally representative, and have a particularly small sample of local authorities and therefore reablement services. The NHS Benchmarking Network provided us with only a few select results, and the full report is not in the public domain.   

## Methods

#### Pathways into intermediate care
We used the acute discharge data to describe the average number of people discharged from acute hospitals each month and the percentage of people discharged to each pathway. 

We calculated the number of people starting local authority funded intermediate care each month by dividing the figure for 2022/23 by 12 (using adult social care data).

To estimate the scale of intermediate care, we used the discharge data as a starting point for the number of patients discharged to 1) step down reablement or home based intermediate care and 2) bed based intermediate care. We used the percentage of each type of step up intermediate care from the Intermediate Care Benchmarking Report 2022/23 to estimate the number of patients starting intermediate care in total and step up care specifically. We assumed the total discharges to step down reablement or home based intermediate care were split equally between the two types.  

For crisis response intermediate care, we used the number of referrals into urgent community response services. We assumed there was only one referral per patient each month.  

The numbers we used to estimate the scale of intermediate care are shown in Table 1.

**Table 1: Our estimation of the scale of intermediate care using the percentage of each type of intermediate care that is step up from the NHS benchmarking report (orange), acute discharge data (blue) and Urgent Community Response statistics (grey).**

![plot](https://github.com/HFAnalyticsLab/Intermediate-care-open-data-clean/blob/main/Technical%20appendix/Technical%20appendix%20-%20table%201_1.png)

*This was calculated from a combined total of 27,000 for home based or reablement

#### Pathways out of intermediate care
We used the community discharge data to calculate the average number of discharges from bed based intermediate care and the percentage of people discharged to each pathway.

Adult social care data tell us what happens at the end of local authority funded intermediate care packages in England, as well as provide an indication of the proportion of people aged 65 and over still at home 91 days after discharge into reablement care from hospital. We do not know if this is similar for patients receiving NHS-funded or jointly commissioned intermediate care.

#### Discharge delays
We also used the acute and community discharge datasets to describe the number of delayed discharges and the percentage due to each type of intermediate care per month. We assumed all delays due to pathway 1 and pathway 2 were due to intermediate care. We report discharge delays for patients with lengths of stay of 14 or more days. The weekly delay numbers are monthly medians of weekly data.  

We calculated the number of people expected to start intermediate care by adding the delayed discharges and the discharges for pathway 1 and 2. We then divided the number of delays by the total number of people ready for intermediate care to get the percentage of people delayed for each intermediate care type. 

#### Increasing costs
We drew expenditure data on local authority funded intermediate care from the Adult Social Care Activity and Finance Report. We constructed real terms expenditure figures using the [GDP deflator](https://www.gov.uk/government/collections/gdp-deflators-at-market-prices-and-money-gdp). We derived average costs of intermediate care episodes by dividing total expenditure by the number of episodes (for both new and existing clients). An ‘episode’ refers to a single package of intermediate care.

While both figures are available in the Adult Social Care Activity and Finance Report, data on local authority short-term care expenditure originate from the [Adult Social Care Finance Return](https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/social-care-collection-materials-2023/adult-social-care-finance-return-asc-fr) data collection, while data on number of episodes of intermediate care are sourced from the [Short and Long Term](https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/social-care-collection-materials-2023/salt-data-return-2022-2023-guidance) returns. While these returns are designed to be complement each other, it must be noted that they are separately collected. As such, unit cost calculations made using these data must be taken as approximate.

