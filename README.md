# Fatal Police Shootings Dashboard

### Name: Zhi Hern Tom

### Student ID: 1068268

This assignment aims to deploy a R dashboard app analysing police fatal shooting records in the United States since 1st January 2015.

[Link to dashboard (shinyapps.io)](https://ltn3hf-zhi0hern0tom.shinyapps.io/Police-Fatal-Shootings/)

## Design Summary
This dashboard provides an overview and analysis of police fatal shootings in the United States of America. This dashboard contains 6 tabs including Dashboard, Interactive Map, Data Explorer, Select Parameters, Setting and FAQs. 

### Dashboard
This dashboard contains an overview of police fatal shooting records from 2015 to 2021. You can find the shooting records across all states in the United States of America. If the cursor is placed at particular state, the record information will pop up. Also, there are several graphs including line charts and bar plots showing the distribution of several features such as age, gender, race, threat level and arm type. Highchart library is used to plotted charts and graphs since it has better visual appearance.

### Interactive Map
This tab shows how the data distributed across the USA. The floating explorer enables you to colour the data points according to selected category (Race, Manner of Death, Arm Type, Gender, and Threat Level). The filter will also be applied to the barplot inside the floating explorer. When a data point is clicked, the information of that data point will pop up at that location. Map furniture discussed during the lectures such as title, legend, and representative fraction have been used in this section.

### Data Explorer
This tab provides the data table. It enables you to copy or download the dataset as CSV or Excel file. You are able filter the data by using the “State”, “City” and “Search” boxes. By clicking the “target” icon at the last column (Action), you will be redirected to the location in the Interactive Map and the information will be pop up.

### Select Parameters
You can select specific year from 2015 to 2021 and the filter will be applied to all charts, maps, as well as the data table. Please note this filter will not be applied to charts under the section “Historical Police Fatal Shootings Overview”.

### Setting
Display mode is available. You can either choose “Dark Mode” or “Light Mode” for better readability. This display mode is applied to all charts, graphs and maps.

### FAQs
This tab provides some frequently asked questions.

## Appendix
The data was obtained from https://github.com/washingtonpost/data-police-shootings and The data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/). The dataset was processed by several steps including:
* Dropping instances with NaN values.
* Re-categorise several columns for better performance.
* Omitting records in 2022 since the data is not complete.
* Aggregating numeric columns such as age.

