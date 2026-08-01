# Air Quality Data Cleaning in R

## Overview

This project demonstrates data cleaning and missing-value handling techniques in **R** using the **PRSA Air Quality Dataset (Aotizhongxin Station)**. The assignment covers importing the dataset, identifying different types of missing values, cleaning numerical and categorical variables, implementing error handling, generating a visualization, and exporting the cleaned dataset.

## Dataset

* **Dataset:** PRSA_Data_Aotizhongxin_20130301_20170228.csv
* **Observations:** 35,064
* **Variables:** 18

Selected variables used for data cleaning:

* PM2.5
* PM10
* SO2
* NO2
* TEMP
* WSPM
* wd (Wind Direction)

## Tasks Performed

1. Imported the CSV dataset into R.
2. Displayed dataset structure, dimensions, and missing values.
3. Demonstrated the difference between **NA**, **NULL**, and **NaN**.
4. Created a custom **missing_summary()** function.
5. Generated **pollution_ratio** and handled invalid numerical values.
6. Replaced missing numerical values using **median imputation**.
7. Replaced missing categorical values using **mode imputation**.
8. Implemented **tryCatch()** for robust error handling.
9. Compared missing values before and after cleaning.
10. Created a bar chart visualization of missing values.
11. Exported the cleaned dataset as a CSV file.

## Files Included

* `Air_Quality_Cleaning_Assignment.R` — Complete R script containing all tasks.
* `cleaned_air_quality_data.csv` — Cleaned dataset.
* `missing_values_comparison.png` — Bar chart comparing missing values before and after cleaning.
* `README.md` — Project documentation.


## Visualization

The project includes a grouped bar chart comparing the number of missing values **before** and **after** cleaning for all selected variables.

## Interpretation

The air quality dataset was successfully cleaned by handling missing values in both numerical and categorical variables. Numerical variables were imputed using the **median**, while the categorical variable **wd** was imputed using the **mode**. Invalid numerical results such as **NaN** and **infinite values** were converted to **NA** before treatment. After the cleaning process, all selected variables contained **zero missing values**, making the dataset suitable for further statistical analysis, visualization, and machine learning applications.

