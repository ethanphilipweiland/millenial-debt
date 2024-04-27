# "Generation Debt" Turns 40: Modeling Millennial Debt Trajectories in a Multilevel Framework
This analysis uses the National Longitudinal Survey of Youth - 1997 (NLSY97) and multilevel modeling techniques to analyze the dynamics of debt for Millennial adults as they age through their twenties and thirties. There are five key takeaways. First, most of the variation in debt comes from within individuals rather than between individuals. Second, on average, debt increases over time but this increase diminishes. Third, individuals experience varied trajectories of debt growth. Fourth, there is no association between debt levels at age 20 and the rate of debt growth and there is a negative association between debt levels at age 25 and the rate of debt growth. Finally, parental net worth does not meaningfully impact debt.

This research builds upon my master's thesis in sociology and was independently completed for EDUC-Y639: Multilevel Modeling taught by Professor Huang at Indiana University Bloomington in Spring 2024.

## Analytical Report
The file "Report.pdf" contains a write-up of the motivation, data, methodology, analysis, results, and limitations of this study. 

## Necessary Software
You will need the following software and packages installed to run the code files and reproduce the analysis.

Necessary software: `R`, `Quarto`

Necessary `R` packages: `tidyverse`, `caret`, `survey`, `stargazer`, `lme4`, `lmerTest`

## File Descriptions
    1. /Data Cleaning/ : Folder containing all code for data cleaning, uncleaned data, and custom longitudinal weights file downloaded from NLS Investigator
    2. /Data Cleaning/NLS Investigator/ : Files downloaded from NLS Investigator (online tool used to download National Longitudinal Survey data)
    3. Presentation.pdf : Slide deck detailing the motivation, data, methodology, analysis, results, and limitations of the study
    4. Presentation.qmd : Dynamic document used to produce Presentation.pdf
    5. Report.pdf : Write-up of the motivation, data, methodology, analysis, results, and limitations of the study
    6. Report.qmd : Dynamic document used to produce Report.pdf
    7. total_debt.RData : Cleaned NLSY97 data used for the analysis

## Installation and File Execution
To begin, download the /Data Cleaning/ folder. Open `R` and set /Data Cleaning/NLS Investigator/ as your working directory using `setwd()`. `R` script files are executable once a working directory to the folder containing the data is set. Running "R Source Code.R" will produce nlsy97.RData. Move nlsy97.RData into /Data Cleaning/ and set your working directory to /Data Cleaning/. Running Cleaning.R will produce the total_debt.RData, which is the cleaned data file. Download Presentation.qmd and Report.qmd and place these files in /Data Cleaning/. Running Presentation.qmd or Report.qmd will generate a slide deck or write-up (respectively) with a detailed walk-through of this project.

## Acknowledgements
Snijders, Tom A. B., and Roel J. Bosker. [1999] 2012. Multilevel Analysis: An Introduction to Basic
and Advanced Modeling. 2nd ed. Thousand Oaks, California: SAGE.
