# Students who prefer face-to-face tests outperform their online peers in organic chemistry
 
Abby E. Beatty*, Abby Esco, Ash Curtiss, & Cissy J. Ballen  
*Corresponding author: aeb0084@auburn.edu

This repository holds all supplemental files for "Students who prefer face-to-face tests outperform their online peers in organic chemistry"

## Abstract: 
> "To test the hypothesis that students who complete remote online tests experience an ‘online grade penalty,’ we compared performance outcomes of second-year students who elected to complete exams online to those who completed face-to-face, paper-based tests in an organic chemistry course. We pursued the following research questions: (RQ1) Are there performance gaps between students who elect to take online tests and those who take face-to-face tests? (RQ2) Do these two groups differ with respect to other affective or incoming performance attributes? How do these attributes relate to performance overall? (RQ3) How does performance differ between students who reported equal in-class engagement but selected different testing modes? (RQ4) Why do students prefer one testing mode over the other? We found that students who elected to take online tests consistently underperformed relative to those who took face-to-face tests. While we observed no difference between the two student groups with respect to their intrinsic goal orientation and incoming academic preparation, students who preferred face-to-face tests perceived chemistry as more valuable than students who preferred to complete exams online. We observed a positive correlation between performance outcomes and all affective factors. Among students who reported similar levels of in-class engagement, online testers underperformed relative to face-to-face testers. Open-ended responses revealed online testers were avoiding exposure to illness/COVID-19 and preferred the convenience of staying at home; the most common responses from face-to-face testers included the ability to perform and focus better in the classroom, and increased comfort or decreased stress they perceived while taking exams."

### Quick Key to File Directory: Detailed Descriptions of file use can be found below.

Note: The final data set used in analysis is available for public use. Additionally, deidentified raw survey data is available here. Due to IRB Restrictions all data files used in analysis that contain institutional information (Grades, GPA, etc.) prior to the final merged, and deidientified data are available upon direct request. Following approval, all deidentified data including institutional info will be shared directly.


Analysis and File Names| Brief Description | Link to File
-------------------------------------|------------------------------------ | -----------------------------------------------------
CHEM-master-file_Deidentified.xlsb   |Excel File Containing All Raw Survey Data  | [Raw Survey Data](CHEM-master-file_Deidentified.xlsb)
CHEM_deidentified.csv                |CSV File Use in R Statistical Analysis     | [CSV used in Statistical Analysis](CHEM_deidentified.csv)


## Supplementary Tables: 

<img src="TableS1.JPG" width="1200">

<img src="TableS2.JPG" width="800">


## Statistical and Data Visualization Code
```ruby
#Load all necessary Packages
library(reshape2)
library(nlme)
library(ggplot2)
library(plyr)
library(emmeans)
library(devtools)
library(tidyverse)
library(corrplot)
library(GPArotation)
library(semPlot)
library(lavaan)
library(cowplot)
library(readr)
library(tidyr)
library(Hmisc)
library(RColorBrewer)
library(PupillometryR)
```

```ruby
#Load data files containing latent variables and institutional information. Merge the two data sets into a single data frame.
chem.cfa.raw=read.csv("CHEM_CFA.csv", header=T, na.strings = c("", "NA"))
chem.cfa.raw$GID<-toupper(chem.cfa.raw$GID) 

institut=read.csv("Institutional_info.csv")
#Institutional data was then used to merge with survey data. Note: ACTcalc is raw ACT values when available, and SAT values converted to ACT when no ACT was reported but SAT was.

chem.cfa <- merge(chem.cfa.raw,institut, by="GID", all=TRUE, incomparables = FALSE)
```
> Data is often scewed to the right. Only a couple of the data columns are normal. So data will need to be transformed. Go to this website to see alternitives to normalizing data within the cfa model: https://lavaan.ugent.be/tutorial/est.html
> "MLM": maximum likelihood estimation with robust standard errors and a Satorra-Bentler scaled test statistic. For complete data only.See this pub for justification of test statistic choice: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.382.6856&rep=rep1&type=pdf



