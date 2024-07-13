### Determinants of honeybee hive survival and its implications for urban biodiversity in Toronto and Montreal: A Canadian case study

# Description
This project contains the code used in the following article: 
[Determinants of honeybee hive survival and its implications for urban biodiversity in Toronto and Montreal: A Canadian case study](https://www.sciencedirect.com/science/article/abs/pii/S0169204624000653)

## Instructions

Open the Rproj file, then access the individual scripts from the file menu <br/>

## Script descriptions

**1-GLM_model** :  Binominal logit model used to calculate the impact of environmental variables on urban hive health <br/>
**2-Descriptive_Stats** :  Descriptive statistics for the data <br/>

Note: Data preparation script is not included due to the fact that the data is proprietary. <br/>

## Folder structure 

**Outputs** : Contians all the tables and figures created with the scripts <br/>
**Scripts** : Contains the individual R scripts <br/>
**DataTreated** : Contains the processed data that was created using raw data from the following sources : <br/>
- Alveole.buzz proprietary data <br/>
- Ontario Ministry of Natural Resources and Forestry <br/>
- Communaute metropolitaine de Montreal <br/>
- Statistics Canada (Census of population 2016) <br/>
- Landsat 8 <br/>
- Canadian Urban Environmental Health Research Consortium (CANUE) <br/>

## Packages : 
"tidyverse" <br/>
"openxlsx" <br/>
"car" <br/>
"sandwich" <br/>
"pscl" <br/>
"mfx" <br/>
"DHARMa" <br/>
"spdep" <br/>
"sp" <br/>
"pgirmess" <br/>
"spfilteR" <br/>
"betareg" <br/>
"rgdal" <br/>

Note : All package installation is handled within the scripts.

# Important :

The structure of the code has been designed to be flexible and adapt to your own unique lists of variables. <br/>

Please note that our Hive data has been anonymized, but is provided to help validate our results. <br/>
This means all spatial information has been removed from the dataframe, therefore the MoransI test for spatial autocorrelation will not function properly. <br/>
Be sure to have geographic coordinates in your datasets when performing the the MoransI test. <br/>
Also, be sure to adjust the geographic projection contained in the #3-GLM model script at: <br/>
 - Line 333  <br/>
 - Line 334  <br/>  

# Author

Gavin MacGregor

