# Hive
 Code for article : "Determinants of honeybee hive survival and its implications for urban biodiversity in Toronto and Montreal: A Canadian case study"

Available here : https://www.sciencedirect.com/science/article/abs/pii/S0169204624000653

The structure of the code has been designed to be flexible and adapt to your own unique lists of variables.

Please note that our publicly available data has been anonymized, but is provided to help test the code.
This means all spatial information has been removed from the dataframe, therefore the MoransI test for spatial autocorrelation will not function properly.
Be sure to have geographic coordinates in your datasets when performing the the MoransI test.
Also, be sure to adjust the geographic projection contained in the 4-GLM model script at:
 - Line 333 
 - Line 334  
