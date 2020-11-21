# Sustainability-and-Gentrification-Starbucks-in-EPA-Brownfields-
Datasets and R file used to extract causal effect of EPA Area Wide Planning Grants on the number of Starbucks in the project area, where Starbucks are an indicator of gentrification. 
The excel files in this repository are:
  (1) "Control (BF Zipcodes by Applicant Year) -- Includes all Area Wide Planning grant applicants accross the 4 funding years and the zipcodes of the proposed regions
  (2) "Treatment (BF Zipcodes by Award Year)  -- Includes all Area Wide Planning grant recipients accross the 4 funding years and the zipcodes of the projects that took place
  (3) "Control and Treatment Zipcodes by Applicant Year" -- csv loaded into the R file as the basis of the event study, where treatment zipcodes are assigned a value of 1 and control are assigned a value of 0 
  (4) "Starbucks Locations by Opening Date" -- Includes all Starbucks Locations in the US to date by Opening Year
The R File documents code that merges these four data sets by ZIP Code and Year to create the Panel Data Set used for the regression analysis
