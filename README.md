## Code repository for *Ice thickness regulates heat flux in permanently ice-covered lakes in Antarctica*

### All data were collected by the McMurdo Dry Valleys Long Term Ecological Research program (https://mcm.lternet.edu/)

Takacs-vesbach, C.D., Priscu, J.C. 2025. Conductivity, temperature, and depth (CTD) vertical profiles collected from lakes in the McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 18. Environmental Data Initiative. DOI: 10.6073/pasta/004de32b27fc88954abdce0ff8a3bbb3. Dataset accessed 14 April 2025.

Takacs-Vesbach, C. and J. Priscu. 2025. Lake ice thickness and density measurements, McMurdo Dry Valleys, Antarctica (1989-2025, ongoing) ver 17. Environmental Data Initiative. https://doi.org/10.6073/pasta/e72dc49d774796767884c535e864c915 (Accessed 2025-04-14).

Doran, P.T. and M.N. Gooseff. 2025. Lake level surveys in the McMurdo Dry Valleys, Antarctica (1968-2025, ongoing) ver 14. Environmental Data Initiative. https://doi.org/10.6073/pasta/649e1e54f663e8077f6ca96352e703ba 

### How to reproduce figures and output statistics

All "00\_" .R scripts are helper scripts that download raw data from the Environmental Data Initiative (EDI)

`02_ctdJoin.R`

-   Retrieves ctd data using `00_getCTD.R`
    -   Joins CTD data with lake levels and hypsometry
-   Remove bad data
-   Clean data: For each individual profile
    -   Round to 0.1 m depth increment
    -   Interpolate any gaps larger than 5 m
    -   Remove any profile with less than 10 data points
-   Add salinity using UNESCO and Spigel and Priscu equations [SP96]
-   Add density using UNESCO equations, back calculating from SP96 for bottom of ELB
-   Add specific heat capacity using `SW_SpcHeat` function
    -  Rewritten in R from a MATLAB implementation {Source: https://web.mit.edu/seawater/}
-   Add density using `sw_dens` function
-   Add ice thickness by date
    -   Linearly interpolate ice thickness between sampling dates
-   Export plots
    -   'figures/Fig1_CTD.png' CTD temperature and conductivity profiles
    -   'figures/Fig2_CTDchange.png' Differences in CTD profiles
-   Find representative fall profile dates `source('src/functions/profileAvgDates.R'`
    -   'figures/SI_SamplingDays.png' Grid of sampling dates and representative profiles

`03_heatFlux.R`

-   Get hyposmetry, area and volume for every 0.1 m layer
-   Check days that have full depth profiles
-   For deepest sampling depth, assume volume of this layer equals cumulative volume beneath it
-   Add a cutoff depth to all profiles
    -   ELB 25 m asl
    -   WLB 25 m asl
    -   LF 2.5 m asl
    -   LH 48 m asl
-   Plot temperature heat maps 
-   Pull out representative fall profiles
    -   Calculate change in temperature between each layer
    -   Calculte heat flux 
-   Plot heat flux timeseries
-   Plot SI figure comparing 1D to area weighted mean temperature 
-   Export daily data 'dataout/MDVLakes_profileMeans.csv'
-   Export annual heatflux 'dataout/MDVLakes_annualHeatFlux.csv'
-   Export depth discrete data 'dataout/MDVLakes_depthDiscrete.csv'

`04_GAMs.R`

-   For each lake do the following:
-   Add annual difference in ice thickness and lake level using representative fall dates 
-   Using this dataframe, create a series of linear models

```
  ### Regression models for temperature 
  lm(temp ~ LL, data = annual.list[[i]])
  lm(temp ~ iceZ, data = annual.list[[i]])
  lm(temp ~ LL + iceZ, data = annual.list[[i]])
  
  ### Regression models for heat flux
  lm(flux ~ LL.diff, data = annual.list[[i]])
  lm(flux ~ iceZ, data = annual.list[[i]])
  lm(flux ~ ice.diff, data = annual.list[[i]])
  lm(flux ~ iceZ + ice.diff, data = annual.list[[i]])
  
  ### Regression models for ice
  output$reg.ice1[[i]] <- lm(ice.diff ~ iceZ, data = annual.list[[i]])
```

-   Output 6 panel figure 'figures/Fig5_GAMS.png'
    -   Mean temp (GAM fit line)
    -   Ice thickness (GAM fit line)
    -   Lake level (GAM fit line)
    -   Annual heat flux
    -   Annual ice difference
    -   Annual LL difference
-   Assess model fits
    -   Extract AIC, BIC, r2
    -   Get coefficients and significance
    -   Variable Inflation Factors to assess colinearity
    -   Output latex tables
-   Plot scatter plot of heat flux vs 1) Ice Thickness and 2) ∆Ice

`05_Synchrony.R`

-   Synchony function to test correlation, statistical significance of Kendall’s W, concurrency,
and phase synchrony 
-   Applied to mean water temperature, ice thickness, and heat flux

`06_PaperStats.R`

-   Test autocorrelation between annual differences using `acf` function
-   Test Variable-lag Granger Causality
-   Other ranges for paper 
