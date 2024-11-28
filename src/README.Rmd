# Code repository for `Heat storage and thermal dynamics in permanently ice-covered lakes in Antarctica`

## How to reproduce figures and output statistics

All "00\_" .R scripts are helper scripts that download raw data from EDI

`01_condSalTable.R`

-   Calls `00_getIons.R` script:
    -   Downloads discrete water sample data for ion concentrations, DIC, pH
    -   Downloads CTD data
    -   Joins discrete data with profiles within 1 week of each other
    -   Uses temp, DIC, pH to estimate carbonate/bicarbonate concentrations
    -   Calculates salinity as total mass of major ions
-   Creates specific conductance:salinity transfer function (gam function)
-   Outputs 'dataout/condSalTransfer.csv'
-   Outputs 'SI_SpC_Salinity.png'

`01_salinityTransferTable.R`

-   Creates a salinity transfer table of depth to salinity
-   To be used for ELB at depth when conductance relationship breaks down
-   Outputs 'dataout/salinityTransferTable.csv'

`02_ctdJoin.R`

-   Retrieves ctd data using `00_getCTD.R`
    -   Joins CTD data with lake levels and hypsometry
-   Remove bad data
-   Clean data: For each individual profile
    -   Round to 0.1 m depth increment
    -   Interpolate any gaps larger than 5 m
    -   Remove any profile with less than 10 data points
-   Add salinity
-   Calculate specific conductance relative to 5°C, and round to nearest 0.1 mS/cm (0.01 mS/cm for Lake Hoare)
-   Join with conductance:salinity transfer table 'dataout/condSalTransfer.csv'
-   For ELB, with salinities above 180, join with `salinityTransferTable.csv`
    -   Use 1995 data for 1993-1994
    -   Use 2019 data from 2020-2023
    -   Where transfer table doesn't go deep enough, interpolate with down last value
-   Add specific heat capacity using `SW_SpcHeat` function
-   Add density using `sw_dens` function
-   Add freezing point by using lookup table 'datain/papers/Bodnar_1993_FreezingPoint_Lookup.csv'
    -   Create 3rd degree polynomial from lookup table
    -   Apply polynomial to salinity data
    -   Anything above 21.2 %wt salinity (eutectic point of pure NaCl) set FPD to 23.2
-   Add ice thickness by date
    -   Linearly interpolate ice thickness between sampling dates
-   Export plots
    -   'figures/Fig1_CTD.png' CTD temperature and conductivity profiles
    -   'figures/Fig2_CTDchange.png' Differences in CTD profiles
    -   'figures/SI_SamplingDays.png' Grid of sampling dates

`03_heatStorage.R`

-   Get hyposmetry, area and volume for every 0.1 m layer
-   Check days that have full depth profiles
-   Calculate specific heat in J/m3/K (equivalent to °C)
-   Calculate total joules heat storage by spHeat_J_m3K \* volume layer \* temperature above freezing point
    -   Also calculate J/m2 and J/m3
-   Calculate latent heat of ice
    -   ice density of 900 kg/m3 \* 334000 J/kg
    -   calculate total joules by multiplying by volume layer
-   Plot heat storage heat maps for the four lakes
-   Plot temperature heat maps for the four lakes
-   Add a cutoff depth to all profiles
    -   ELB 25 m asl
    -   WLB 25 m asl
    -   LF 2.5 m asl
    -   LH 58 m asl [shallow, but many short profiles and bottom of LH is very close to 0°C]
-   Calculate daily total heat for lake and ice
-   Limit to between Sep 1 and Dec 15th for all lakes
-   Plot heat map and timeseries
    -   'figures/Fig3_HeatContent.png'
-   Export daily data 'dataout/MDVLakes_dailyHeatStorage.csv'

`04_GAMs.R`

-   For each lake do the following (limit data to between Sep 1 and Dec 15th)
-   Create gam model of temperature
-   Create gam model of ice thickness (use ice observations not interpolations)
-   Create gam model of lake level (use lake level observations not interpolations)
-   Create monthly dataframe from Oct 1995 to Jan 2024
-   Predict temperature, ice thickness, lake level from GAM at monthly interval
-   Create 95% confidence interval for each model
-   Exclude 2020 (no data collection)
-   For each year, calculate mean temp, ice and lake level data (observational data, not GAM data) between Sep 1 and Dec 15th
    -   Calculate difference between years
    -   Using this dataframe, create a series of linear models
```         
lm(temp.diff ~ iceZ + ice.diff + LL.diff, data = fit.interp)
lm(temp.diff ~ LL.diff, data = fit.interp)
lm(temp.diff ~ ice.diff, data = fit.interp)
lm(temp.diff ~ iceZ, data = fit.interp)
lm(temp ~ LL, data = fit.interp)
lm(temp ~ iceZ, data = fit.interp)
lm(temp ~ LL + iceZ, data = fit.interp)
lm(temp ~ LL + iceZ + ice.diff + LL, data = fit.interp)
```
-   Output 6 panel figure 'figures/Fig4_GAMS.png'
    -   Mean temp (GAM fit line)
    -   Ice thickness (GAM fit line)
    -   Lake and water level (GAM fit line)
    -   Annual temperature difference
    -   Annual ice difference
    -   Annual LL difference
- Assess model fits
  - Extract AIC, BIC, r2
  - Get coefficients and significance
  - Variable Inflation Factors to assess colinearity 
  - Output latex tables
  
`05_PaperStats.R`

- Scenario of melting ice vs. warming water temperature and the effect on heat
- Test synchrony of water column temperature and ice thickness using a variety of synchrony metrics (using GAM fits)
- Test autocorrelation between annual differences using `acf` function
- Test Variable-lag Granger Causality
