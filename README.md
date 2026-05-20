# nitrous-ph
An analysis of N<sub>2</sub>O reduction potential with pH management and nitrogen surplus reduction at the continental US scale.

### Description
This repository contains the R scripts and data files used for data processing, analysis, and visualization in the manuscript _Soil pH management yields modest national N₂O mitigation potential with significant regional concentration across US croplands_ (working title). The key objectives of the analysis are:

* To quantify national level potential for N<sub>2</sub>O emissions reduction as a function of pH management
* To understand how pH management interacts with N surplus reduction to effect N<sub>2</sub>O emissions
* To identify which counties or regions contribute the greatest to national N<sub>2</sub>O reduction potential

### Data
The data files used in this project are included in the `data` directory. The following datasets are provided:

* `Zhang_2021_nitrogen_data_county.csv`: Annual crop-specific nitrogen budget history from _Half-Century History of Crop Nitrogen Budget in the Conterminous United States: Variations Over Time, Space and Crop Types_ [(Zhang et al. 2021)](https://doi.org/10.6084/m9.figshare.13030436).
* `02102026_crop_soil_pH_recommendations.csv`: Crop-specific optimal soil pH recommendations sourced from multiple agricultural state extension services. **(NEEDS CITATION)**
*  `multistate_histosols_by_county.csv`: Soil series data from the Soil Survey Geographic Database [(SSURGO)](https://websoilsurvey.sc.egov.usda.gov/App/HomePage.htm).
* `EBK_Counties_RecDelta_and_ZonalAverage (With Counts and Percentages).csv` and `RF_Counties_RecDelta_and_ZonalAverage (With Counts and Percentages).csv`: Current pH predictions derived by Woollen et al. **(NEEDS CITATION)** using random forest and empirical Bayesian kriging models.
* `qiu_supplement.csv`: Supplmentary data used to simulate uncertainty factor for the quadratic model. See `Supplementary Data 1.xlsx` from [Qiu et al. 2024](https://figshare.com/articles/dataset/Intermediate_soil_acidification_induces_highest_nitrous_oxide_emissions/24591522).
* `N2O_summary_data_04012026.csv`: Raw data output from the `N2O_pH_model.R` script. Used in analysis and figures.
* `avoided_emissions_components.Rdata`: Intermediate emissions data used in analysis.
* `avoided_emissions_total.csv`: Avoided total emissions for each of the scenarios.

### R Scripts
The R scripts used are located in the `code` directory and structured as follows:

* `N2O_pH_model.R`: Calculates N<sub>2</sub>O emissions based on the linear and quadratic empirical models. Requires the `EBK_Counties_RecDelta_and_ZonalAverage (With Counts and Percentages).csv`, `RF_Counties_RecDelta_and_ZonalAverage (With Counts and Percentages).csv`, `Zhang_2021_nitrogen_data_county.csv`, `02102026_crop_soil_pH_recommendations.csv`, `02102026_crop_soil_pH_recommendations.csv`, `multistate_histosols_by_county.csv`, and `qiu_supplement.csv` files. Produces the  `N2O_summary_data_04012026.csv` raw data file needed for the other R scripts.
* `county_maps.R`: Computes county-level avoided N<sub>2</sub>O emissions then creates the map figures in the `plots` directory.
* `data_prep.R`: Calculates per row avoided N<sub>2</sub>O emissions, then applies a filter to outputs of the quadratic (Qiu) model for counties where liming increases N<sub>2</sub>O emissions and aggregates for a national summary.
* `figures.R`: Creates dot plot, stacked bar, bubble plot, and accumulation figures.
* `format_national_table.R`: Generates national summary table of avoided emissions.
* `supporting_figures.R`: Creates uncertainty decomposition figure.

The listed R packages must be installed prior to running the scripts: `tidyverse`, `maps`, `patchwork`, `sf`, `ggdist`, `flextable`, `officer`, and `ggrepel`. 

### Plots
All figures used in the manuscript main text and supplement are provided in the `plots` directory.

### Contact
For any questions or issues, please contact: Kelsey Jensen (kjensen@edf.org) or Victoria Dombrowik (vdombrowik@edf.org)
