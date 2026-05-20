# Call relevant libraries
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# Read in pH prediction county datasets
ebk_csv <- read.csv("EBK_Counties_RecDelta_and_ZonalAverage (With Counts and Percentages).csv")
rf_csv <- read.csv("RF_Counties_RecDelta_and_ZonalAverage (With Counts and Percentages).csv")

# Joining EBK and RF data into one dataset, then subsetting and relabeling
pH_predictions <- left_join(rf_csv, ebk_csv,
                            join_by(State_1 == State_1, 
                                    County == County, 
                                    County_Num == County_Num), suffix = c("_RF", "_EBK"))

pH_predictions <- pH_predictions[, c(3, 6, 20, 22, 49, 51)]
names(pH_predictions)[names(pH_predictions) == "State_1"] <- "State"

# Reassigning names to counties that don't follow a standard rule
pH_predictions$County <- sub("DeKalb", "De Kalb", pH_predictions$County)
pH_predictions$County <- sub(" County", "", pH_predictions$County)
pH_predictions$County <- sub("DeWitt", "De Witt", pH_predictions$County)
pH_predictions$County <- sub("DuPage", "Du Page", pH_predictions$County)
pH_predictions$County <- sub("LaSalle", "La Salle", pH_predictions$County)
pH_predictions$County <- sub("LaPorte", "La Porte", pH_predictions$County)
pH_predictions$County <- sub("DeSoto", "De Soto", pH_predictions$County)
pH_predictions$County <- sub("LaMoure", "La Moure", pH_predictions$County)
pH_predictions$County <- sub("Oglala", "Oglala Lakota", pH_predictions$County)
pH_predictions$County <- sub("Doña Ana", "Dona Ana", pH_predictions$County)
pH_predictions$County <- sub("Virginia", "Virginia Beach City", pH_predictions$County)

# Finicky naming conventions
pH_predictions[436, "County"] <- "Jeff Davis"
pH_predictions[2830, "County"] <- "King And Queen"
pH_predictions[2831, "County"] <- "King George"
pH_predictions[2832, "County"] <- "King William"
pH_predictions[2844, "County"] <- "New Kent"
pH_predictions[2853, "County"] <- "Prince Edward"
pH_predictions[2907, "County"] <- "Suffolk City"
pH_predictions[2881, "County"] <- "Chesapeake City"
pH_predictions[2885, "County"] <- "Franklin City"

# Read in Zhang nitrogen data as one data frame
zhang_files <- list.files(path = "Zhang_2021_nitrogen_data_county/", pattern=".csv")

zhang_data <- do.call("rbind", lapply(paste0("Zhang_2021_nitrogen_data_county/", 
                                             zhang_files), read.csv))

# Setting negative N-surplus (underfertilizing) rows to zero
zhang_data <- zhang_data %>% 
  mutate(Nsurplus..kg.N.ha.yr. = 
           ifelse(Nsurplus..kg.N.ha.yr. < 0, 0, Nsurplus..kg.N.ha.yr.))

# Average nitrogen inputs and field area by county and crop, filtered between 2009 and 2019
zhang_data <- zhang_data %>%
  filter(YEAR >= 2009, YEAR <= 2019) %>%
  group_by(State, County, crop_type) %>%
  summarise(Average_Area = mean(Area..Mha. * 1000000),
            Actual_N_input = mean(Ninput..kg.N.ha.yr., na.rm = TRUE),
            Reduced_N_input = mean(Ninput..kg.N.ha.yr. - 
                                     (Nsurplus..kg.N.ha.yr. * 0.5), na.rm = TRUE),
            Total_N_Actual = mean(Ninput..kg.N.ha.yr. * 
                                    (Area..Mha. * 1000000), na.rm = TRUE),
            Total_N_Reduced = mean((Ninput..kg.N.ha.yr. - 
                                 (Nsurplus..kg.N.ha.yr. * 0.5)) * (Area..Mha. * 1000000)))

# Changing Zhang data frame columns to title case to make joins easier
zhang_data <- zhang_data %>%
  mutate(State = str_to_title(State), County = str_to_title(County),
         crop_type = str_to_title(crop_type))

# Again weird naming stuff to make sure joins happen
zhang_data[3789:3790, "County"] <- "Le Flore"
zhang_data[5159:5161, "County"] <- "Richmond City"

zhang_data$County <- sub("Mclean", "McLean", zhang_data$County)
zhang_data$County <- sub("Mcpherson", "McPherson", zhang_data$County)
zhang_data$County <- sub("Lapaz", "La Paz", zhang_data$County)
zhang_data$County <- sub("Mcdonough", "McDonough", zhang_data$County)
zhang_data$County <- sub("Mclennan", "McLennan", zhang_data$County)
zhang_data$County <- sub("Mcculloch", "McCulloch", zhang_data$County)
zhang_data$County <- sub("Mchenry", "McHenry", zhang_data$County)
zhang_data$County <- sub("Fond Du Lac", "Fond du Lac", zhang_data$County)
zhang_data$County <- sub("Isle Of Wight", "Isle of Wight", zhang_data$County)
zhang_data$County <- sub("Lagrange", "LaGrange", zhang_data$County)
zhang_data$County <- sub("O Brien", "O'Brien", zhang_data$County)
zhang_data$County <- sub("Mccracken", "McCracken", zhang_data$County)
zhang_data$County <- sub("Mcleod", "McLeod", zhang_data$County)
zhang_data$County <- sub("Lewis And Clark", "Lewis and Clark", zhang_data$County)
zhang_data$County <- sub("Lac Qui Parle", "Lac qui Parle", zhang_data$County)
zhang_data$County <- sub("Mckenzie", "McKenzie", zhang_data$County)
zhang_data$County <- sub("Mcnairy", "McNairy", zhang_data$County)
zhang_data$County <- sub("Mcminn", "McMinn", zhang_data$County)
zhang_data$County <- sub("Mcclain", "McClain", zhang_data$County)
zhang_data$County <- sub("Prince Georges", "Prince George's", zhang_data$County)
zhang_data$County <- sub("Queen Annes", "Queen Anne's", zhang_data$County)
zhang_data$County <- sub("St Marys", "St. Mary's", zhang_data$County)
zhang_data$County <- sub("Lake Of The Woods", "Lake of the Woods", zhang_data$County)
zhang_data$County <- sub("Mcdonald", "McDonald", zhang_data$County)
zhang_data$County <- sub("Mccone", "McCone", zhang_data$County)
zhang_data$County <- sub("Mcdowell", "McDowell", zhang_data$County)
zhang_data$County <- sub("Mcintosh", "McIntosh", zhang_data$County)
zhang_data$County <- sub("Mccurtain", "McCurtain", zhang_data$County)
zhang_data$County <- sub("Mckean", "McKean", zhang_data$County)
zhang_data$County <- sub("Mccook", "McCook", zhang_data$County)
zhang_data$County <- sub("St ", "St. ", zhang_data$County)
zhang_data$County <- sub("Saint", "St.", zhang_data$County)
zhang_data$County <- sub("Ste ", "Ste. ", zhang_data$County)

#Replace Durum as Durum Wheat to make later operation easier
zhang_data$crop_type <-sub("Durum", "Durum Wheat", zhang_data$crop_type)

# Join pH predictions to Zhang data, keeping all Zhang rows
zhang_pH <- left_join(zhang_data, pH_predictions,
                      join_by(State == State, County == County))

zhang_pH <- zhang_pH %>% rename(Crop = crop_type)

# Reading in Rec. and Max Rec. pH values
pH_recommendations <- read.csv("02102026_crop_soil_pH_recommendations.csv")

pH_recommendations_wide <- pH_recommendations %>% pivot_wider(
  id_cols = c(State, Crop),
  names_from = Soil_Type, 
  values_from = c(pH_Min, pH_Max))

# Joining recommendations to pH predictions
zhang_pH <- zhang_pH %>% left_join(pH_recommendations_wide, join_by(State, Crop))

# Removing basic soils
zhang_pH <- zhang_pH %>% filter(pH_Min_both != "basic soil" | is.na(pH_Min_both))

# Converting columns to numeric
zhang_pH <- zhang_pH %>%
  mutate(across(matches("pH_"), as.numeric))

# Reading in Mineral v. organic soils distinction
distinct_soils <- read.csv('multistate_histosols_by_county.csv') 

# Tidying county names
distinct_soils$areaname <- sub("\\s+County,.*$", "", distinct_soils$areaname, 
                               ignore.case = TRUE)
distinct_soils[121, 'areaname'] <- 'Haywood'
distinct_soils[284, 'areaname'] <- 'Haywood'

# Data has rows where soil types are given for two counties, splitting those
distinct_soils <- distinct_soils %>% mutate(
    areaname = str_remove(areaname, ",\\s*[A-Za-z ]+$"),
    areaname = str_remove(areaname, "\\s+Counties?$")) %>%
  separate_rows(areaname, sep = ",\\s*|\\s+and\\s+") %>%
  mutate(areaname = str_trim(areaname))

# Joining distinct soils to larger dataframe
# Replacing values based on soil content and cleaning up input data for MC
input_data <- left_join(zhang_pH, distinct_soils, join_by(State == state_name, 
                                                    County == areaname))

input_data <- input_data %>% mutate(pH_Min = ifelse(!is.na(pH_Min_both), pH_Min_both,
                                                     round(pH_Min_mineral*(mineral_pct/100) +
                                                       pH_Min_organic*(histosol_pct/100), 2)),
                                     pH_Max = ifelse(!is.na(pH_Max_both), pH_Max_both,
                                                     round(pH_Max_mineral*(mineral_pct/100) +
                                                       pH_Max_organic*(histosol_pct/100), 2)),
                                     pH_Avg = round((pH_Min + pH_Max)/2, 2))


input_data <- select(input_data, c(1:12, 29, 28))

# Recreating the residual SD from Qiu et al.
qiu_supplement <- read.csv("qiu_supplement.csv")

qiu_supplement <- qiu_supplement %>% filter(!is.na(H2O_pH),
                                            !is.na(EF....))

qiu_supplement <- qiu_supplement %>% 
  mutate(EF_hat = -0.0913 * H2O_pH^2 + 1.030 * H2O_pH - 1.826,
         residual = EF.... - EF_hat)

sigma_hat <- sqrt(sum(qiu_supplement$residual^2)/(nrow(qiu_supplement) - 3))

# Monte Carlo analysis
set.seed(123)

n_sims <- 10000
n_rows <- nrow(input_data)

# Prepare simulation matrices
pH_Avg_mat <- matrix(rep(input_data$pH_Avg, each = n_sims), nrow = n_rows, ncol = n_sims, byrow = TRUE)
pH_Max_mat <- matrix(rep(input_data$pH_Max, each = n_sims), nrow = n_rows, ncol = n_sims, byrow = TRUE)

RF_mat  <- matrix(rnorm(n_rows * n_sims, rep(input_data$MeanRF, each = n_sims), rep(input_data$STDRF, each = n_sims)),
                  nrow = n_rows, ncol = n_sims, byrow = TRUE)
EBK_mat <- matrix(rnorm(n_rows * n_sims, rep(input_data$MeanEBK, each = n_sims), rep(input_data$STDEBK, each = n_sims)),
                  nrow = n_rows, ncol = n_sims, byrow = TRUE)

# Model error
err_wang <- matrix(rnorm(n_rows * n_sims, 0, 1.22), nrow = n_rows, ncol = n_sims)
err_qiu  <- matrix(rnorm(n_rows * n_sims, 0, sigma_hat), nrow = n_rows, ncol = n_sims)

EF_WangAvg <- -0.38 * pH_Avg_mat + 3.55 + err_wang
EF_WangMax <- -0.38 * pH_Max_mat + 3.55 + err_wang
EF_WangRF  <- -0.38 * RF_mat + 3.55 + err_wang
EF_WangEBK <- -0.38 * EBK_mat + 3.55 + err_wang

EF_QiuAvg <- -0.0913 * pH_Avg_mat^2 + 1.03 * pH_Avg_mat - 1.826 + err_qiu
EF_QiuMax <- -0.0913 * pH_Max_mat^2 + 1.03 * pH_Max_mat - 1.826 + err_qiu
EF_QiuRF  <- -0.0913 * RF_mat^2 + 1.03 * RF_mat - 1.826 + err_qiu
EF_QiuEBK <- -0.0913 * EBK_mat^2 + 1.03 * EBK_mat - 1.826 + err_qiu

compute_N2O <- function(EF_mat, N_total) {
  EF_mat / 100 * N_total}

N2O_WangAvg_NActual   <- compute_N2O(EF_WangAvg, input_data$Total_N_Actual)
N2O_WangAvg_NReduced  <- compute_N2O(EF_WangAvg, input_data$Total_N_Reduced)
N2O_WangMax_NActual   <- compute_N2O(EF_WangMax, input_data$Total_N_Actual)
N2O_WangMax_NReduced  <- compute_N2O(EF_WangMax, input_data$Total_N_Reduced)
N2O_WangRF_NActual    <- compute_N2O(EF_WangRF, input_data$Total_N_Actual)
N2O_WangRF_NReduced   <- compute_N2O(EF_WangRF, input_data$Total_N_Reduced)
N2O_WangEBK_NActual   <- compute_N2O(EF_WangEBK, input_data$Total_N_Actual)
N2O_WangEBK_NReduced  <- compute_N2O(EF_WangEBK, input_data$Total_N_Reduced)

N2O_QiuAvg_NActual    <- compute_N2O(EF_QiuAvg, input_data$Total_N_Actual)
N2O_QiuAvg_NReduced   <- compute_N2O(EF_QiuAvg, input_data$Total_N_Reduced)
N2O_QiuMax_NActual    <- compute_N2O(EF_QiuMax, input_data$Total_N_Actual)
N2O_QiuMax_NReduced   <- compute_N2O(EF_QiuMax, input_data$Total_N_Reduced)
N2O_QiuRF_NActual     <- compute_N2O(EF_QiuRF, input_data$Total_N_Actual)
N2O_QiuRF_NReduced    <- compute_N2O(EF_QiuRF, input_data$Total_N_Reduced)
N2O_QiuEBK_NActual    <- compute_N2O(EF_QiuEBK, input_data$Total_N_Actual)
N2O_QiuEBK_NReduced   <- compute_N2O(EF_QiuEBK, input_data$Total_N_Reduced)

# Helper function
row_stats <- function(mat) {
  data.frame(mean = rowMeans(mat), sd = apply(mat, 1, sd))}

# Combine all EF summaries
EF_summary <- cbind(
  row_stats(EF_WangAvg),
  row_stats(EF_WangMax),
  row_stats(EF_WangRF),
  row_stats(EF_WangEBK),
  row_stats(EF_QiuAvg),
  row_stats(EF_QiuMax),
  row_stats(EF_QiuRF),
  row_stats(EF_QiuEBK))

colnames(EF_summary) <- c(
  "EF_WangAvg_mean","EF_WangAvg_sd",
  "EF_WangMax_mean","EF_WangMax_sd",
  "EF_WangRF_mean","EF_WangRF_sd",
  "EF_WangEBK_mean","EF_WangEBK_sd",
  "EF_QiuAvg_mean","EF_QiuAvg_sd",
  "EF_QiuMax_mean","EF_QiuMax_sd",
  "EF_QiuRF_mean","EF_QiuRF_sd",
  "EF_QiuEBK_mean","EF_QiuEBK_sd")

# Combine all N2O summaries
N2O_summary <- cbind(
  row_stats(N2O_WangAvg_NActual),
  row_stats(N2O_WangAvg_NReduced),
  row_stats(N2O_WangMax_NActual),
  row_stats(N2O_WangMax_NReduced),
  row_stats(N2O_WangRF_NActual),
  row_stats(N2O_WangRF_NReduced),
  row_stats(N2O_WangEBK_NActual),
  row_stats(N2O_WangEBK_NReduced),
  
  row_stats(N2O_QiuAvg_NActual),
  row_stats(N2O_QiuAvg_NReduced),
  row_stats(N2O_QiuMax_NActual),
  row_stats(N2O_QiuMax_NReduced),
  row_stats(N2O_QiuRF_NActual),
  row_stats(N2O_QiuRF_NReduced),
  row_stats(N2O_QiuEBK_NActual),
  row_stats(N2O_QiuEBK_NReduced))

# Column names (mean + sd)
colnames(N2O_summary) <- c(
  "N2O_WangAvg_NActual_mean","N2O_WangAvg_NActual_sd",
  "N2O_WangAvg_NReduced_mean","N2O_WangAvg_NReduced_sd",
  "N2O_WangMax_NActual_mean","N2O_WangMax_NActual_sd",
  "N2O_WangMax_NReduced_mean","N2O_WangMax_NReduced_sd",
  "N2O_WangRF_NActual_mean","N2O_WangRF_NActual_sd",
  "N2O_WangRF_NReduced_mean","N2O_WangRF_NReduced_sd",
  "N2O_WangEBK_NActual_mean","N2O_WangEBK_NActual_sd",
  "N2O_WangEBK_NReduced_mean","N2O_WangEBK_NReduced_sd",
  
  "N2O_QiuAvg_NActual_mean","N2O_QiuAvg_NActual_sd",
  "N2O_QiuAvg_NReduced_mean","N2O_QiuAvg_NReduced_sd",
  "N2O_QiuMax_NActual_mean","N2O_QiuMax_NActual_sd",
  "N2O_QiuMax_NReduced_mean","N2O_QiuMax_NReduced_sd",
  "N2O_QiuRF_NActual_mean","N2O_QiuRF_NActual_sd",
  "N2O_QiuRF_NReduced_mean","N2O_QiuRF_NReduced_sd",
  "N2O_QiuEBK_NActual_mean","N2O_QiuEBK_NActual_sd",
  "N2O_QiuEBK_NReduced_mean","N2O_QiuEBK_NReduced_sd")

# Combine with original input_data
output_final <- cbind(input_data, EF_summary, N2O_summary)

# Converting kg to CO2e, using AR6 GWP for N2O
CO2e_conversion <- function(x){x * (44/28) * 273}

summary_data <- output_final %>% mutate(
  across(c(N2O_WangAvg_NActual_mean:N2O_QiuEBK_NReduced_sd),
         CO2e_conversion, .names = "{.col}_CO2e"))

#write.csv(summary_data, "N2O_summary_data_04012026.csv")
