

# DATA 622 Final Project - Winter 2026

**Authors:** Catherine Dube, Guillermo Schneider, Tai Chou-Kudu
 
---

## How to Run

1. Knit `DATA 622 - Project MVP V1.Rmd` top to bottom — this regenerates `data/model_input.rds`
2. Open and run `app.R` to launch the Shiny app

> **Note:** The `randomForest` package must be installed before knitting. Run `install.packages("randomForest")` in your R console once if you haven't already.
> 
> **Note:** `nyc_census_tracts.rds` was updated with a new car ownership feature (`pct_2plus_cars`). Re-knit from the `vehicle-availability` chunk onward to pick up this change. You will need a Census API key for that chunk.

---

## SETUP INSTRUCTIONS 
Before running any code in this repository,

### 1. Install Git LFS (Large File Storage)
There are massive `.parquet` files from the TLC and heavy `.rds` spatial files, standard Git will not pull the data correctly.  

**To fix this:**
1. Download and install Git LFS from [git-lfs.github.com](https://git-lfs.github.com/) (or run `brew install git-lfs` on Mac).
2. Open terminal, navigate to this project folder, and run:
   ```bash
   git lfs install
   git lfs pull
   ```

### 2. Set Up a Census API Key
The data_prep.Rmd file dynamically pulls 2022 ACS data directly from the US Census Bureau using the tidycensus package. It needs an api key though.

To fix this:

Request a free API key at [https://api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html). 

In RStudio:

   ```r
   usethis::edit_r_environ()
   ```

The .Renviron file will open. Add the key:

   ```CENSUS_API_KEY="your_actual_api_key_string_here"
   Restart R (Session -> Restart R). The get_acs() function will now automatically find your key.
   ```

---

## Project Status & Next Directions

### What's Built
- Full data pipeline: 2022 ACS census features, TLC Oct 2023 trip counts, Citi Bike Oct 2023, subway entrance distances, IBX station geocoding
- Linear regression baseline with IBX sensitivity simulation
- Shiny app with linear predictions, IBX simulation, demographics, and infrastructure layers
- Borough fixed effects to account for neighborhood heterogeneity across the five boroughs
- Random Forest model with 80/20 train/test evaluation (RMSE + R² comparison)
- RF feature importance plot
- RF IBX simulation — predicts commute relief under IBX scenario
- RF residuals map layer — shows where the model over/under-predicts geographically
- RF layers added to Shiny app alongside existing linear layers

> **Tai's contributions (April 2026):** borough fixed effects, Random Forest model + evaluation, RF feature importance, RF IBX simulation, residuals map layer, RF Shiny app layers

### High Priority Next Steps
- **Cars owned variable** — add ACS `B25044` to the census pull and engineer a car-ownership feature; flagged as likely the strongest missing predictor and a key source of omitted-variable bias
- **K-fold cross-validation** — more robust evaluation than a single 80/20 split given our dataset size
- **Spatial lag features** — include averages of neighboring tract values to account for spatial autocorrelation; used in similar transit research and directly relevant to the IBX corridor analysis

### Stretch Goals
- **Causal forest** — estimates heterogeneous treatment effects per neighborhood; the most rigorous way to answer "which neighborhoods benefit most from IBX"
- **Event study** — use the Second Ave Q extension opening as a natural experiment to validate whether the model correctly captures the sensitivity of commute patterns to new transit

### Known Limitations
- No car-ownership variable yet — likely causing omitted-variable bias
- IBX simulation sets transit distance to the global minimum z-score, which is a simplification
- `pct_commute_60p` only captures workers who commute — does not reflect non-work trips or people who avoid long-commute jobs entirely
- Spatial autocorrelation between neighboring tracts not yet addressed
