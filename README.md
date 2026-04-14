

# DATA 622 Final Project - Winter 2026

**Authors:** Catherine Dube, Guillermo Schneider, Tai Chou-Kudu
 
---

## How to Run

1. Knit `DATA 622 - Project MVP V1.Rmd` top to bottom — this regenerates `data/model_input.rds`
2. Open and run `app.R` to launch the Shiny app

> **Note:** The `randomForest` package must be installed before knitting. Run `install.packages("randomForest")` in your R console once if you haven't already.

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
