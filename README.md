Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
11 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,196
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,~
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M~
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:~
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4~
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N~
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA~
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N~
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR~
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0~
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", ~
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638~

The last day in the data set is 2022-03-09 20:54:42, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2404.27
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31196         |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     20737 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20737 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20737 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2585 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-09 | 2021-09-07 |      472 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1499090.26 | 1862438.91 | 357 | 16879 | 271097.5 | 3246663 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8285.82 |   15976.48 |   1 |   307 |   2670.0 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-09 20:54:42 | 2021-09-07 18:03:49 |    18150 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     52 |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Now that we have our full data set and saved our parameters we can
create the full data set.

``` r
horizon         <- 4*7
lag_period      <- 4*7
rolling_periods <- c(7, 14, 28)

data_prepared_full_tbl <- data_transformed_tbl %>%
  group_by(package) %>%
  
  # Add future windows
  bind_rows(
    future_frame(., .date_var = date, .length_out = horizon)
  ) %>%
  
  # Add autocorolated lags
  tk_augment_lags(value_trans, .lags = lag_period) %>%
  
  # Add rolling features
  tk_augment_slidify(
    .value     = value_trans_lag28
    , .f       = median
    , .period  = rolling_periods
    , .align   = "center"
    , .partial = TRUE
  ) %>%
  
  # Format columns
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .)) %>%
  select(date, package, everything()) %>%
  ungroup()

data_prepared_full_tbl %>% 
  group_by(package) %>% 
  pivot_longer(-c(date, package)) %>% 
  plot_time_series(
    .date_var = date
    , .value = value
    , .color_var = name
    , .smooth = FALSE
    , .interactive = FALSE
    , .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-data_prepared_full_tbl-1.png)<!-- -->

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
data_prepared_tbl <- data_prepared_full_tbl %>%
  filter(!is.na(value_trans))

forecast_tbl <- data_prepared_full_tbl %>%
  filter(is.na(value_trans))

nested_data_tbl <- data_prepared_tbl %>%
  nest_timeseries(
    .id_var = package
    , .length_future = horizon
  ) %>%
  split_nested_timeseries(
    .length_test = horizon
  )
```

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ .
  , data = extract_nested_test_split(nested_data_tbl)
  ) %>%
  step_mutate(yr = lubridate::year(date)) %>%
  step_harmonic(yr, frequency = 365/12, cycle_size = 1) %>%
  #step_rm(yr) %>%
  step_hai_fourier(value_trans, scale_type = "sincos", period = 365/12, order = 1) %>%
  step_lag(value_trans, lag = 1) %>%
  step_impute_knn(contains("lag_"))

recipe_base
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          5
    ## 
    ## Operations:
    ## 
    ## Variable mutation for lubridate::year(date)
    ## Harmonic numeric variables for yr
    ## Fourier Transformation on value_trans
    ## Lagging value_trans
    ## K-nearest neighbor imputation for contains("lag_")

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# Boosted Auto ARIMA ------------------------------------------------------

model_spec_arima_boosted <- arima_boost(
  min_n = 2
  , learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost")

wflw_arima_boosted <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_boosted)

# ETS ---------------------------------------------------------------------

model_spec_ets <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "ets") 

wflw_ets <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_ets)

model_spec_croston <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "croston")

wflw_croston <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_croston)

model_spec_theta <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "theta")

wflw_theta <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_theta)


# STLM ETS ----------------------------------------------------------------

model_spec_stlm_ets <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_ets")

wflw_stlm_ets <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_ets)

model_spec_stlm_tbats <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("tbats")

wflw_stlm_tbats <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_tbats)

model_spec_stlm_arima <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_arima")

wflw_stlm_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_arima)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)


# Prophet -----------------------------------------------------------------

model_spec_prophet <- prophet_reg(
  seasonality_yearly = "auto",
  seasonality_weekly = "auto",
  seasonality_daily = "auto"
) %>%
  set_engine(engine = "prophet")

wflw_prophet <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_prophet)

model_spec_prophet_boost <- prophet_boost(
  learn_rate = 0.1
  , trees = 10
  , seasonality_yearly = FALSE
  , seasonality_weekly = FALSE
  , seasonality_daily  = FALSE
) %>% 
  set_engine("prophet_xgboost") 

wflw_prophet_boost <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_prophet_boost)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

model_spec_glm <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>%
  set_engine("glmnet")

wflw_glm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_glm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)

# XGBoost -----------------------------------------------------------------

model_spec_xgboost <- boost_tree(
  mode  = "regression",
  mtry  = 10,
  trees = 100,
  min_n = 5,
  tree_depth = 3,
  learn_rate = 0.3,
  loss_reduction = 0.01
) %>%
  set_engine("xgboost")

wflw_xgboost <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_xgboost)
```

### Nested Modeltime Tables

``` r
parallel_start(n_cores)
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
  control = control_nested_fit(
    verbose = TRUE,
    allow_par = TRUE,
    cores = n_cores
  ),
  
  # Add workflows
  wflw_arima_boosted,
  wflw_auto_arima,
  wflw_croston,
  wflw_ets,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar,
  wflw_prophet,
  wflw_prophet_boost,
  wflw_stlm_arima,
  wflw_stlm_ets,
  wflw_stlm_tbats,
  wflw_theta,
  wflw_xgboost
)
parallel_stop()

nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [413|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [405|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [354|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [330|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [144|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [20 x 6]> <tibble>     <split [0|20]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.9333275 | 199.166607 | 0.9207030 | 146.904091 | 1.1561129 | 0.0001714 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0724350 |  23.503740 | 0.0714552 |  17.992063 | 0.0972240 | 0.9912001 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7624971 | 235.636257 | 0.7521833 | 111.442449 | 0.9391742 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0808028 |  33.586250 | 0.0797099 |  22.397701 | 0.1117756 | 0.9929852 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2734568 | 128.536264 | 0.2697579 |  50.377423 | 0.3912122 | 0.8957357 |
| healthyR.data |         8 | NNAR                       | Test  | 0.4876243 | 110.396464 | 0.4810285 |  73.049068 | 0.7191615 | 0.7536210 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0804305 |  27.942730 | 0.0793426 |  21.647381 | 0.1147238 | 0.9910346 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3222579 |  73.869098 | 0.3178989 |  62.779748 | 0.3953780 | 0.9934224 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8801174 | 408.443678 | 0.8682127 | 104.037769 | 1.0648688 | 0.4377524 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1319522 | 343.267873 | 1.1166411 | 122.955929 | 1.4268027 | 0.0080916 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7437953 | 218.909606 | 0.7337345 | 110.412490 | 0.9365169 | 0.0041013 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7640154 | 238.489181 | 0.7536811 | 111.187532 | 0.9415833 | 0.1377818 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.7413485 | 280.116067 | 0.8459784 | 148.424290 | 0.8890862 | 0.2697395 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0389169 |  12.586116 | 0.0444094 |  15.744948 | 0.0468785 | 0.9985810 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6539117 | 316.731653 | 0.7462012 | 140.726596 | 0.7785894 | 0.0866601 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0366186 |  10.435773 | 0.0417868 |  10.340943 | 0.0430164 | 0.9985882 |
| healthyR      |         7 | EARTH                      | Test  | 0.0176323 |   5.384029 | 0.0201209 |   5.413728 | 0.0214994 | 0.9994282 |
| healthyR      |         8 | NNAR                       | Test  | 0.0286227 |  25.111101 | 0.0326623 |  13.336825 | 0.0378239 | 0.9979980 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0400429 |  12.334791 | 0.0456944 |  13.736440 | 0.0473911 | 0.9976741 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3435895 | 159.344769 | 0.3920818 | 103.770713 | 0.3955194 | 0.9992640 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0195810 | 716.022096 | 1.1634791 | 120.012241 | 1.2609331 | 0.4154201 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0643544 | 750.131972 | 1.2145716 | 131.254121 | 1.3655009 | 0.0590909 |
| healthyR      |        13 | TBATS                      | Test  | 0.6744266 | 212.332679 | 0.7696115 | 156.234920 | 0.8100706 | 0.0835911 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6131754 | 125.811830 | 0.6997156 | 149.076441 | 0.8015980 | 0.0040099 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.6779385 | 185.981255 | 0.7689043 | 150.138627 | 0.8337187 | 0.3171002 |
| healthyR.ts   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7417480 | 451.816286 | 0.8412757 | 117.967673 | 0.9471425 | 0.0019173 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0462608 |  25.769719 | 0.0524681 |  18.628664 | 0.0616508 | 0.9971125 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0154498 |   3.279559 | 0.0175229 |   3.207960 | 0.0269342 | 0.9989899 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.2245561 |  86.893751 | 0.2546870 |  69.533682 | 0.2516247 | 0.9701127 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0482713 |  17.805391 | 0.0547483 |  15.746083 | 0.0616289 | 0.9959310 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4829298 | 216.377438 | 0.5477293 | 121.632368 | 0.5322897 | 0.9974425 |
| healthyR.ts   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2957277 | 666.243820 | 1.4695884 | 134.336293 | 1.6292594 | 0.0000323 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.6812977 | 187.618030 | 0.7727143 | 142.076053 | 0.8843708 | 0.0016763 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.7288955 | 457.770215 | 0.8266986 | 111.066353 | 0.9265574 | 0.0057583 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5360719 |  92.768558 | 0.5687928 | 119.177435 | 0.6933276 | 0.3604286 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0511653 |  18.494673 | 0.0542883 |  14.382101 | 0.0609636 | 0.9990417 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.1214554 | 545.996105 | 1.1899069 | 126.705528 | 1.3818071 | 0.0246591 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0511702 |  18.710356 | 0.0542935 |  14.558916 | 0.0612383 | 0.9990461 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0189619 |   3.072388 | 0.0201192 |   3.109944 | 0.0343887 | 0.9984122 |
| healthyverse  |         8 | NNAR                       | Test  | 0.0774664 |  38.983553 | 0.0821948 |  33.189830 | 0.0902309 | 0.9959353 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0569292 |  16.322830 | 0.0604040 |  16.075194 | 0.0678285 | 0.9982049 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2507978 |  43.870938 | 0.2661060 |  58.102279 | 0.3031469 | 0.9972200 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.7777870 | 275.985038 | 0.8252617 | 107.509617 | 0.9515856 | 0.4438658 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2294937 | 509.942127 | 1.3045397 | 134.828974 | 1.6336320 | 0.0178634 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6109240 | 141.755138 | 0.6482136 | 130.258294 | 0.7982043 | 0.0958899 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.1000770 | 535.197593 | 1.1672237 | 126.753934 | 1.3314994 | 0.0388895 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6395829 | 307.209069 | 0.8338912 | 134.581612 | 0.7723223 | 0.4218715 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.5624642 | 207.453436 | 0.7333435 | 140.160921 | 0.7246256 | 0.0441325 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0627965 |  36.230674 | 0.0818744 |  25.089434 | 0.0787236 | 0.9988508 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0110894 |   2.245708 | 0.0144585 |   2.223345 | 0.0142103 | 0.9996515 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0362734 |  23.608407 | 0.0472934 |  15.593922 | 0.0524061 | 0.9947078 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0620401 |  35.002805 | 0.0808881 |  25.552251 | 0.0748514 | 0.9978138 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3183202 | 162.359825 | 0.4150274 |  95.211541 | 0.3587454 | 0.9990410 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0097834 | 540.824355 | 1.3165603 | 145.956621 | 1.2643818 | 0.0580176 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.5646820 | 218.822356 | 0.7362350 | 134.357983 | 0.7120069 | 0.0739806 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.5709717 | 188.004258 | 0.7444356 | 143.636037 | 0.7059910 | 0.0057256 |
| healthyR.ai   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         1 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         4 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         6 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         7 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         8 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         9 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        10 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        12 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        13 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        14 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 10
    ##   package .model_id .model_desc .type     mae  mape    mase smape    rmse    rsq
    ##   <chr>       <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl>  <dbl>
    ## 1 health~         2 REGRESSION  Test   0.0724 23.5   0.0715 18.0   0.0972  0.991
    ## 2 health~         7 EARTH       Test   0.0176  5.38  0.0201  5.41  0.0215  0.999
    ## 3 health~         7 EARTH       Test   0.0154  3.28  0.0175  3.21  0.0269  0.999
    ## 4 health~         7 EARTH       Test   0.0190  3.07  0.0201  3.11  0.0344  0.998
    ## 5 health~         7 EARTH       Test   0.0111  2.25  0.0145  2.22  0.0142  1.00 
    ## 6 TidyDe~        NA <NA>        <NA>  NA      NA    NA      NA    NA      NA

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
parallel_start(n_cores)
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = TRUE, 
      cores = n_cores
    )
  )
parallel_stop()

nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [413|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [405|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [354|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [330|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [144|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [20 x 6]> <tibble>     <split [0|20]>   <mdl_time_tbl>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
