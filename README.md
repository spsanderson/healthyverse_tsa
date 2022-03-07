Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
07 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,695
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

The last day in the data set is 2022-03-05 19:53:40, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2307.25
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30695         |
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
| r_version     |     20420 |          0.33 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20420 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20420 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2548 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-05 | 2021-09-03 |      468 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1504091.28 | 1865287.01 | 357 | 16906 | 271098 | 3247239 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8275.75 |   15947.26 |   1 |   258 |   2660 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-05 19:53:40 | 2021-09-03 12:45:40 |    17853 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 0M 7S |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [409|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [401|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [350|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [326|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [140|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [16 x 6]> <tibble>     <split [0|16]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.8027265 | 276.602927 | 0.7204243 | 132.696982 | 1.0048958 | 0.1954299 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0713610 |  25.174608 | 0.0640445 |  23.962497 | 0.0948269 | 0.9905911 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7879318 | 312.275592 | 0.7071465 | 116.721331 | 0.9673521 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0929132 |  36.779692 | 0.0833870 |  26.144977 | 0.1229947 | 0.9921051 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2603325 | 115.899419 | 0.2336410 |  55.653964 | 0.3851563 | 0.8924377 |
| healthyR.data |         8 | NNAR                       | Test  | 0.6993040 | 207.796781 | 0.6276056 |  80.890141 | 1.1489709 | 0.6319403 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0859947 |  29.510812 | 0.0771778 |  23.756673 | 0.1214221 | 0.9897514 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3248672 | 111.442422 | 0.2915591 |  69.328283 | 0.3965483 | 0.9907891 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8227903 | 443.915944 | 0.7384310 |  94.449444 | 1.0823183 | 0.4299170 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2092343 | 517.993925 | 1.0852536 | 133.606678 | 1.4791216 | 0.0105241 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7349928 | 282.280825 | 0.6596353 | 115.091999 | 0.8983136 | 0.1160834 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7905416 | 315.292896 | 0.7094888 | 116.648332 | 0.9704782 | 0.0285225 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.6457548 | 189.939246 | 0.7422472 | 135.945045 | 0.8014558 | 0.1713118 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0459909 |  12.832434 | 0.0528632 |  16.280009 | 0.0538729 | 0.9979850 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6843461 | 392.253559 | 0.7866051 | 115.916083 | 0.9229601 | 0.0853437 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0414726 |   9.968959 | 0.0476697 |   9.911482 | 0.0492245 | 0.9979881 |
| healthyR      |         7 | EARTH                      | Test  | 0.0221514 |   5.357644 | 0.0254614 |   5.343748 | 0.0335485 | 0.9989594 |
| healthyR      |         8 | NNAR                       | Test  | 0.0553462 |  14.823921 | 0.0636163 |  16.520670 | 0.0730971 | 0.9922292 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0454531 |  12.744356 | 0.0522449 |  14.629935 | 0.0543915 | 0.9969873 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3359980 | 117.261361 | 0.3862047 |  96.417515 | 0.3873488 | 0.9993999 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9148696 | 588.007943 | 1.0515747 |  98.943588 | 1.2306816 | 0.4363441 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1578962 | 780.106441 | 1.3309157 | 120.685187 | 1.4376471 | 0.0517747 |
| healthyR      |        13 | TBATS                      | Test  | 0.7109891 | 366.401674 | 0.8172291 | 110.321856 | 0.9419320 | 0.0595192 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.7006668 | 335.413589 | 0.8053644 | 110.944012 | 0.9393485 | 0.1392833 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.6726327 | 154.712451 | 0.8014052 | 143.197496 | 0.8639012 | 0.1926931 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0569899 |  34.451935 | 0.0679003 |  23.558497 | 0.0771202 | 0.9927006 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.6752675 | 477.477937 | 0.8045444 | 113.888216 | 0.8427827 | 0.0150406 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0553805 |  32.609824 | 0.0659829 |  22.463285 | 0.0753031 | 0.9928980 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0117327 |   2.814790 | 0.0139788 |   2.760832 | 0.0154604 | 0.9996065 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.1009874 |  32.722071 | 0.1203210 |  23.693901 | 0.1774578 | 0.9659773 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0583043 |  23.066142 | 0.0694664 |  19.374375 | 0.0762808 | 0.9916652 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5483484 | 252.851269 | 0.6533272 | 128.048415 | 0.6234348 | 0.9871927 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9523198 | 339.627365 | 1.1346371 | 114.056367 | 1.3112046 | 0.2255416 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3038090 | 701.897143 | 1.5534173 | 128.585721 | 1.5762409 | 0.0039297 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.6447380 | 175.438525 | 0.7681702 | 134.954676 | 0.8672275 | 0.0545368 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6355050 | 403.461972 | 0.7571695 | 108.946150 | 0.7850213 | 0.0100445 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5862825 | 104.200853 | 0.5982007 | 125.120329 | 0.7419808 | 0.4129549 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0513351 |  16.252231 | 0.0523787 |  13.564958 | 0.0620263 | 0.9981456 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.2296364 | 625.359170 | 1.2546329 | 121.122815 | 1.4683344 | 0.0045484 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0508120 |  16.261753 | 0.0518449 |  13.583763 | 0.0614552 | 0.9981625 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0210017 |   3.353785 | 0.0214286 |   3.359465 | 0.0399624 | 0.9982870 |
| healthyverse  |         8 | NNAR                       | Test  | 0.2598202 | 133.998038 | 0.2651019 |  85.424075 | 0.3313387 | 0.9914931 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0537297 |  14.943138 | 0.0548219 |  14.305943 | 0.0650476 | 0.9975233 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2766947 |  43.572207 | 0.2823195 |  56.532506 | 0.3407636 | 0.9980832 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.7685522 | 277.531475 | 0.7841757 | 110.597392 | 0.9823891 | 0.4038263 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3944981 | 604.570461 | 1.4228459 | 125.232781 | 1.8139329 | 0.0020454 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7021256 | 260.189307 | 0.7163987 | 110.843828 | 0.9037733 | 0.0958977 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.2337075 | 652.429372 | 1.2587868 | 121.138918 | 1.4578057 | 0.0558278 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6232596 | 213.530223 | 0.7680685 | 135.405634 | 0.7456927 | 0.3185002 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7170384 | 352.961255 | 0.8836361 | 125.331382 | 0.9506586 | 0.0516859 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0660337 |  34.735123 | 0.0813761 |  27.658005 | 0.0852064 | 0.9967266 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0149220 |   2.525218 | 0.0183890 |   2.491895 | 0.0263403 | 0.9991176 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.1178559 |  83.471336 | 0.1452387 |  42.699547 | 0.1469078 | 0.9813889 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0695260 |  38.742558 | 0.0856798 |  33.194719 | 0.0899614 | 0.9955063 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3990788 | 202.781481 | 0.4918013 | 111.024829 | 0.4375232 | 0.9931992 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1231872 | 592.256242 | 1.3841500 | 138.822390 | 1.3867202 | 0.0644693 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.7100722 | 311.268721 | 0.8750513 | 123.688453 | 0.9340391 | 0.0415367 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6525205 | 297.377363 | 0.8041279 | 115.127402 | 0.8962251 | 0.0899814 |
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
    ## 1 health~         2 REGRESSION  Test   0.0714 25.2   0.0640 24.0   0.0948  0.991
    ## 2 health~         7 EARTH       Test   0.0222  5.36  0.0255  5.34  0.0335  0.999
    ## 3 health~         7 EARTH       Test   0.0117  2.81  0.0140  2.76  0.0155  1.00 
    ## 4 health~         7 EARTH       Test   0.0210  3.35  0.0214  3.36  0.0400  0.998
    ## 5 health~         7 EARTH       Test   0.0149  2.53  0.0184  2.49  0.0263  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [409|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [401|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [350|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [326|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [140|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [16 x 6]> <tibble>     <split [0|16]>   <mdl_time_tbl>

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
