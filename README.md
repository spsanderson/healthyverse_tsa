Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
03 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,268
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

The last day in the data set is 2022-03-01 23:03:55, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2214.42
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30268         |
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
| r_version     |     20079 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20079 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20079 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2525 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-01 | 2021-08-31 |      464 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1511571.21 | 1868520.37 | 357 | 19045 | 271313.5 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8275.61 |   15878.67 |   1 |   288 |   2687.0 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-01 23:03:55 | 2021-08-31 19:30:59 |    17636 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      0 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [405|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [397|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [346|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [322|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [136|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [12 x 6]> <tibble>     <split [0|12]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.8379344 | 251.057304 | 0.6891571 | 134.833290 | 1.0206618 | 0.3665025 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0690847 |  23.840392 | 0.0568186 |  21.732680 | 0.0901034 | 0.9917614 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7983960 | 234.724059 | 0.6566389 | 126.825086 | 0.9462784 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0766116 |  30.824558 | 0.0630091 |  21.608296 | 0.1083950 | 0.9927470 |
| healthyR.data |         7 | EARTH                      | Test  | 0.1638595 | 100.799981 | 0.1347659 |  37.213512 | 0.2704773 | 0.9439274 |
| healthyR.data |         8 | NNAR                       | Test  | 1.1813132 | 544.466270 | 0.9715682 | 138.297802 | 1.2577608 | 0.7877121 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0843939 |  33.512336 | 0.0694095 |  25.199276 | 0.1212724 | 0.9915233 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3655911 | 107.323274 | 0.3006795 |  75.113777 | 0.4582078 | 0.9945208 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8279090 | 415.240000 | 0.6809117 |  93.705440 | 1.0531045 | 0.4932441 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1234732 | 444.492750 | 0.9239978 | 127.804398 | 1.3575808 | 0.0243042 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7909552 | 257.740139 | 0.6505192 | 126.600026 | 0.9372458 | 0.0247183 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7995681 | 237.372189 | 0.6576029 | 126.546175 | 0.9475978 | 0.0056130 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.5391893 | 159.510740 | 0.6077657 | 120.847493 | 0.7151164 | 0.2396577 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0457347 |  13.783518 | 0.0515515 |  17.161164 | 0.0542176 | 0.9979163 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.8386581 | 546.349284 | 0.9453222 | 120.582863 | 1.0735344 | 0.1461642 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0428423 |  11.590546 | 0.0482912 |  12.465050 | 0.0507764 | 0.9979270 |
| healthyR      |         7 | EARTH                      | Test  | 0.0236265 |   5.780766 | 0.0266314 |   5.868130 | 0.0380528 | 0.9986887 |
| healthyR      |         8 | NNAR                       | Test  | 0.8062487 | 372.859034 | 0.9087908 |  99.619164 | 1.0000284 | 0.7761563 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0455063 |  14.995750 | 0.0512940 |  18.616845 | 0.0546669 | 0.9971003 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3282762 | 138.756630 | 0.3700278 |  96.660512 | 0.3809234 | 0.9994681 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8455227 | 456.754714 | 0.9530599 |  96.381650 | 1.1376051 | 0.5239026 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2808975 | 869.964525 | 1.4438076 | 132.144026 | 1.4652005 | 0.1036309 |
| healthyR      |        13 | TBATS                      | Test  | 0.8435327 | 498.094705 | 0.9508168 | 117.539943 | 1.0970623 | 0.1217473 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.8444841 | 515.757889 | 0.9518892 | 118.738952 | 1.0985817 | 0.0223047 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7404696 | 223.302220 | 0.7862712 | 148.617473 | 0.9410985 | 0.0702023 |
| healthyR.ts   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.6519095 | 374.119375 | 0.6922333 | 118.980623 | 0.8223049 | 0.0193136 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0742344 |  20.301458 | 0.0788262 |  23.182915 | 0.0865791 | 0.9934829 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0124408 |   3.079254 | 0.0132103 |   2.999698 | 0.0162440 | 0.9995831 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.6423005 | 604.836631 | 0.6820299 | 109.005041 | 0.7542067 | 0.9585905 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0922945 |  43.098244 | 0.0980034 |  40.039449 | 0.1060393 | 0.9920613 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6049731 | 280.083377 | 0.6423936 | 134.784903 | 0.6799107 | 0.9837241 |
| healthyR.ts   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2535464 | 561.202843 | 1.3310842 | 127.977450 | 1.5442220 | 0.0103349 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7303755 | 148.513367 | 0.7755527 | 138.579955 | 0.9509391 | 0.0147494 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6385054 | 367.614932 | 0.6780000 | 106.417818 | 0.8013964 | 0.0248695 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.7361905 | 128.188267 | 0.7261388 | 148.192134 | 0.9145573 | 0.5772867 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0610846 |  16.023146 | 0.0602506 |  13.338053 | 0.0743186 | 0.9960390 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.9950472 | 471.978830 | 0.9814611 | 110.117383 | 1.2561709 | 0.0091772 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0607750 |  15.976413 | 0.0599452 |  13.338889 | 0.0739860 | 0.9960381 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0194851 |   2.710161 | 0.0192190 |   2.714411 | 0.0341102 | 0.9988768 |
| healthyverse  |         8 | NNAR                       | Test  | 0.4811877 | 308.375819 | 0.4746177 |  95.188385 | 0.5485479 | 0.9705178 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0617966 |  17.661741 | 0.0609529 |  16.309422 | 0.0739477 | 0.9961279 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3907854 |  86.465943 | 0.3854497 |  97.575921 | 0.4869783 | 0.9905432 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.7506501 | 241.293778 | 0.7404009 | 106.233585 | 0.9470712 | 0.5163640 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2309719 | 509.556200 | 1.2141646 | 127.049813 | 1.5898109 | 0.0106046 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7262239 | 211.560461 | 0.7163083 | 108.202753 | 0.9053194 | 0.1983016 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.0116075 | 487.010873 | 0.9977953 | 111.975108 | 1.2364870 | 0.4066609 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.5767187 | 222.423601 | 0.6632581 | 135.640878 | 0.6988445 | 0.2518010 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0531833 |  16.952411 | 0.0611637 |  15.566749 | 0.0646633 | 0.9971301 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7288151 | 430.499463 | 0.8381773 | 127.908307 | 0.9670568 | 0.0388174 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0573862 |  27.426232 | 0.0659973 |  22.011897 | 0.0730746 | 0.9965477 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0145088 |   3.061819 | 0.0166859 |   3.002437 | 0.0240650 | 0.9992886 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.1440557 | 140.995488 | 0.1656719 |  47.721071 | 0.1817880 | 0.9593470 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0592780 |  28.553159 | 0.0681730 |  25.422966 | 0.0731484 | 0.9952980 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3527752 | 208.116814 | 0.4057108 | 110.439384 | 0.3917564 | 0.9980187 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9035223 | 422.682288 | 1.0391001 | 111.390728 | 1.2148697 | 0.3574974 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2040178 | 762.531708 | 1.3846864 | 142.595879 | 1.4625067 | 0.0686814 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.7520745 | 452.174106 | 0.8649269 | 123.645220 | 0.9899338 | 0.0519101 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6798110 | 421.356920 | 0.7818198 | 118.088199 | 0.9336769 | 0.0005066 |
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
    ## 1 health~         2 REGRESSION  Test   0.0691 23.8   0.0568 21.7   0.0901  0.992
    ## 2 health~         7 EARTH       Test   0.0236  5.78  0.0266  5.87  0.0381  0.999
    ## 3 health~         7 EARTH       Test   0.0124  3.08  0.0132  3.00  0.0162  1.00 
    ## 4 health~         7 EARTH       Test   0.0195  2.71  0.0192  2.71  0.0341  0.999
    ## 5 health~         7 EARTH       Test   0.0145  3.06  0.0167  3.00  0.0241  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [405|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [397|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [346|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [322|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [136|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [12 x 6]> <tibble>     <split [0|12]>   <mdl_time_tbl>

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
