Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
04 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,362
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

The last day in the data set is 2022-03-02 23:05:56, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2238.46
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30362         |
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
| r_version     |     20159 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20159 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20159 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2530 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-02 | 2021-09-01 |      465 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |         sd |  p0 |   p25 |    p50 |        p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|-----------:|----:|------:|-------:|-----------:|--------:|:------|
| size          |         0 |             1 | 1508812.5 | 1867597.45 | 357 | 16923 | 271097 | 3247929.00 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8305.7 |   15900.01 |   1 |   300 |   2705 |    8307.25 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-02 23:05:56 | 2021-09-01 03:11:24 |    17669 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     26 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [406|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [398|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [347|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [323|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [137|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [13 x 6]> <tibble>     <split [0|13]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.8720935 | 242.338712 | 0.6997184 | 143.660478 | 1.0431425 | 0.2858396 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0686833 |  24.480932 | 0.0551076 |  20.642245 | 0.0966915 | 0.9916894 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.8018116 | 260.225523 | 0.6433282 | 122.706576 | 0.9575561 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.1046426 |  44.576704 | 0.0839593 |  30.477236 | 0.1337450 | 0.9927859 |
| healthyR.data |         7 | EARTH                      | Test  | 0.1928838 | 101.237831 | 0.1547590 |  43.563661 | 0.3133109 | 0.9255590 |
| healthyR.data |         8 | NNAR                       | Test  | 0.9188924 | 388.482046 | 0.7372672 | 111.459280 | 1.0643872 | 0.7740118 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.1085640 |  44.618041 | 0.0871056 |  32.289227 | 0.1424208 | 0.9913532 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3567218 | 102.694938 | 0.2862133 |  73.200358 | 0.4443033 | 0.9950651 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8019366 | 409.744442 | 0.6434285 |  92.076791 | 1.0401914 | 0.5041107 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1033836 | 439.435141 | 0.8852925 | 121.217796 | 1.3475747 | 0.0293902 |
| healthyR.data |        13 | TBATS                      | Test  | 0.8024625 | 265.082888 | 0.6438505 | 126.433335 | 0.9494545 | 0.0013939 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.8026837 | 262.872594 | 0.6440279 | 122.377089 | 0.9593609 | 0.0003291 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.5756911 | 209.160299 | 0.6562781 | 132.931159 | 0.7345553 | 0.2172441 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0471292 |  20.226004 | 0.0537265 |  24.442088 | 0.0550863 | 0.9977922 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.7197218 | 466.347129 | 0.8204706 | 124.059207 | 0.9667347 | 0.0922979 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0449496 |  17.624609 | 0.0512418 |  21.540854 | 0.0527213 | 0.9977999 |
| healthyR      |         7 | EARTH                      | Test  | 0.0138361 |   2.662643 | 0.0157729 |   2.577345 | 0.0352084 | 0.9985044 |
| healthyR      |         8 | NNAR                       | Test  | 0.1742498 | 109.017346 | 0.1986418 |  48.957032 | 0.2096679 | 0.9712116 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0495034 |  28.060132 | 0.0564330 |  28.690937 | 0.0598196 | 0.9966825 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3418848 | 156.893348 | 0.3897428 | 105.056450 | 0.3880070 | 0.9993588 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9081193 | 629.639424 | 1.0352406 |  98.936359 | 1.2580749 | 0.4075491 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3144595 | 941.051122 | 1.4984614 | 134.923291 | 1.5319770 | 0.0500699 |
| healthyR      |        13 | TBATS                      | Test  | 0.6758032 | 469.864199 | 0.7704042 | 112.802550 | 0.9096795 | 0.1990010 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6940909 | 401.181732 | 0.7912518 | 115.891670 | 0.9305584 | 0.0069056 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.6960694 | 215.527634 | 0.7754534 | 142.973509 | 0.9015174 | 0.0840174 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0607984 |  35.661399 | 0.0677322 |  23.920711 | 0.0799512 | 0.9933829 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.6859011 | 437.873382 | 0.7641254 | 119.185828 | 0.8564206 | 0.0226326 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0589238 |  33.234367 | 0.0656438 |  22.491515 | 0.0774595 | 0.9935634 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0128105 |   3.214322 | 0.0142714 |   3.132356 | 0.0163793 | 0.9995797 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.8326489 | 779.153213 | 0.9276092 | 113.313930 | 0.9792341 | 0.9442346 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0611421 |  21.808799 | 0.0681151 |  17.806929 | 0.0789576 | 0.9921747 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5584223 | 256.276997 | 0.6221082 | 129.179566 | 0.6383907 | 0.9864008 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9195494 | 256.701198 | 1.0244204 | 103.302865 | 1.3421793 | 0.2673774 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3013989 | 617.023525 | 1.4498183 | 133.037273 | 1.5673964 | 0.0091347 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.6561774 | 151.530629 | 0.7310119 | 136.466652 | 0.8782602 | 0.0376241 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6787206 | 428.484617 | 0.7561260 | 110.228099 | 0.8355325 | 0.0274450 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.7155098 | 127.751111 | 0.6935442 | 148.224372 | 0.9013914 | 0.5867350 |
| healthyverse  |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.0127786 | 482.489959 | 0.9816871 | 114.238464 | 1.2588626 | 0.0151974 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0519056 |  16.312597 | 0.0503121 |  13.740788 | 0.0663128 | 0.9962170 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0194786 |   2.772613 | 0.0188806 |   2.775234 | 0.0340452 | 0.9988718 |
| healthyverse  |         8 | NNAR                       | Test  | 0.3072092 | 183.393960 | 0.2977781 |  81.862134 | 0.3718918 | 0.9819001 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0554409 |  13.930645 | 0.0537389 |  13.222150 | 0.0702238 | 0.9959082 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3649906 |  74.578880 | 0.3537857 |  90.626266 | 0.4644849 | 0.9911375 |
| healthyverse  |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2036116 | 503.029318 | 1.1666617 | 125.753053 | 1.5701345 | 0.0169035 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7030825 | 149.940628 | 0.6814985 | 122.583858 | 0.8794328 | 0.1926192 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9812041 | 469.543641 | 0.9510820 | 114.159938 | 1.2010148 | 0.4010165 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.5878500 | 245.944477 | 0.6985796 | 133.306377 | 0.7085550 | 0.3922712 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7109565 | 389.570913 | 0.8448749 | 128.266981 | 0.9406797 | 0.0435275 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0638764 |  30.901166 | 0.0759083 |  24.929937 | 0.0801589 | 0.9963518 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0148065 |   2.578782 | 0.0175955 |   2.544652 | 0.0265144 | 0.9990830 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.1162936 |  81.154974 | 0.1381991 |  37.862814 | 0.1631924 | 0.9573905 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0653105 |  28.523608 | 0.0776127 |  26.183151 | 0.0794104 | 0.9945169 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3755585 | 220.711587 | 0.4463001 | 111.283215 | 0.4143710 | 0.9973898 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1683043 | 666.779715 | 1.3883705 | 143.245971 | 1.4426173 | 0.0632180 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6673368 | 335.093019 | 0.7930389 | 125.889743 | 0.9058821 | 0.0508766 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6636124 | 361.329540 | 0.7886129 | 121.241774 | 0.9001035 | 0.0000083 |
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
    ## 1 health~         2 REGRESSION  Test   0.0687 24.5   0.0551 20.6   0.0967  0.992
    ## 2 health~         7 EARTH       Test   0.0138  2.66  0.0158  2.58  0.0352  0.999
    ## 3 health~         7 EARTH       Test   0.0128  3.21  0.0143  3.13  0.0164  1.00 
    ## 4 health~         7 EARTH       Test   0.0195  2.77  0.0189  2.78  0.0340  0.999
    ## 5 health~         7 EARTH       Test   0.0148  2.58  0.0176  2.54  0.0265  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [406|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [398|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [347|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [323|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [137|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [13 x 6]> <tibble>     <split [0|13]>   <mdl_time_tbl>

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
