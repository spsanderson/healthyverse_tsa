Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
02 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,026
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

The last day in the data set is 2022-02-28 23:48:45, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2191.17
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30026         |
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
| r_version     |     19934 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     19934 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     19934 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2510 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-02-28 | 2021-08-31 |      463 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1512898.37 | 1869562.10 | 357 | 19237.75 | 271097.5 | 3247952 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8303.54 |   15912.71 |   1 |   281.75 |   2720.0 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-02-28 23:48:45 | 2021-08-31 09:12:51 |    17503 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     17 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [404|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [396|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [345|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [321|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [135|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [11 x 6]> <tibble>     <split [0|11]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.9357729 | 249.594168 | 0.8130262 | 147.013855 | 1.1128244 | 0.3224766 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0890110 |  31.805989 | 0.0773353 |  27.449004 | 0.1066061 | 0.9914997 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7979117 | 175.584942 | 0.6932484 | 131.283160 | 0.9447823 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0708847 |  23.650457 | 0.0615867 |  19.979239 | 0.0926232 | 0.9922598 |
| healthyR.data |         7 | EARTH                      | Test  | 0.1411330 |  84.234364 | 0.1226204 |  32.899087 | 0.2362402 | 0.9555595 |
| healthyR.data |         8 | NNAR                       | Test  | 0.9312618 | 367.462902 | 0.8091068 | 132.061164 | 1.0137723 | 0.7927149 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0682344 |  20.225658 | 0.0592840 |  20.051673 | 0.0986901 | 0.9910577 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4330679 | 127.876329 | 0.3762618 |  85.897892 | 0.5224537 | 0.9938077 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8272791 | 370.102630 | 0.7187637 |  88.016138 | 1.0626673 | 0.5390525 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0728513 | 390.064183 | 0.9321238 | 125.947231 | 1.2952695 | 0.0532782 |
| healthyR.data |        13 | BATS                       | Test  | 0.8000461 | 181.476774 | 0.6951028 | 129.998861 | 0.9586017 | 0.0068862 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7978916 | 177.469487 | 0.6932310 | 130.821200 | 0.9448191 | 0.0076956 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.5602970 | 140.610087 | 0.6003403 | 124.509467 | 0.7261120 | 0.2798458 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0496398 |  14.056158 | 0.0531875 |  17.485676 | 0.0582531 | 0.9979341 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.7555190 | 405.553639 | 0.8095144 | 110.312974 | 1.0065701 | 0.1169747 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0622063 |  24.909488 | 0.0666520 |  28.980288 | 0.0718368 | 0.9979633 |
| healthyR      |         7 | EARTH                      | Test  | 0.0242098 |   5.436540 | 0.0259401 |   5.447297 | 0.0387447 | 0.9987066 |
| healthyR      |         8 | NNAR                       | Test  | 0.2353160 | 142.752349 | 0.2521336 |  57.465136 | 0.2695966 | 0.9671390 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0677448 |  31.448130 | 0.0725864 |  31.885548 | 0.0792231 | 0.9970550 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3430168 | 123.366486 | 0.3675315 |  96.704392 | 0.3939322 | 0.9993896 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8331296 | 422.065163 | 0.8926717 |  90.454031 | 1.1391434 | 0.5119816 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2165213 | 739.237769 | 1.3034636 | 125.705040 | 1.4192306 | 0.0854145 |
| healthyR      |        13 | TBATS                      | Test  | 0.7057643 | 312.941301 | 0.7562038 | 111.941593 | 0.9546318 | 0.0463999 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.7856364 | 428.061779 | 0.8417842 | 112.216734 | 1.0487562 | 0.0123154 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0567313 | 342.645589 | 1.0768688 | 164.605048 | 1.2499191 | 0.6928084 |
| healthyR.ts   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7301911 | 197.105899 | 0.7441059 | 135.272325 | 0.9241311 | 0.0617095 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0981681 |  58.556205 | 0.1000389 |  33.980057 | 0.1230195 | 0.9905157 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0151652 |   3.193721 | 0.0154542 |   3.109958 | 0.0212083 | 0.9994614 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.3408312 | 191.864122 | 0.3473262 |  70.856518 | 0.4369082 | 0.8788427 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0939645 |  44.177447 | 0.0957551 |  31.142572 | 0.1205922 | 0.9882073 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7120367 | 289.653694 | 0.7256055 | 138.180951 | 0.8071106 | 0.9751424 |
| healthyR.ts   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1308387 | 367.503882 | 1.1523884 | 132.548869 | 1.4687845 | 0.0449177 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.8778934 | 174.096547 | 0.8946229 | 160.984400 | 1.0752457 | 0.1260427 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.7422777 | 157.064401 | 0.7564228 | 143.639004 | 0.9571210 | 0.0714763 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.7614098 | 131.449268 | 0.7592690 | 149.996000 | 0.9360381 | 0.5297753 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0980383 |  33.881722 | 0.0977627 |  28.730127 | 0.1119536 | 0.9958658 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.9215516 | 432.968974 | 0.9189607 | 104.988415 | 1.1719310 | 0.0028706 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0972804 |  33.427661 | 0.0970069 |  28.528427 | 0.1113531 | 0.9958532 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0191157 |   2.595528 | 0.0190620 |   2.588286 | 0.0342969 | 0.9988512 |
| healthyverse  |         8 | NNAR                       | Test  | 0.6431990 | 417.071904 | 0.6413906 |  98.204510 | 0.7716726 | 0.9578510 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0998241 |  37.247611 | 0.0995435 |  28.570147 | 0.1150231 | 0.9957024 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3675613 |  61.319434 | 0.3665279 |  79.583392 | 0.4654003 | 0.9901482 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.7589856 | 254.560255 | 0.7568517 | 107.468113 | 0.9393437 | 0.4803435 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1689903 | 474.415232 | 1.1657036 | 121.744790 | 1.5249028 | 0.0020112 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7213616 | 131.411138 | 0.7193335 | 118.552287 | 0.9307354 | 0.0837212 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9248819 | 452.542188 | 0.9222816 | 105.162739 | 1.1545968 | 0.3826948 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6171258 | 218.403366 | 0.7018818 | 138.268495 | 0.7432174 | 0.2355957 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0958514 |  49.023199 | 0.1090157 |  40.759838 | 0.1078810 | 0.9969127 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.6835347 | 352.939958 | 0.7774114 | 120.878811 | 0.9230774 | 0.0368489 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0844406 |  37.114376 | 0.0960376 |  32.847002 | 0.0958356 | 0.9963762 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0149544 |   2.560995 | 0.0170082 |   2.526871 | 0.0270381 | 0.9990812 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.2757820 | 136.288359 | 0.3136579 |  68.761377 | 0.2998243 | 0.9695572 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0972636 |  48.616386 | 0.1106218 |  40.254933 | 0.1132462 | 0.9946096 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3428241 | 164.423940 | 0.3899075 | 103.346888 | 0.3834000 | 0.9992118 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9046451 | 348.678710 | 1.0288890 | 102.097345 | 1.2253605 | 0.3427086 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1980571 | 661.910947 | 1.3625982 | 143.786919 | 1.4435248 | 0.0534553 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.7288823 | 415.603905 | 0.8289870 | 124.783787 | 0.9656826 | 0.0378520 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6454432 | 344.200613 | 0.7340884 | 113.907923 | 0.8963407 | 0.0005284 |
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
    ## 1 health~         6 LM          Test   0.0709 23.7   0.0616 20.0   0.0926  0.992
    ## 2 health~         7 EARTH       Test   0.0242  5.44  0.0259  5.45  0.0387  0.999
    ## 3 health~         7 EARTH       Test   0.0152  3.19  0.0155  3.11  0.0212  0.999
    ## 4 health~         7 EARTH       Test   0.0191  2.60  0.0191  2.59  0.0343  0.999
    ## 5 health~         7 EARTH       Test   0.0150  2.56  0.0170  2.53  0.0270  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [404|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [396|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [345|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [321|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [135|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [11 x 6]> <tibble>     <split [0|11]>   <mdl_time_tbl>

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
