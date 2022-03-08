Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
08 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,736
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

The last day in the data set is 2022-03-06 22:50:36, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2334.2
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30736         |
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
| r_version     |     20430 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20430 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20430 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2561 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-06 | 2021-09-03 |      469 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1504559.50 | 1865095.35 | 357 | 16906 | 271104 | 3247218 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8269.97 |   15939.49 |   1 |   253 |   2660 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-06 22:50:36 | 2021-09-03 16:52:55 |    17887 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   11.5 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [410|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [402|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [351|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [327|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [141|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [17 x 6]> <tibble>     <split [0|17]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.8940758 | 309.095323 | 0.8466924 | 139.513786 | 1.0961633 | 0.1486889 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0760588 |  24.721068 | 0.0720280 |  24.525224 | 0.0994947 | 0.9898518 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7193645 | 226.405120 | 0.6812403 | 117.350955 | 0.8807800 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0905607 |  33.343329 | 0.0857612 |  24.584261 | 0.1204137 | 0.9910119 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2782526 | 111.789439 | 0.2635060 |  56.395134 | 0.3974508 | 0.8916983 |
| healthyR.data |         8 | NNAR                       | Test  | 0.5415266 | 128.719298 | 0.5128272 |  79.489078 | 0.7077351 | 0.7651342 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0926735 |  31.174518 | 0.0877621 |  25.460525 | 0.1275524 | 0.9885061 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3401312 | 103.154262 | 0.3221053 |  72.097166 | 0.4137338 | 0.9910799 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9242686 | 527.836608 | 0.8752851 | 104.110259 | 1.1208818 | 0.3760638 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2748835 | 566.491894 | 1.2073183 | 134.886941 | 1.4869005 | 0.0011339 |
| healthyR.data |        13 | BATS                       | Test  | 0.6718401 | 186.165986 | 0.6362345 | 114.012851 | 0.8340468 | 0.0960017 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7200566 | 228.890070 | 0.6818957 | 116.988033 | 0.8818121 | 0.0258041 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.6382804 | 190.881253 | 0.7007344 | 135.330410 | 0.7999829 | 0.1680845 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0432083 |  11.414910 | 0.0474361 |  13.322883 | 0.0517636 | 0.9979987 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6819134 | 375.103569 | 0.7486367 | 119.472656 | 0.9103547 | 0.0681882 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0402972 |  10.343225 | 0.0442401 |  10.031171 | 0.0488015 | 0.9979992 |
| healthyR      |         7 | EARTH                      | Test  | 0.0218286 |   5.203717 | 0.0239645 |   5.181894 | 0.0371990 | 0.9986585 |
| healthyR      |         8 | NNAR                       | Test  | 0.0530108 |  19.422485 | 0.0581978 |  21.180181 | 0.0667100 | 0.9942186 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0445003 |  13.486588 | 0.0488545 |  15.809169 | 0.0542302 | 0.9969790 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3290798 | 117.724336 | 0.3612794 |  96.027247 | 0.3827417 | 0.9993976 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9206095 | 599.046534 | 1.0106886 | 103.078589 | 1.2306848 | 0.4192106 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1510102 | 773.741451 | 1.2636334 | 123.830532 | 1.4400572 | 0.0434741 |
| healthyR      |        13 | TBATS                      | Test  | 0.6585598 | 257.076737 | 0.7229980 | 123.784944 | 0.8621200 | 0.0279183 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.7229894 | 339.424406 | 0.7937319 | 117.976744 | 0.9469953 | 0.1475440 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.5747932 | 205.176727 | 0.7275537 | 136.841030 | 0.7460950 | 0.1138369 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0539168 |  24.537258 | 0.0682461 |  19.558821 | 0.0651514 | 0.9956971 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7767876 | 642.911547 | 0.9832313 | 107.216702 | 0.9597039 | 0.0159840 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0529286 |  22.873722 | 0.0669952 |  18.556607 | 0.0637383 | 0.9959000 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0111434 |   3.346242 | 0.0141050 |   3.245703 | 0.0131401 | 0.9997216 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.2110190 |  98.467282 | 0.2671007 |  67.873868 | 0.2545562 | 0.9583132 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0565996 |  15.443636 | 0.0716418 |  15.047134 | 0.0676893 | 0.9945355 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4762504 | 214.949308 | 0.6028216 | 121.754149 | 0.5254656 | 0.9962854 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9925718 | 399.775579 | 1.2563636 | 119.341450 | 1.3394180 | 0.1996392 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.4491434 | 895.279775 | 1.8342764 | 132.509670 | 1.7063735 | 0.0059840 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.6392989 | 241.094313 | 0.8092028 | 137.504774 | 0.8241960 | 0.0055027 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6995467 | 571.492891 | 0.8854624 | 105.039595 | 0.8517347 | 0.0134668 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5781955 | 104.658288 | 0.5891957 | 124.784690 | 0.7388465 | 0.4199461 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0472082 |  16.869621 | 0.0481064 |  14.391429 | 0.0570688 | 0.9986009 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.0911198 | 540.544309 | 1.1118786 | 119.558077 | 1.3615303 | 0.0000125 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0471532 |  17.041940 | 0.0480503 |  14.593727 | 0.0571118 | 0.9986160 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0166204 |   2.646019 | 0.0169366 |   2.644846 | 0.0283908 | 0.9990632 |
| healthyverse  |         8 | NNAR                       | Test  | 0.3120500 | 137.473085 | 0.3179868 |  86.134607 | 0.4156894 | 0.9820030 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0521052 |  16.661707 | 0.0530966 |  17.096622 | 0.0627166 | 0.9979205 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2489217 |  37.189841 | 0.2536575 |  45.506411 | 0.3189512 | 0.9983708 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.7252024 | 276.988277 | 0.7389995 | 111.226822 | 0.9456760 | 0.4084628 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2836436 | 560.312719 | 1.3080652 | 133.660765 | 1.6908299 | 0.0003309 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6491743 | 185.657198 | 0.6615250 | 120.432320 | 0.8385267 | 0.0931022 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.1588539 | 594.555276 | 1.1809013 | 122.803630 | 1.3858445 | 0.0634672 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6163502 | 209.178498 | 0.7403327 | 133.194681 | 0.7447506 | 0.3042312 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7153839 | 334.437707 | 0.8592876 | 130.351213 | 0.9367323 | 0.0444852 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0699795 |  36.692209 | 0.0840563 |  29.665373 | 0.0884451 | 0.9967081 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0146902 |   2.982757 | 0.0176452 |   2.923713 | 0.0239460 | 0.9993106 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0588036 |  37.744396 | 0.0706323 |  24.237809 | 0.0708625 | 0.9941682 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0732940 |  39.977781 | 0.0880376 |  35.202719 | 0.0921045 | 0.9953189 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3576685 | 166.868317 | 0.4296156 | 101.537432 | 0.3985456 | 0.9976154 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0854189 | 558.960237 | 1.3037572 | 144.527536 | 1.3396083 | 0.0535191 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6760685 | 292.195340 | 0.8120636 | 135.669672 | 0.8867669 | 0.0401728 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6695039 | 298.597359 | 0.8041785 | 122.064854 | 0.8993481 | 0.1008558 |
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
    ## 1 health~         2 REGRESSION  Test   0.0761 24.7   0.0720 24.5   0.0995  0.990
    ## 2 health~         7 EARTH       Test   0.0218  5.20  0.0240  5.18  0.0372  0.999
    ## 3 health~         7 EARTH       Test   0.0111  3.35  0.0141  3.25  0.0131  1.00 
    ## 4 health~         7 EARTH       Test   0.0166  2.65  0.0169  2.64  0.0284  0.999
    ## 5 health~         7 EARTH       Test   0.0147  2.98  0.0176  2.92  0.0239  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [410|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [402|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [351|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [327|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [141|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [17 x 6]> <tibble>     <split [0|17]>   <mdl_time_tbl>

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
