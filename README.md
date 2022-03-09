Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
09 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,895
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

The last day in the data set is 2022-03-07 23:21:43, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2358.72
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30895         |
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
| r_version     |     20541 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20541 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20541 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2573 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-07 | 2021-09-06 |      470 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1503776.99 | 1864362.00 | 357 | 16906 | 271482 | 3247069 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8249.03 |   15914.42 |   1 |   270 |   2687 |    8235 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-07 23:21:43 | 2021-09-06 05:54:04 |    17979 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 10H 59M 39S |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [411|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [403|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [352|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [328|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [142|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [18 x 6]> <tibble>     <split [0|18]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.9948912 | 341.222922 | 0.9515133 | 146.008265 | 1.2084197 | 0.1369028 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0723831 |  26.262323 | 0.0692272 |  24.734361 | 0.0975551 | 0.9916874 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7650870 | 261.534077 | 0.7317287 | 117.548501 | 0.9302439 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0869915 |  33.431537 | 0.0831986 |  23.551929 | 0.1177447 | 0.9930658 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2842873 | 121.842443 | 0.2718922 |  58.185344 | 0.4052300 | 0.9008878 |
| healthyR.data |         8 | NNAR                       | Test  | 0.6614315 | 124.617673 | 0.6325927 |  73.573190 | 1.0106553 | 0.7279058 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0877391 |  30.656877 | 0.0839136 |  24.354242 | 0.1236775 | 0.9911195 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3600565 | 104.955341 | 0.3443578 |  70.129041 | 0.4483770 | 0.9895564 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9227934 | 558.262752 | 0.8825590 | 103.147159 | 1.1145397 | 0.4321926 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2292754 | 592.530895 | 1.1756782 | 133.302482 | 1.4591481 | 0.0073967 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7320578 | 167.078002 | 0.7001396 | 119.179878 | 0.8779581 | 0.0924905 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7654161 | 264.358944 | 0.7320434 | 117.137089 | 0.9309464 | 0.0917373 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.6691442 | 214.675890 | 0.8094858 | 150.130282 | 0.8204239 | 0.1627363 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0412062 |  11.422297 | 0.0498485 |  14.027080 | 0.0505107 | 0.9980252 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6829167 | 415.746053 | 0.8261469 | 123.186140 | 0.8961108 | 0.0659016 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0402430 |  11.946077 | 0.0486833 |  11.699344 | 0.0489543 | 0.9980237 |
| healthyR      |         7 | EARTH                      | Test  | 0.0213188 |   5.587978 | 0.0257901 |   5.586266 | 0.0369230 | 0.9986574 |
| healthyR      |         8 | NNAR                       | Test  | 0.0384772 |  16.688012 | 0.0465471 |  15.175151 | 0.0499968 | 0.9963673 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0441665 |  14.697571 | 0.0534297 |  17.109400 | 0.0542004 | 0.9970289 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3258793 | 130.211215 | 0.3942269 |  98.617667 | 0.3805586 | 0.9993719 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9342923 | 647.173022 | 1.1302443 | 108.380762 | 1.2423165 | 0.3893607 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1460265 | 820.553074 | 1.3863861 | 131.974640 | 1.4384677 | 0.0385078 |
| healthyR      |        13 | TBATS                      | Test  | 0.6527200 | 260.897782 | 0.7896169 | 140.888075 | 0.8448409 | 0.0163588 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.7250124 | 368.038208 | 0.8770715 | 122.164552 | 0.9393513 | 0.1617168 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.5802940 | 201.543167 | 0.7046002 | 137.397422 | 0.7559475 | 0.1394871 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0516189 |  34.951761 | 0.0626763 |  23.653387 | 0.0681921 | 0.9963243 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7571110 | 639.068369 | 0.9192935 | 111.092784 | 0.9383563 | 0.0038204 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0500032 |  33.148575 | 0.0607145 |  22.593128 | 0.0661822 | 0.9965313 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0107401 |   3.260056 | 0.0130408 |   3.169812 | 0.0127686 | 0.9997252 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.1550492 |  74.706200 | 0.1882627 |  48.621680 | 0.1962977 | 0.9516633 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0526740 |  23.887298 | 0.0639574 |  19.646081 | 0.0668544 | 0.9951007 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4345260 | 205.257901 | 0.5276068 | 114.066427 | 0.4887041 | 0.9966775 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9298228 | 408.431765 | 1.1290023 | 118.644745 | 1.2789769 | 0.1830751 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3470953 | 879.922591 | 1.6356597 | 131.871043 | 1.6220429 | 0.0006446 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.6414771 | 267.087232 | 0.7788894 | 141.019842 | 0.8181013 | 0.0029726 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.7180495 | 602.186983 | 0.8718646 | 110.674279 | 0.8624701 | 0.0053715 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5315987 |  98.279619 | 0.5654661 | 123.531669 | 0.6953949 | 0.2831249 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0476714 |  16.665518 | 0.0507085 |  13.889710 | 0.0570088 | 0.9989719 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.2212073 | 623.456634 | 1.2990086 | 125.459547 | 1.4695256 | 0.0012456 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0474003 |  16.780350 | 0.0504201 |  13.997514 | 0.0569392 | 0.9989774 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0161821 |   2.700234 | 0.0172130 |   2.706821 | 0.0282116 | 0.9988953 |
| healthyverse  |         8 | NNAR                       | Test  | 0.2835602 | 120.404538 | 0.3016254 |  76.239949 | 0.3782816 | 0.9795610 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0518362 |  16.149304 | 0.0551386 |  16.342969 | 0.0625504 | 0.9981692 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2277891 |  37.656914 | 0.2423012 |  45.264868 | 0.2906213 | 0.9981744 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.7437193 | 287.823832 | 0.7911005 | 110.469511 | 0.9383132 | 0.3716117 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3587847 | 603.363238 | 1.4453509 | 138.210184 | 1.7529892 | 0.0001697 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6412698 | 213.235603 | 0.6821241 | 116.173229 | 0.8513155 | 0.0497648 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.2692870 | 665.138535 | 1.3501514 | 127.969589 | 1.4818296 | 0.0143671 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6246748 | 221.630344 | 0.8321898 | 138.866479 | 0.7478776 | 0.2858840 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7089496 | 337.427117 | 0.9444605 | 130.773743 | 0.9192725 | 0.0349130 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0726677 |  41.243699 | 0.0968077 |  31.474074 | 0.0923702 | 0.9970469 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0147528 |   2.621128 | 0.0196536 |   2.585068 | 0.0263251 | 0.9990854 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0631982 |  45.598852 | 0.0841924 |  29.122344 | 0.0744806 | 0.9950005 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0759404 |  43.810939 | 0.1011676 |  36.282520 | 0.0953035 | 0.9955954 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3118263 | 142.094446 | 0.4154140 |  92.231302 | 0.3541627 | 0.9990465 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0883063 | 591.161770 | 1.4498382 | 146.319009 | 1.3398294 | 0.0455794 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6872620 | 324.816106 | 0.9155682 | 129.724304 | 0.8936101 | 0.0428567 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6707390 | 336.155281 | 0.8935564 | 118.458407 | 0.9221410 | 0.1920159 |
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
    ## 1 health~         2 REGRESSION  Test   0.0724 26.3   0.0692 24.7   0.0976  0.992
    ## 2 health~         7 EARTH       Test   0.0213  5.59  0.0258  5.59  0.0369  0.999
    ## 3 health~         7 EARTH       Test   0.0107  3.26  0.0130  3.17  0.0128  1.00 
    ## 4 health~         7 EARTH       Test   0.0162  2.70  0.0172  2.71  0.0282  0.999
    ## 5 health~         7 EARTH       Test   0.0148  2.62  0.0197  2.59  0.0263  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [411|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [403|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [352|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [328|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [142|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [18 x 6]> <tibble>     <split [0|18]>   <mdl_time_tbl>

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
