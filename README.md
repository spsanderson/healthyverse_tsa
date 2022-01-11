Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
11 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,430
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

The last day in the data set is 2022-01-09 20:49:26, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -988.18
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26430          |
| Number of columns                                | 11             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| character                                        | 6              |
| Date                                             | 1              |
| numeric                                          | 2              |
| POSIXct                                          | 1              |
| Timespan                                         | 1              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| r\_version     |      17660 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17660 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17660 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2223 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-09 | 2021-08-04 |       413 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1535261.79 | 1879577.66 | 357 | 19045 | 238826 | 3246662 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8087.08 |   15317.04 |   1 |   205 |   2823 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-09 20:49:26 | 2021-08-04 18:31:06 |     15434 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   12.5 |        60 |

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
  step_rm(yr) %>%
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
    ## Variable mutation
    ## Harmonic numeric variables for yr
    ## Delete terms yr
    ## Fourier transformation on value_trans
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
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [384 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [374 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [324 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [299 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [113 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7897735 |  304.693335 | 0.7238892 | 103.742010 | 0.9729403 | 0.4433521 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0867622 |   26.628366 | 0.0795243 |  27.219609 | 0.1061597 | 0.9920504 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.2616563 |  523.022205 | 1.1564066 | 116.158950 | 1.5135478 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.1024854 |   36.192963 | 0.0939359 |  27.719637 | 0.1203259 | 0.9934326 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0207901 |    8.451609 | 0.0190558 |   6.400842 | 0.0374978 | 0.9985860 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0292078 |   23.400963 | 0.0267713 |  10.536733 | 0.0566968 | 0.9959574 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.1069288 |   35.326512 | 0.0980086 |  28.020254 | 0.1312558 | 0.9915980 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5225971 |  244.162525 | 0.4790011 |  89.570221 | 0.6186887 | 0.9903497 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1557834 |  596.931624 | 1.0593659 | 111.615528 | 1.3975234 | 0.5508166 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6395308 |  396.996332 | 1.5027582 | 125.087906 | 1.9837397 | 0.1850664 |
| healthyR.data |         13 | TBATS                      | Test  | 1.2730458 |  489.386858 | 1.1668460 | 118.353590 | 1.4982500 | 0.1872859 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.2599499 |  522.236717 | 1.1548426 | 116.105847 | 1.5119132 | 0.0045996 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 1.1615667 |  433.680174 | 1.2166409 | 157.602087 | 1.2231021 | 0.4541066 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0371189 |   14.385942 | 0.0388789 |  17.085783 | 0.0436902 | 0.9990926 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.3558336 |  562.224898 | 1.4201189 | 155.436434 | 1.4754331 | 0.0303185 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0407477 |   18.157900 | 0.0426797 |  18.804012 | 0.0496952 | 0.9991100 |
| healthyR      |          7 | EARTH                      | Test  | 0.0182267 |    5.376493 | 0.0190909 |   5.040967 | 0.0211709 | 0.9993576 |
| healthyR      |          8 | NNAR                       | Test  | 0.0700236 |   11.862523 | 0.0733436 |  11.458119 | 0.2431324 | 0.9407895 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0459460 |   20.836872 | 0.0481245 |  20.323177 | 0.0609629 | 0.9981041 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4057930 |  179.276250 | 0.4250332 | 117.256409 | 0.4698775 | 0.9956047 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3260085 |  671.003123 | 1.3888796 | 125.921292 | 1.5723963 | 0.5010185 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6910654 |  527.091469 | 1.7712452 | 143.205768 | 2.0946294 | 0.2348052 |
| healthyR      |         13 | TBATS                      | Test  | 1.3676523 |  557.558920 | 1.4324979 | 157.441255 | 1.4714460 | 0.0925885 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3080027 |  577.561847 | 1.3700201 | 152.349172 | 1.4495373 | 0.0535813 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 1.0365307 |  391.186400 | 0.6688377 | 113.801605 | 1.2864139 | 0.1533913 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0523669 |   25.860472 | 0.0337906 |  13.386932 | 0.0615802 | 0.9972655 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.7163940 |  797.280462 | 1.1075302 | 130.626732 | 1.9216546 | 0.1298420 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0721932 |   15.357335 | 0.0465838 |  13.988982 | 0.0908712 | 0.9973985 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0264751 |    1.958851 | 0.0170835 |   1.947755 | 0.0559730 | 0.9971644 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0640832 |   18.984764 | 0.0413507 |  14.031595 | 0.1825701 | 0.9746930 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0953034 |   26.749928 | 0.0614960 |  24.802193 | 0.1188422 | 0.9959595 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4006551 |  164.546234 | 0.2585290 |  88.098902 | 0.4811531 | 0.9898406 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3694751 |  459.031979 | 0.8836753 | 126.781683 | 1.5360160 | 0.4591007 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 2.1441318 | 1051.108515 | 1.3835348 | 139.229392 | 2.5331736 | 0.0954972 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.8566842 |  843.437472 | 1.1980547 | 132.151717 | 2.0782609 | 0.0922620 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6579173 |  755.378235 | 1.0697972 | 126.842143 | 1.9300021 | 0.0048898 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2399230 |  676.782732 | 1.2159095 | 163.758763 | 1.3706896 | 0.5891325 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0609528 |   36.233821 | 0.0597723 |  27.545784 | 0.0764500 | 0.9962713 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.4776157 |  825.184789 | 1.4489989 | 167.040610 | 1.6080732 | 0.2671543 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0668800 |   25.206983 | 0.0655847 |  19.969786 | 0.0818991 | 0.9973123 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0092418 |    4.145023 | 0.0090628 |   3.881986 | 0.0135632 | 0.9998032 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0055295 |    2.441392 | 0.0054224 |   2.224248 | 0.0084898 | 0.9998869 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0569387 |   22.243898 | 0.0558359 |  16.740781 | 0.0702603 | 0.9970033 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5341609 |  318.918757 | 0.5238159 | 128.539725 | 0.6033548 | 0.9766755 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2813163 |  827.272389 | 1.2565012 | 130.400474 | 1.4482887 | 0.5781653 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7349773 | 1076.699202 | 1.7013762 | 152.224904 | 2.0726128 | 0.2713712 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8955023 |  417.664430 | 0.8781592 | 159.153936 | 1.0586326 | 0.3066860 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.3925935 |  763.921484 | 1.3656233 | 163.745728 | 1.5729212 | 0.0388233 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.6686894 |  367.595518 | 1.1693373 | 132.316026 | 1.8650106 | 0.5050672 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1127398 |   19.522632 | 0.0790027 |  21.202603 | 0.1333740 | 0.9964012 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1974802 |  267.541121 | 0.8391366 | 117.464545 | 1.4993055 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1109032 |   19.395641 | 0.0777157 |  21.280235 | 0.1307041 | 0.9967979 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0270805 |    3.516705 | 0.0189767 |   3.465594 | 0.0403294 | 0.9984923 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2982800 |   35.925159 | 0.2090203 |  47.887696 | 0.4003141 | 0.9189167 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1479472 |   25.580406 | 0.1036743 |  26.787557 | 0.1712615 | 0.9930912 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.1070697 |  249.490565 | 0.7757812 | 123.936006 | 1.2393979 | 0.8794505 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2542028 |  242.957940 | 0.8788851 | 122.762612 | 1.4128264 | 0.5104543 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7003404 |  385.050628 | 1.1915168 | 137.725629 | 1.9876661 | 0.1165790 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.3953884 |  315.581885 | 0.9778211 | 124.107960 | 1.6595138 | 0.1104144 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.3392465 |  302.726540 | 0.9384796 | 120.745996 | 1.6489751 | 0.0157537 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |

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
    ##   # A tibble: 5 x 10
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         7 EARTH       Test  0.0208   8.45 0.0191   6.40 0.0375  0.999
    ## 2 healthyR         7 EARTH       Test  0.0182   5.38 0.0191   5.04 0.0212  0.999
    ## 3 healthy~         7 EARTH       Test  0.0265   1.96 0.0171   1.95 0.0560  0.997
    ## 4 healthy~         8 NNAR        Test  0.00553  2.44 0.00542  2.22 0.00849 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0271   3.52 0.0190   3.47 0.0403  0.998

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
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
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [384 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [374 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [324 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [299 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [113 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
