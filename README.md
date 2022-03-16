Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
16 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,624
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

The last day in the data set is 2022-03-14 23:07:39, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2526.49
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31624         |
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
| r_version     |     20968 |          0.34 |   5 |   5 |     0 |       30 |          0 |
| r_arch        |     20968 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20968 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2624 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-14 | 2021-09-10 |      477 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1496689.30 | 1860204.1 | 357 | 16918 | 271482 | 3246657 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8319.08 |   16062.8 |   1 |   338 |   2659 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-14 23:07:39 | 2021-09-10 14:43:17 |    18434 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     39 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [418|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [410|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [359|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [335|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [149|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [25 x 6]> <tibble>     <split [0|25]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 1.1369857 | 259.553623 | 0.9188780 | 152.890446 | 1.3459068 | 0.0517916 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.1317696 |  44.803117 | 0.1064923 |  24.711076 | 0.1479009 | 0.9945394 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7884072 | 111.494154 | 0.6371673 | 160.090810 | 0.9329841 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0461142 |  10.161973 | 0.0372681 |   9.758305 | 0.0560228 | 0.9967199 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2078581 |  95.469209 | 0.1679847 |  38.458967 | 0.3159125 | 0.9230347 |
| healthyR.data |         8 | NNAR                       | Test  | 0.1552683 |  36.148418 | 0.1254832 |  29.626970 | 0.1966559 | 0.9836657 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0520570 |  10.442113 | 0.0420709 |  11.508068 | 0.0621692 | 0.9957071 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3517501 |  77.882382 | 0.2842739 |  67.414026 | 0.4242692 | 0.9936985 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8470027 | 346.163322 | 0.6845223 | 103.986163 | 1.0191281 | 0.3597457 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2414668 | 425.551761 | 1.0033165 | 146.980638 | 1.4913029 | 0.0028474 |
| healthyR.data |        13 | TBATS                      | Test  | 0.8405024 | 125.463716 | 0.6792690 | 174.211816 | 0.9604489 | 0.0220747 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7859643 | 112.600102 | 0.6351929 | 157.731422 | 0.9304636 | 0.0087273 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.7822450 | 259.941934 | 0.7650578 | 155.404336 | 0.9644817 | 0.2058837 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0461231 |  14.677953 | 0.0451097 |  17.564109 | 0.0660734 | 0.9943631 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.7051838 | 305.769963 | 0.6896898 | 144.509391 | 0.8687245 | 0.0938695 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0427515 |  10.689556 | 0.0418122 |  10.982192 | 0.0643601 | 0.9943779 |
| healthyR      |         7 | EARTH                      | Test  | 0.0118031 |   2.598203 | 0.0115438 |   2.509055 | 0.0238650 | 0.9992978 |
| healthyR      |         8 | NNAR                       | Test  | 0.0433766 |  24.665666 | 0.0424235 |  22.244261 | 0.0669135 | 0.9945686 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0504561 |  15.717533 | 0.0493475 |  16.746469 | 0.0721673 | 0.9929340 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3678736 | 154.929225 | 0.3597908 | 106.586129 | 0.4736893 | 0.9811811 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.1683274 | 849.108112 | 1.1426573 | 141.728364 | 1.3703864 | 0.3917093 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2515786 | 855.906318 | 1.2240793 | 140.185160 | 1.5250815 | 0.0620374 |
| healthyR      |        13 | TBATS                      | Test  | 0.6935903 | 267.402946 | 0.6783510 | 152.745544 | 0.8516403 | 0.0812055 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6251949 | 123.947838 | 0.6114583 | 143.922224 | 0.8733370 | 0.0000035 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7010519 | 268.430405 | 0.7457986 | 145.806156 | 0.8645494 | 0.2176676 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0503278 |  24.715839 | 0.0535401 |  18.026851 | 0.0611400 | 0.9968353 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7248301 | 377.853952 | 0.7710945 | 129.051206 | 0.9118444 | 0.0002180 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0495408 |  25.491529 | 0.0527029 |  18.118716 | 0.0606405 | 0.9969378 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0155813 |   3.605385 | 0.0165758 |   3.513303 | 0.0271921 | 0.9989530 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.0521053 |  26.213329 | 0.0554311 |  25.219138 | 0.0654034 | 0.9946922 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0530426 |  13.055310 | 0.0564282 |  12.843782 | 0.0662369 | 0.9953479 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3899922 | 195.621243 | 0.4148845 | 107.961104 | 0.4451598 | 0.9984835 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0662623 | 852.242272 | 1.1343195 | 119.162094 | 1.3775106 | 0.1427714 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3987633 | 967.307149 | 1.4880433 | 143.541353 | 1.7018154 | 0.0057214 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7073717 | 331.701376 | 0.7525217 | 152.415469 | 0.8911075 | 0.0000150 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6487455 | 406.229621 | 0.6901535 | 112.815100 | 0.8458437 | 0.0484633 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5797450 | 117.788739 | 0.5434542 | 135.032616 | 0.7845330 | 0.2606001 |
| healthyverse  |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.9060635 | 370.052375 | 0.8493458 | 122.455844 | 1.1474870 | 0.0335467 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0631786 |  23.463318 | 0.0592237 |  18.962739 | 0.0802191 | 0.9941401 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0184201 |   2.806710 | 0.0172670 |   2.839362 | 0.0334273 | 0.9986091 |
| healthyverse  |         8 | NNAR                       | Test  | 0.0760554 |  14.987714 | 0.0712945 |  17.238527 | 0.1204163 | 0.9920640 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0696076 |  20.421897 | 0.0652503 |  20.175073 | 0.0893951 | 0.9924653 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2618100 |  48.063294 | 0.2454213 |  60.740009 | 0.3560439 | 0.9890236 |
| healthyverse  |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1076975 | 297.962655 | 1.0383580 | 142.221558 | 1.4259900 | 0.0035179 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6591272 | 142.493529 | 0.6178673 | 118.266625 | 0.9002821 | 0.0173417 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9872732 | 445.244575 | 0.9254720 | 127.217508 | 1.1967978 | 0.0398440 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6987341 | 297.501034 | 0.7572149 | 142.262092 | 0.8759596 | 0.3894774 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.6507670 | 287.889584 | 0.7052332 | 147.945778 | 0.8202899 | 0.0064604 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0499123 |  35.046825 | 0.0540897 |  20.541356 | 0.0643607 | 0.9973773 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0113085 |   2.437806 | 0.0122550 |   2.408548 | 0.0140238 | 0.9996770 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0339566 |  22.386792 | 0.0367986 |  21.429342 | 0.0399936 | 0.9973771 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0562474 |  30.442061 | 0.0609551 |  21.927894 | 0.0683578 | 0.9957956 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4299860 | 281.967439 | 0.4659738 | 113.340116 | 0.4818586 | 0.9897386 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1217006 | 942.162938 | 1.2155818 | 139.638099 | 1.3935952 | 0.0305006 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6379596 | 297.792417 | 0.6913538 | 143.804351 | 0.8118158 | 0.0000039 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.5869490 | 175.714086 | 0.6360740 | 158.070965 | 0.7375364 | 0.0368239 |
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
    ## 1 health~         6 LM          Test   0.0461 10.2   0.0373  9.76  0.0560  0.997
    ## 2 health~         7 EARTH       Test   0.0118  2.60  0.0115  2.51  0.0239  0.999
    ## 3 health~         7 EARTH       Test   0.0156  3.61  0.0166  3.51  0.0272  0.999
    ## 4 health~         7 EARTH       Test   0.0184  2.81  0.0173  2.84  0.0334  0.999
    ## 5 health~         7 EARTH       Test   0.0113  2.44  0.0123  2.41  0.0140  1.00 
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [418|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [410|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [359|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [335|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [149|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [25 x 6]> <tibble>     <split [0|25]>   <mdl_time_tbl>

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
