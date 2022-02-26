Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
26 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,734
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

The last day in the data set is 2022-02-24 23:04:05, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2094.43
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29734          |
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
| r\_version     |      19749 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19749 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19749 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2491 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-24 | 2021-08-29 |       459 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1512381.63 | 1869805.51 | 357 | 18437 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8256.84 |   15823.01 |   1 |   253 |   2742 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-24 23:04:05 | 2021-08-29 08:36:23 |     17322 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     14 |        60 |

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
    ##   # A tibble: 6 x 5
    ##   package       .actual_data     .future_data .splits          .modeltime_tables
    ##   <chr>         <list>           <list>       <list>           <list>           
    ## 1 healthyR.data <tibble>         <tibble>     <split [400|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [392|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [341|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [317|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [131|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [7 x 6]> <tibble>     <split [0|7]>    <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9381713 | 223.400466 | 0.7750209 | 135.336775 | 1.1671440 | 0.5291701 |
| healthyR.data |          2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8668572 | 114.411766 | 0.7161085 | 160.979689 | 1.0140319 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0682181 |  19.767015 | 0.0563548 |  20.762534 | 0.0969849 | 0.9928030 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0873948 |  29.133585 | 0.0721966 |  22.984888 | 0.1502719 | 0.9822979 |
| healthyR.data |          8 | NNAR                       | Test  | 0.5999995 |  94.627521 | 0.4956581 |  73.529318 | 0.8254246 | 0.7884498 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0707222 |  18.670089 | 0.0584234 |  17.353440 | 0.1061967 | 0.9916934 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4473530 |  98.101046 | 0.3695572 |  82.976455 | 0.5570947 | 0.9926685 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9842626 | 367.972583 | 0.8130968 | 100.521701 | 1.2258306 | 0.4791795 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2431246 | 410.285355 | 1.0269420 | 136.958172 | 1.4453912 | 0.0453491 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8259561 | 129.586617 | 0.6823202 | 150.931883 | 0.9903655 | 0.0882419 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8657881 | 114.895066 | 0.7152253 | 159.743340 | 1.0132201 | 0.0481540 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8298253 | 179.765714 | 0.9033549 | 152.146647 | 0.9727936 | 0.4211884 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0606805 |   9.264542 | 0.0660573 |   8.685890 | 0.0720900 | 0.9952295 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6529305 | 116.788375 | 0.7107858 | 113.071973 | 0.8743927 | 0.1382269 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0582279 |   9.502984 | 0.0633874 |   8.932720 | 0.0695816 | 0.9952451 |
| healthyR      |          7 | EARTH                      | Test  | 0.0344685 |   4.517175 | 0.0375227 |   4.512707 | 0.0750967 | 0.9937031 |
| healthyR      |          8 | NNAR                       | Test  | 1.1067271 | 231.576045 | 1.2047926 |  99.985392 | 1.3239973 | 0.8374663 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0635348 |  12.808352 | 0.0691645 |  15.273899 | 0.0762976 | 0.9945543 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4516743 | 104.665415 | 0.4916965 |  97.598144 | 0.5506596 | 0.9755293 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8359762 | 263.713853 | 0.9100508 |  79.916973 | 1.1321794 | 0.4972540 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0026488 | 306.171208 | 1.0914920 | 119.064475 | 1.3147414 | 0.0776451 |
| healthyR      |         13 | TBATS                      | Test  | 0.7119752 | 112.402358 | 0.7750623 | 138.827202 | 0.8839603 | 0.1701887 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7506595 | 116.387544 | 0.8171743 | 135.818056 | 0.9456077 | 0.0745716 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1360863 | 348.512131 | 1.2495493 | 161.696076 | 1.3441394 | 0.6310164 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0751187 |  15.273801 | 0.0826209 |  13.922457 | 0.0916011 | 0.9912097 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7164437 | 123.532641 | 0.7879962 | 135.818428 | 0.9375830 | 0.1284520 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0692445 |  18.407979 | 0.0761601 |  15.185109 | 0.0850933 | 0.9914718 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0146620 |   2.789072 | 0.0161264 |   2.730034 | 0.0207797 | 0.9995106 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0111891 |   2.596092 | 0.0123065 |   2.713697 | 0.0227192 | 0.9995369 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0792155 |  16.459727 | 0.0871269 |  17.111790 | 0.0948961 | 0.9897413 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6506451 | 225.994004 | 0.7156262 | 129.636698 | 0.7489447 | 0.9779156 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9123393 | 248.901308 | 1.0034562 |  87.959930 | 1.3274248 | 0.5031003 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0242257 | 259.634899 | 1.1265170 | 133.699839 | 1.4151112 | 0.1277520 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.8626746 | 166.853017 | 0.9488315 | 152.892297 | 1.0844809 | 0.0967441 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.7808244 | 111.827439 | 0.8588067 | 170.054479 | 0.9964874 | 0.0190166 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.9844080 | 147.703508 | 0.9884164 | 167.237337 | 1.1789409 | 0.7952353 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0684678 |  17.595225 | 0.0687465 |  12.389882 | 0.0907320 | 0.9924464 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8463358 | 169.493777 | 0.8497819 | 122.421226 | 1.0401815 | 0.0488325 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0745062 |  21.168985 | 0.0748096 |  14.389226 | 0.0971455 | 0.9924156 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0246734 |   2.649183 | 0.0247739 |   2.671800 | 0.0471585 | 0.9983311 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0231007 |   1.989062 | 0.0231947 |   1.958065 | 0.0501827 | 0.9979401 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0715820 |  14.545683 | 0.0718735 |  12.047211 | 0.0939210 | 0.9918894 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5194247 |  85.656340 | 0.5215397 |  94.049518 | 0.6519436 | 0.9755372 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7878264 | 213.168374 | 0.7910342 |  96.060423 | 0.9209362 | 0.5783977 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0855218 | 261.185396 | 1.0899419 | 137.673520 | 1.3188373 | 0.0367792 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9015711 | 109.357621 | 0.9052422 | 142.785419 | 1.1627143 | 0.0412342 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8689786 | 161.334337 | 0.8725169 | 116.319527 | 1.0810928 | 0.5127950 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7916460 | 189.761609 | 0.8527857 | 149.180334 | 0.9728903 | 0.4186030 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0718011 |  17.367989 | 0.0773463 |  19.300120 | 0.0831468 | 0.9935037 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.6351803 | 138.730694 | 0.6842360 | 114.395499 | 0.8680442 | 0.1087001 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0688786 |  16.589555 | 0.0741982 |  18.183894 | 0.0901023 | 0.9930873 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0170932 |   2.630979 | 0.0184134 |   2.592810 | 0.0267001 | 0.9991918 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0304094 |   9.715969 | 0.0327579 |  12.033212 | 0.0537115 | 0.9972429 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0723773 |  19.031045 | 0.0779671 |  22.686804 | 0.0926092 | 0.9914861 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7934345 | 309.805316 | 0.8547123 | 148.448354 | 0.8761001 | 0.9420973 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8543656 | 268.881093 | 0.9203492 |  95.069165 | 1.1130585 | 0.5801762 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0051365 | 280.727758 | 1.0827643 | 129.316845 | 1.2655707 | 0.1262400 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.6605772 | 131.338478 | 0.7115943 | 136.143294 | 0.8708180 | 0.1023092 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.7232587 | 101.402289 | 0.7791168 | 174.606866 | 0.9258572 | 0.1220102 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          1 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          4 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          6 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          7 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          8 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          9 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         10 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         12 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         13 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         14 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ## 1 health~         6 LM          Test   0.0682 19.8   0.0564 20.8   0.0970  0.993
    ## 2 health~         6 LM          Test   0.0582  9.50  0.0634  8.93  0.0696  0.995
    ## 3 health~         7 EARTH       Test   0.0147  2.79  0.0161  2.73  0.0208  1.00 
    ## 4 health~         7 EARTH       Test   0.0247  2.65  0.0248  2.67  0.0472  0.998
    ## 5 health~         7 EARTH       Test   0.0171  2.63  0.0184  2.59  0.0267  0.999
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
    ##   package       .actual_data     .future_data .splits          .modeltime_tables
    ##   <chr>         <list>           <list>       <list>           <list>           
    ## 1 healthyR.data <tibble>         <tibble>     <split [400|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [392|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [341|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [317|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [131|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [7 x 6]> <tibble>     <split [0|7]>    <mdl_time_tbl>

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
