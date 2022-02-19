Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,318
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

The last day in the data set is 2022-02-17 21:50:33, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1925.2
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29318          |
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
| r\_version     |      19501 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19501 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19501 |           0.33 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2430 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-17 | 2021-08-27 |       452 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1515866.97 | 1870824.40 | 357 | 20493 | 271097 | 3247923 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8281.92 |   15787.27 |   1 |   307 |   2770 |    8310 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-17 21:50:33 | 2021-08-27 11:03:23 |     17101 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   56.5 |        60 |

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [394|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [385|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [334|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [310|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [124|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1375727 | 279.4134262 | 0.9119302 | 148.1957725 | 1.3882631 | 0.0022905 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0628988 |  29.9939199 | 0.0504226 |  18.1695615 | 0.0831243 | 0.9937602 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8999418 | 113.3089877 | 0.7214345 | 188.1593962 | 1.0376018 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0640312 |  27.5911121 | 0.0513303 |  18.8623969 | 0.0943721 | 0.9932146 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0776629 |  32.7727917 | 0.0622582 |  21.5977941 | 0.1486731 | 0.9800789 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0216649 |   8.1325039 | 0.0173675 |  10.2960600 | 0.0350281 | 0.9987649 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0715885 |  26.3248528 | 0.0573886 |  20.6076198 | 0.1050065 | 0.9916782 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4975639 | 174.1950369 | 0.3988700 |  80.6562953 | 0.6184820 | 0.9914465 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0357862 | 642.1203509 | 0.8303336 |  94.0990139 | 1.2919890 | 0.4049404 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4175591 | 788.9609230 | 1.1363802 | 140.0045875 | 1.6322402 | 0.0058176 |
| healthyR.data |         13 | TBATS                      | Test  | 1.0180584 | 162.6683535 | 0.8161221 | 154.6102193 | 1.2472401 | 0.0006388 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8995114 | 114.5057849 | 0.7210894 | 187.2540694 | 1.0364943 | 0.0081140 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8569503 | 343.6763530 | 0.9678114 | 161.9014438 | 1.0218986 | 0.4056192 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0625409 |  22.3243626 | 0.0706317 |  17.4249040 | 0.0742941 | 0.9948534 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6002674 | 137.8989393 | 0.6779222 | 113.7966572 | 0.8394589 | 0.2179981 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0574386 |  19.2276546 | 0.0648692 |  17.3077203 | 0.0700614 | 0.9948673 |
| healthyR      |          7 | EARTH                      | Test  | 0.0336171 |   6.9404658 | 0.0379661 |   7.7336140 | 0.0756221 | 0.9934586 |
| healthyR      |          8 | NNAR                       | Test  | 0.0083459 |   1.5099785 | 0.0094255 |   1.5492535 | 0.0181399 | 0.9997539 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0618397 |  28.0201525 | 0.0698397 |  22.4050388 | 0.0757704 | 0.9939049 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4934362 | 256.0485648 | 0.5572706 | 113.3159632 | 0.5910298 | 0.9714549 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9715013 | 359.8083928 | 1.0971816 | 104.9895468 | 1.2544109 | 0.5371568 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0759422 | 419.5604852 | 1.2151337 | 128.9243142 | 1.3409877 | 0.1150933 |
| healthyR      |         13 | TBATS                      | Test  | 0.6139686 | 157.0063603 | 0.6933959 | 119.8292978 | 0.8225826 | 0.3465542 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7454671 | 160.6695051 | 0.8419060 | 151.0822978 | 0.9566981 | 0.0669745 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2548388 | 623.8565970 | 1.4933953 | 168.1025159 | 1.4562038 | 0.1319237 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0803293 |  46.4098753 | 0.0956006 |  20.6029105 | 0.0975269 | 0.9894212 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.9268495 | 430.4314320 | 1.1030522 | 157.3163259 | 1.1645673 | 0.2751580 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0740812 |  42.8174133 | 0.0881647 |  22.1674476 | 0.0884290 | 0.9898375 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0148289 |   3.4666976 | 0.0176480 |   3.3466018 | 0.0212775 | 0.9994645 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0156258 |   6.2192615 | 0.0185964 |   5.9123850 | 0.0334496 | 0.9992226 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0854079 |  46.5342723 | 0.1016447 |  25.5251335 | 0.1017465 | 0.9876068 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7154845 | 439.2711216 | 0.8515047 | 143.8330707 | 0.8014281 | 0.9723720 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9995959 | 287.2459610 | 1.1896283 |  86.0217150 | 1.4103479 | 0.5985531 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1183596 | 583.3509935 | 1.3309701 | 122.4969674 | 1.5699804 | 0.2721421 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9621158 | 371.3439063 | 1.1450229 | 166.0072033 | 1.1654746 | 0.0802431 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9983657 | 378.8619972 | 1.1881642 | 164.0595491 | 1.2501568 | 0.0306044 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.9709235 | 105.4567694 | 0.9850986 | 165.6693543 | 1.1735095 | 0.2486620 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0602239 |   6.8282929 | 0.0611031 |   6.6046723 | 0.0864121 | 0.9909586 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7329096 | 151.3734287 | 0.7436098 |  92.4608843 | 0.8754009 | 0.0444981 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0592983 |   6.7446889 | 0.0601641 |   6.6046763 | 0.0884915 | 0.9909493 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0206963 |   1.8919848 | 0.0209984 |   1.8970142 | 0.0431736 | 0.9986210 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0140867 |   0.9800955 | 0.0142924 |   0.9838314 | 0.0339084 | 0.9991945 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0640698 |   8.9314209 | 0.0650052 |   8.3564442 | 0.0879837 | 0.9907126 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5746080 |  73.7447536 | 0.5829971 |  95.4162759 | 0.6962650 | 0.9648657 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0312340 | 240.1550486 | 1.0462897 | 100.9351047 | 1.2020804 | 0.3734284 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2023283 | 267.5061830 | 1.2198819 | 130.9457591 | 1.4416670 | 0.0230623 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9775494 | 138.3022032 | 0.9918213 | 144.3432110 | 1.1557856 | 0.0006133 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7410532 | 134.2570222 | 0.7518724 |  91.4797068 | 0.9239977 | 0.0000243 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8455588 | 364.0835579 | 0.9017093 | 163.0161269 | 1.0290581 | 0.2978702 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0783295 |  42.0063735 | 0.0835311 |  28.5754769 | 0.0906047 | 0.9923580 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.7555541 | 262.7672507 | 0.8057277 | 154.9297360 | 0.9298159 | 0.1878153 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0686947 |  27.7542459 | 0.0732565 |  23.9221403 | 0.0850733 | 0.9922813 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0162660 |   2.3482733 | 0.0173461 |   2.3214468 | 0.0294882 | 0.9989858 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0232470 |   6.7038042 | 0.0247908 |   6.3126193 | 0.0482159 | 0.9975827 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0800551 |  55.2058507 | 0.0853712 |  28.9775304 | 0.0977047 | 0.9905455 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.0148139 | 825.0304244 | 1.0822039 | 167.3069028 | 1.0872184 | 0.9181420 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0242311 | 380.1051351 | 1.0922465 | 122.4778370 | 1.3169939 | 0.5279901 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0509539 | 421.1102592 | 1.1207439 | 130.0692661 | 1.4160054 | 0.1732630 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8381966 | 362.9633222 | 0.8938581 | 165.4759812 | 0.9947092 | 0.1329609 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.8751901 | 297.9578316 | 0.9333083 | 162.5570422 | 1.0921516 | 0.0885918 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |

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
    ##   package  .model_id .model_desc .type     mae  mape    mase  smape   rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl>  <dbl>  <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.0217  8.13  0.0174  10.3   0.0350 0.999
    ## 2 healthyR         8 NNAR        Test  0.00835 1.51  0.00943  1.55  0.0181 1.00 
    ## 3 healthy~         7 EARTH       Test  0.0148  3.47  0.0176   3.35  0.0213 0.999
    ## 4 healthy~         8 NNAR        Test  0.0141  0.980 0.0143   0.984 0.0339 0.999
    ## 5 healthy~         7 EARTH       Test  0.0163  2.35  0.0173   2.32  0.0295 0.999

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [394|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [385|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [334|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [310|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [124|28]> <mdl_time_tbl>

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
