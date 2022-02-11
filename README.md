Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
11 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,844
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

The last day in the data set is 2022-02-09 21:22:39, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1732.74
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28844          |
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
| r\_version     |      19133 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19133 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19133 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2406 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-09 | 2021-08-25 |       444 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1522116.30 | 1872941.00 | 357 | 23811.25 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8266.41 |   15733.82 |   1 |   306.75 |   2817 |    8369 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-09 21:22:39 | 2021-08-25 06:48:38 |     16856 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     23 |        60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [387|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [377|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [327|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [302|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [116|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1781091 | 1021.099413 | 0.8042848 | 140.863805 | 1.4364695 | 0.0930557 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0577539 |  105.099218 | 0.0394281 |  13.959380 | 0.0779765 | 0.9943853 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0252753 |  639.363584 | 0.6999465 | 141.828386 | 1.2727761 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0553586 |   83.781204 | 0.0377928 |  12.864667 | 0.0825537 | 0.9937588 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0495572 |   52.372873 | 0.0338323 |  11.395545 | 0.1017128 | 0.9892979 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0095630 |   10.170741 | 0.0065286 |   8.240642 | 0.0169150 | 0.9997027 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0648794 |   77.533882 | 0.0442926 |  14.704918 | 0.0981567 | 0.9911669 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6387368 |  881.717991 | 0.4360600 |  93.518041 | 0.7412225 | 0.9909640 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2667146 | 3751.947291 | 0.8647749 | 121.856020 | 1.5405711 | 0.2439559 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5601106 | 3297.819625 | 1.0650738 | 142.382097 | 1.9066385 | 0.0039868 |
| healthyR.data |         13 | TBATS                      | Test  | 1.0322098 |  571.045932 | 0.7046806 | 135.576298 | 1.3056706 | 0.1802960 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0247237 |  634.925796 | 0.6995699 | 142.008285 | 1.2714815 | 0.0002786 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.9190963 | 2592.804992 | 1.0632967 | 168.549472 | 1.0842502 | 0.4451962 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0601207 |   88.497235 | 0.0695533 |  22.998858 | 0.0729406 | 0.9944517 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6946733 | 1171.989580 | 0.8036631 | 132.373626 | 0.9033031 | 0.1987148 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0558357 |   80.090929 | 0.0645959 |  28.381933 | 0.0691884 | 0.9944318 |
| healthyR      |          7 | EARTH                      | Test  | 0.0339359 |   32.568677 | 0.0392602 |  15.824395 | 0.0751440 | 0.9928238 |
| healthyR      |          8 | NNAR                       | Test  | 0.0137105 |    9.485388 | 0.0158616 |  10.609241 | 0.0261340 | 0.9992581 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0640368 |  131.507555 | 0.0740838 |  24.263982 | 0.0782647 | 0.9927732 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5548403 | 1889.063906 | 0.6418912 | 125.945734 | 0.6441961 | 0.9724853 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0904026 | 2415.866963 | 1.2614798 | 101.610299 | 1.3913101 | 0.5417676 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0440720 | 2523.027914 | 1.2078802 | 126.286973 | 1.3457131 | 0.1894899 |
| healthyR      |         13 | TBATS                      | Test  | 0.6163218 |  932.776727 | 0.7130187 | 117.671063 | 0.8555400 | 0.3347733 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7645047 |  172.683172 | 0.8844506 | 195.758838 | 0.9749006 | 0.0121025 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2947173 | 3194.351930 | 1.1049772 | 151.021940 | 1.5354498 | 0.6600460 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0777318 |  211.764367 | 0.0663403 |  22.294358 | 0.0966311 | 0.9934301 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.2996826 | 3192.868390 | 1.1092148 | 157.121938 | 1.5138424 | 0.2129340 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0742345 |  186.486175 | 0.0633554 |  22.332113 | 0.0893758 | 0.9935309 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0257180 |    7.549330 | 0.0219490 |  10.083579 | 0.0615447 | 0.9976150 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0434655 |   32.556787 | 0.0370956 |  16.952153 | 0.1306283 | 0.9888759 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0820698 |  216.865733 | 0.0700425 |  22.339693 | 0.1005563 | 0.9922325 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7015357 | 1905.568628 | 0.5987260 | 126.131295 | 0.8090076 | 0.9803768 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0847332 | 1092.227769 | 0.9257661 | 112.050657 | 1.3840900 | 0.5929338 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5006046 | 4461.839743 | 1.2806917 | 142.404947 | 1.8556245 | 0.2094323 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.0194159 | 2031.041081 | 0.8700210 | 145.265220 | 1.2782912 | 0.4169983 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2591948 | 2665.992889 | 1.0746604 | 151.760646 | 1.5361857 | 0.1880251 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.2000978 |  150.221043 | 1.1218068 | 172.124064 | 1.4052361 | 0.2720319 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0668837 |    8.600383 | 0.0625204 |   8.185312 | 0.0901769 | 0.9909930 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9169316 |  126.506302 | 0.8571136 | 133.951380 | 1.1093571 | 0.0091896 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0622669 |    7.540919 | 0.0582048 |   7.166341 | 0.0910731 | 0.9909723 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0206886 |    2.135665 | 0.0193390 |   2.149280 | 0.0435327 | 0.9986892 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0158632 |    1.010353 | 0.0148283 |   1.024049 | 0.0365792 | 0.9990231 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1178830 |   27.464736 | 0.1101926 |  27.938682 | 0.1279951 | 0.9904226 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6610996 |   91.575053 | 0.6179714 | 105.379711 | 0.7722369 | 0.9633585 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1758712 |  256.276316 | 1.0991607 | 109.277593 | 1.3652153 | 0.2814359 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3951744 |  251.083016 | 1.3041571 | 137.489585 | 1.6984075 | 0.0114559 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9762484 |  102.318996 | 0.9125607 | 136.200138 | 1.2072279 | 0.0171631 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8925188 |  101.328331 | 0.8342934 | 121.081311 | 1.1351119 | 0.1102967 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1241093 | 2814.845201 | 0.9242241 | 153.779955 | 1.3171355 | 0.4208865 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0676891 |  165.879882 | 0.0556528 |  24.966291 | 0.0836794 | 0.9943428 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9859543 | 1550.317462 | 0.8106354 | 159.199719 | 1.1473272 | 0.0788165 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0576403 |   79.301926 | 0.0473909 |  19.745046 | 0.0757838 | 0.9946135 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0167239 |    3.893824 | 0.0137501 |   3.662245 | 0.0295207 | 0.9993181 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0348563 |   18.641534 | 0.0286582 |  16.530290 | 0.0526982 | 0.9979521 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0781437 |  230.462525 | 0.0642485 |  28.612392 | 0.0958036 | 0.9922244 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7349893 | 2516.897573 | 0.6042961 | 137.024896 | 0.8305750 | 0.9584742 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1746958 | 1220.738467 | 0.9658155 | 132.069822 | 1.4274181 | 0.4375962 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3220813 | 1768.763705 | 1.0869934 | 138.390559 | 1.6921809 | 0.0891908 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0167880 | 1627.909813 | 0.8359863 | 167.449737 | 1.1793602 | 0.0723072 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0072739 | 1497.516515 | 0.8281640 | 157.370027 | 1.2321515 | 0.0091026 |
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
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR        Test  0.00956 10.2  0.00653  8.24 0.0169 1.00 
    ## 2 healthyR          8 NNAR        Test  0.0137   9.49 0.0159  10.6  0.0261 0.999
    ## 3 healthyR~         7 EARTH       Test  0.0257   7.55 0.0219  10.1  0.0615 0.998
    ## 4 healthyv~         8 NNAR        Test  0.0159   1.01 0.0148   1.02 0.0366 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0167   3.89 0.0138   3.66 0.0295 0.999

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [387|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [377|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [327|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [302|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [116|28]> <mdl_time_tbl>

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
