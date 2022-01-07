Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
07 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,281
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

The last day in the data set is 2022-01-05 22:55:27, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -894.28
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26281          |
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
| r\_version     |      17602 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17602 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17602 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2221 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-05 | 2021-08-04 |       409 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530390.94 | 1878777.6 | 357 | 16923 | 238636 | 3246564 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8039.65 |   15232.9 |   1 |   205 |   2817 |    8218 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-05 22:55:27 | 2021-08-04 11:12:58 |     15310 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 49M 37S |        60 |

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
    ## 1 healthyR.data <tibble [380 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [370 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [320 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [295 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [109 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1992415 | 338.004988 | 1.0493771 | 110.113736 | 1.5053983 | 0.0220200 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2072073 |  41.793762 | 0.1813134 |  43.899894 | 0.3852471 | 0.8676844 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.5123007 | 519.616717 | 1.3233146 | 120.871569 | 1.7418905 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.1296544 |  34.265156 | 0.1134520 |  30.742901 | 0.1605945 | 0.9880233 |
| healthyR.data |          7 | EARTH                      | Test  | 1.0224841 | 344.377604 | 0.8947083 | 115.204338 | 1.0657867 | 0.8988155 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0350078 |   7.409984 | 0.0306330 |   9.601376 | 0.0732867 | 0.9942313 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.1506446 |  39.287640 | 0.1318191 |  32.865855 | 0.1832491 | 0.9873671 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4331828 | 156.724000 | 0.3790497 |  84.404769 | 0.5209676 | 0.9792157 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2809247 | 442.063728 | 1.1208527 | 110.661152 | 1.4979680 | 0.4546002 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7987658 | 345.422710 | 1.5739812 | 137.233779 | 2.1010942 | 0.0877460 |
| healthyR.data |         13 | TBATS                      | Test  | 1.8917213 | 609.064335 | 1.6553204 | 128.304856 | 2.1227108 | 0.0664920 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.5076979 | 518.014801 | 1.3192869 | 120.754663 | 1.7376499 | 0.0601566 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 1.2867476 | 357.086314 | 1.3239450 | 144.801052 | 1.4158748 | 0.3179405 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0423505 |  10.501966 | 0.0435748 |   9.916989 | 0.0519156 | 0.9990135 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.5490815 | 451.073726 | 1.5938626 | 146.066803 | 1.7173019 | 0.0334969 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0482978 |  13.199922 | 0.0496940 |  12.399388 | 0.0610581 | 0.9990314 |
| healthyR      |          7 | EARTH                      | Test  | 0.0090065 |   1.380153 | 0.0092668 |   1.376889 | 0.0139858 | 0.9997338 |
| healthyR      |          8 | NNAR                       | Test  | 0.0745740 |  12.411573 | 0.0767298 |  12.317864 | 0.2289439 | 0.9497013 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0551996 |  15.612761 | 0.0567953 |  14.443076 | 0.0730186 | 0.9980765 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3747443 | 102.180528 | 0.3855774 | 104.520673 | 0.4423506 | 0.9979230 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2530134 | 311.198425 | 1.2892357 | 125.811930 | 1.4825045 | 0.3996989 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7921331 | 484.519737 | 1.8439403 | 150.764102 | 2.1358424 | 0.0865198 |
| healthyR      |         13 | TBATS                      | Test  | 1.6390380 | 475.012021 | 1.6864195 | 146.742517 | 1.8190171 | 0.0024087 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.4882893 | 434.438965 | 1.5313129 | 144.072867 | 1.6699310 | 0.1822967 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.8769867 | 252.585788 | 0.5595233 | 107.348640 | 1.0787949 | 0.5785492 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0978606 |  10.790714 | 0.0624357 |   9.694521 | 0.3092020 | 0.9497265 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.4245571 | 486.725353 | 0.9088768 | 122.458446 | 1.6288094 | 0.1644875 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0963625 |   8.291412 | 0.0614799 |   8.090272 | 0.3062680 | 0.9496019 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0686991 |   3.214794 | 0.0438305 |   3.246609 | 0.1802257 | 0.9810783 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0713863 |   8.955377 | 0.0455449 |  11.702549 | 0.1988635 | 0.9770237 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1099631 |  17.186117 | 0.0701572 |  18.461955 | 0.3036925 | 0.9499952 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4068703 |  76.317832 | 0.2595860 |  74.767958 | 0.5697051 | 0.9660541 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2244883 | 370.155032 | 0.7812316 | 117.993683 | 1.3772107 | 0.5463730 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6759049 | 627.935553 | 1.0692384 | 132.164597 | 1.9847334 | 0.1159316 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.6169991 | 567.140588 | 1.0316561 | 125.055096 | 1.8284628 | 0.1032485 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.3436788 | 438.490067 | 0.8572759 | 118.985152 | 1.6137666 | 0.2861586 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.8926796 | 766.012348 | 1.9355285 | 160.830986 | 2.0661954 | 0.0526288 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0704881 |  15.801486 | 0.0720839 |  16.541684 | 0.0867235 | 0.9969780 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.6842338 | 698.404262 | 1.7223637 | 157.453619 | 1.8608200 | 0.1160537 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0686893 |  15.493066 | 0.0702444 |  16.517619 | 0.0851085 | 0.9970258 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0164655 |   2.922821 | 0.0168382 |   2.865310 | 0.0426222 | 0.9976743 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0067628 |   1.939374 | 0.0069159 |   1.952719 | 0.0115530 | 0.9998163 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0599566 |  12.486992 | 0.0613139 |  11.033289 | 0.0747619 | 0.9972304 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4977174 | 184.188019 | 0.5089854 | 118.825020 | 0.5815382 | 0.9901410 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0399005 | 407.122505 | 1.0634431 | 121.962946 | 1.1630551 | 0.5697447 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.9116824 | 983.648521 | 1.9549615 | 161.712931 | 2.2088703 | 0.1109059 |
| healthyverse  |         13 | TBATS                      | Test  | 1.5649148 | 622.140835 | 1.6003434 | 153.739799 | 1.7970263 | 0.0119063 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.6794269 | 647.930109 | 1.7174479 | 157.636554 | 1.8715344 | 0.2294084 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.3107050 | 265.719003 | 0.9412111 | 118.094172 | 1.5611712 | 0.5401705 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1528809 |  20.299862 | 0.1097830 |  22.393268 | 0.2156196 | 0.9715695 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.0058164 | 169.156756 | 0.7222720 | 115.806968 | 1.2711304 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1581984 |  20.996068 | 0.1136015 |  22.649361 | 0.2203553 | 0.9704762 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0368983 |   3.163069 | 0.0264965 |   3.157998 | 0.0889110 | 0.9952907 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2444284 |  33.777631 | 0.1755229 |  34.651879 | 0.3632631 | 0.9324344 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1640206 |  22.314643 | 0.1177824 |  24.192449 | 0.2225481 | 0.9692954 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9094537 | 179.973157 | 0.6530744 | 114.744564 | 1.0454522 | 0.8705535 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9284323 | 184.194580 | 0.6667028 |  86.902045 | 1.1380236 | 0.6552045 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2940141 | 269.333627 | 0.9292254 | 125.591710 | 1.4712484 | 0.1845416 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0057766 | 180.411861 | 0.7222434 | 116.751212 | 1.2620346 | 0.0494821 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0693410 | 194.136107 | 0.7678887 | 113.164944 | 1.3756659 | 0.1767165 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ## 1 healthyR~         8 NNAR        Test  0.0350   7.41 0.0306   9.60 0.0733 0.994
    ## 2 healthyR          7 EARTH       Test  0.00901  1.38 0.00927  1.38 0.0140 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0687   3.21 0.0438   3.25 0.180  0.981
    ## 4 healthyv~         8 NNAR        Test  0.00676  1.94 0.00692  1.95 0.0116 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0369   3.16 0.0265   3.16 0.0889 0.995

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
    ## 1 healthyR.data <tibble [380 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [370 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [320 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [295 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [109 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
