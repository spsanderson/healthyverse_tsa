Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
30 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,902
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

The last day in the data set is 2022-01-28 18:55:17, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1442.28
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27902          |
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
| r\_version     |      18549 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18549 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18549 |           0.34 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2350 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-28 | 2021-08-18 |       432 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1529622.51 | 1876340.55 | 357 | 22847.5 | 261816 | 3247929 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8210.03 |   15699.94 |   1 |   231.0 |   2782 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-28 18:55:17 | 2021-08-18 19:02:51 |     16345 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     13 |        60 |

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
    ## 1 healthyR.data <tibble [403 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [393 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [343 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [318 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [132 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9002212 |  576.4112888 | 0.6386811 | 121.6686186 | 1.1252496 | 0.1799790 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0953982 |   20.6310879 | 0.0676823 |  26.1517879 | 0.1572211 | 0.9718023 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8220073 |  235.0607140 | 0.5831906 | 118.8319885 | 1.0619002 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0613568 |   20.3415789 | 0.0435309 |  17.0533269 | 0.0743521 | 0.9942420 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0233692 |   11.2001640 | 0.0165798 |   6.9703201 | 0.0283218 | 0.9994102 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0193512 |   13.7500389 | 0.0137291 |  10.5780755 | 0.0534397 | 0.9966566 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0607773 |   18.4593874 | 0.0431198 |  17.7209593 | 0.0841554 | 0.9924466 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5172509 |  235.1616097 | 0.3669747 |  91.9869719 | 0.6180305 | 0.9971159 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1424655 |  689.3527168 | 0.8105465 | 110.6196713 | 1.4401234 | 0.3245124 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3316947 |  623.5426667 | 0.9447992 | 128.4041340 | 1.6784313 | 0.0264215 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8689058 |  402.9797624 | 0.6164637 | 117.5938973 | 1.1248441 | 0.0001619 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8216546 |  234.8338524 | 0.5829404 | 118.8905440 | 1.0611235 | 0.0590544 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7166664 |  545.6602621 | 0.8591482 | 154.9357305 | 0.8374314 | 0.6040306 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0383057 |   23.8789042 | 0.0459213 |  18.4900861 | 0.0450007 | 0.9989302 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.4947358 |  213.2192190 | 0.5930952 | 128.1798112 | 0.6194430 | 0.1774875 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0360510 |   23.9522563 | 0.0432184 |  21.6780440 | 0.0415577 | 0.9990799 |
| healthyR      |          7 | EARTH                      | Test  | 0.0174243 |    9.5915639 | 0.0208885 |   8.1355899 | 0.0196257 | 0.9995415 |
| healthyR      |          8 | NNAR                       | Test  | 0.0038168 |    3.5730838 | 0.0045756 |   3.2813215 | 0.0051566 | 0.9999480 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0402601 |   26.5527576 | 0.0482643 |  19.4485153 | 0.0499066 | 0.9981415 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4515477 |  415.3981195 | 0.5413208 | 128.1688562 | 0.4991059 | 0.9961734 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1975250 | 1258.2784695 | 1.4356072 | 127.8230715 | 1.4126174 | 0.6160264 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0203104 | 1274.3988678 | 1.2231602 | 122.2396946 | 1.2214232 | 0.3820307 |
| healthyR      |         13 | TBATS                      | Test  | 0.4896037 |  234.1817700 | 0.5869427 | 123.5703165 | 0.6039021 | 0.1822391 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.5674882 |   91.3900445 | 0.6803116 | 177.1218653 | 0.7022933 | 0.0109676 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7246182 |  295.3420282 | 0.5889968 | 133.8576738 | 0.8855814 | 0.1905518 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0694599 |  108.4265171 | 0.0564596 |  28.7775628 | 0.0808873 | 0.9946666 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7086955 |  306.9609348 | 0.5760542 | 156.0304612 | 0.9345139 | 0.1647185 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0701358 |   93.4741043 | 0.0570090 |  28.8304852 | 0.0818244 | 0.9950812 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0294171 |    3.8169831 | 0.0239113 |   3.7832163 | 0.0658254 | 0.9967226 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0336773 |   22.2749822 | 0.0273742 |  18.7024849 | 0.0660818 | 0.9956571 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0747709 |  113.2862352 | 0.0607766 |  29.7300816 | 0.0930808 | 0.9936412 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4279123 |  491.9414813 | 0.3478231 | 100.5136687 | 0.4958446 | 0.9801958 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2592550 |  980.7473232 | 1.0235695 | 137.7367596 | 1.5003197 | 0.4970356 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3234044 | 1182.1606341 | 1.0757125 | 137.0201620 | 1.5842697 | 0.1460423 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.7064176 |  361.6694901 | 0.5742026 | 140.1477799 | 0.8749505 | 0.2074714 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.7559952 |  246.1322299 | 0.6145011 | 152.1517750 | 0.9871313 | 0.0608329 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8284265 |  265.6213477 | 0.7796187 | 166.5544811 | 0.9618795 | 0.4493183 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0570637 |   20.4153260 | 0.0537017 |  13.7800689 | 0.0659202 | 0.9970375 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.6172854 |  279.9083143 | 0.5809172 | 128.7863331 | 0.7059770 | 0.0830843 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0504715 |   13.6471668 | 0.0474979 |  11.4880602 | 0.0579101 | 0.9972266 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0118076 |    3.7791532 | 0.0111120 |   3.4570993 | 0.0170269 | 0.9996663 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0026580 |    0.6056712 | 0.0025014 |   0.6046258 | 0.0037982 | 0.9999744 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0881349 |   27.8956374 | 0.0829423 |  30.3134462 | 0.0976722 | 0.9961843 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5120537 |  209.9639940 | 0.4818853 | 120.8003455 | 0.5700561 | 0.9907648 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0417383 |  670.2818595 | 0.9803629 | 118.5664038 | 1.1902565 | 0.4014990 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2835865 |  952.9503529 | 1.2079624 | 134.9474967 | 1.4897942 | 0.0787957 |
| healthyverse  |         13 | TBATS                      | Test  | 0.6127659 |  174.0328995 | 0.5766640 | 132.3490475 | 0.7411064 | 0.1286293 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.6131755 |  164.6046491 | 0.5770495 | 120.8746677 | 0.7532608 | 0.0006151 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9695805 |  499.1337482 | 0.6481687 | 128.8362827 | 1.1755767 | 0.4861359 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0518339 |   30.4367142 | 0.0346512 |  17.4463039 | 0.0638435 | 0.9978894 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9074637 |  333.9580433 | 0.6066433 | 154.5591050 | 1.0662721 | 0.0241301 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0488099 |   24.5858881 | 0.0326296 |  15.7132268 | 0.0589170 | 0.9980583 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0190640 |    4.7151756 | 0.0127443 |   4.1077886 | 0.0340719 | 0.9991596 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0917575 |   20.2157452 | 0.0613403 |  23.4999103 | 0.1206709 | 0.9859758 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0648475 |   54.2350345 | 0.0433508 |  27.0802854 | 0.0807074 | 0.9965647 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6459410 |  406.0573029 | 0.4318143 | 118.0717830 | 0.7511775 | 0.9839483 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1747421 |  524.4963105 | 0.7853201 | 122.9204498 | 1.3974850 | 0.4040441 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3946294 |  619.9336919 | 0.9323157 | 144.2239421 | 1.6123958 | 0.0540138 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9183349 |  396.8339553 | 0.6139108 | 145.5367386 | 1.0773901 | 0.0342455 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9407568 |  357.8918220 | 0.6288999 | 131.9112275 | 1.1661121 | 0.0166024 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |

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
    ##   package .model_id .model_desc .type     mae   mape    mase smape    rmse   rsq
    ##   <chr>       <int> <chr>       <chr>   <dbl>  <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 health~         7 EARTH       Test  0.0234  11.2   0.0166  6.97  0.0283  0.999
    ## 2 health~         8 NNAR        Test  0.00382  3.57  0.00458 3.28  0.00516 1.00 
    ## 3 health~         7 EARTH       Test  0.0294   3.82  0.0239  3.78  0.0658  0.997
    ## 4 health~         8 NNAR        Test  0.00266  0.606 0.00250 0.605 0.00380 1.00 
    ## 5 health~         7 EARTH       Test  0.0191   4.72  0.0127  4.11  0.0341  0.999

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
    ## 1 healthyR.data <tibble [403 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [393 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [343 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [318 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [132 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
