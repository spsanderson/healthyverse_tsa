Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
20 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,038
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

The last day in the data set is 2022-01-18 20:33:11, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1203.91
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27038          |
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
| r\_version     |      18072 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18072 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18072 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2274 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-18 | 2021-08-09 |       422 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1534318.75 | 1878545.06 | 357 | 16923 | 238827 | 3247715 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8049.24 |   15263.48 |   1 |   221 |   2802 |    8250 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-18 20:33:11 | 2021-08-09 23:24:37 |     15783 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     48 |        60 |

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
    ## 1 healthyR.data <tibble [393 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [383 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [333 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [308 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [122 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8373344 |  407.674886 | 0.5959134 | 112.113594 | 1.0259889 | 0.4052303 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1453804 |   28.508368 | 0.1034642 |  28.770419 | 0.3196241 | 0.9198161 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9584198 |  402.883328 | 0.6820873 | 109.407224 | 1.2503839 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0659534 |   23.574075 | 0.0469377 |  17.363455 | 0.0827685 | 0.9943291 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0285804 |   11.518403 | 0.0203400 |   7.282278 | 0.0445040 | 0.9980387 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0335578 |   13.727094 | 0.0238824 |   9.345398 | 0.0425505 | 0.9985349 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0711967 |   23.231243 | 0.0506692 |  18.046860 | 0.0999797 | 0.9918393 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5266393 |  265.044688 | 0.3747982 |  91.568449 | 0.6277670 | 0.9939501 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0593958 |  615.903353 | 0.7539499 | 104.382333 | 1.3067840 | 0.5461369 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3680587 |  400.504423 | 0.9736187 | 137.640243 | 1.5595413 | 0.1645865 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9070633 |  331.209143 | 0.6455380 | 107.450445 | 1.1923089 | 0.1342322 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9577408 |  402.467791 | 0.6816041 | 109.383689 | 1.2496631 | 0.0196826 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8618580 |  521.732222 | 1.0014824 | 158.950058 | 0.9452957 | 0.7005108 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0327211 |   14.152328 | 0.0380220 |  15.408470 | 0.0411655 | 0.9986695 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8019534 |  435.681792 | 0.9318731 | 152.960603 | 0.9487609 | 0.1276003 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0333220 |   19.647626 | 0.0387203 |  22.035076 | 0.0422991 | 0.9988546 |
| healthyR      |          7 | EARTH                      | Test  | 0.0170029 |    8.289229 | 0.0197574 |   7.158079 | 0.0189823 | 0.9995992 |
| healthyR      |          8 | NNAR                       | Test  | 0.0167791 |    6.587397 | 0.0194974 |   5.584342 | 0.0356441 | 0.9974256 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0348191 |   17.262736 | 0.0404599 |  16.473465 | 0.0501820 | 0.9979772 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5008299 |  386.235332 | 0.5819664 | 136.949446 | 0.5471072 | 0.9943104 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1868399 | 1138.201110 | 1.3791128 | 130.363777 | 1.4307119 | 0.6503699 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0828057 |  793.147296 | 1.2582246 | 130.381822 | 1.4123474 | 0.4286962 |
| healthyR      |         13 | TBATS                      | Test  | 0.7904741 |  394.595623 | 0.9185341 | 153.234384 | 0.9327197 | 0.2041897 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8412840 |  535.752366 | 0.9775754 | 150.496382 | 0.9976174 | 0.0029164 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7545237 |  277.568311 | 0.5034974 | 117.825794 | 0.9110796 | 0.3252030 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0670478 |   69.345501 | 0.0447413 |  21.510860 | 0.0767514 | 0.9960646 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.8135406 |  453.198911 | 0.5428797 | 112.618188 | 1.0054829 | 0.2096636 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0785840 |   65.371196 | 0.0524395 |  21.864351 | 0.0964875 | 0.9964089 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0321387 |    2.748544 | 0.0214463 |   2.700876 | 0.0691790 | 0.9966831 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0463581 |   10.595421 | 0.0309350 |   8.605911 | 0.0960852 | 0.9917598 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0968962 |   83.911430 | 0.0646593 |  26.543535 | 0.1234137 | 0.9955799 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4064049 |  348.774467 | 0.2711960 |  82.787169 | 0.5084886 | 0.9909656 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1623600 |  572.521784 | 0.7756486 | 119.502149 | 1.4295811 | 0.5461627 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2472608 |  753.951689 | 0.8323034 | 141.052221 | 1.5106347 | 0.1623549 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6757603 |  230.478720 | 0.4509383 | 114.510703 | 0.8273098 | 0.4343769 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8951656 |  563.381555 | 0.5973485 | 114.152838 | 1.1094056 | 0.0093563 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0264008 |  430.163142 | 0.9089340 | 168.479775 | 1.1838437 | 0.6059959 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0641281 |   30.858816 | 0.0567889 |  19.956645 | 0.0776084 | 0.9961121 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7662698 |  155.077385 | 0.6785738 | 150.825726 | 0.9548628 | 0.2501195 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0656271 |   26.833774 | 0.0581164 |  18.485129 | 0.0775476 | 0.9966237 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0102004 |    3.326691 | 0.0090330 |   3.112889 | 0.0149246 | 0.9997540 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0057551 |    1.806928 | 0.0050964 |   1.759941 | 0.0072643 | 0.9999576 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0658912 |   26.601715 | 0.0583503 |  23.459877 | 0.0770229 | 0.9960393 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5759046 |  286.582266 | 0.5099950 | 132.947488 | 0.6459686 | 0.9869304 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2293794 |  989.976757 | 1.0886827 | 128.901202 | 1.3981675 | 0.5203817 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1537044 |  927.381004 | 1.0216683 | 125.892420 | 1.4512282 | 0.2033967 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5661869 |  181.327172 | 0.5013894 | 123.548333 | 0.6986020 | 0.4506537 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8573701 |  235.807689 | 0.7592481 | 168.406016 | 1.0589435 | 0.0029741 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8669685 |  112.891147 | 0.4921114 | 108.812107 | 1.0861733 | 0.5615024 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1096599 |   16.115808 | 0.0622455 |  18.374674 | 0.1260251 | 0.9986148 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9653217 |  107.465013 | 0.5479390 | 128.971296 | 1.1296486 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1136343 |   16.645358 | 0.0645014 |  18.822217 | 0.1294757 | 0.9986608 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0314126 |    2.610445 | 0.0178305 |   2.594514 | 0.0471858 | 0.9984454 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3270391 |   36.930892 | 0.1856350 |  44.373262 | 0.3934029 | 0.9722695 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1459374 |   21.058203 | 0.0828375 |  22.027328 | 0.1640339 | 0.9976298 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8366748 |  122.930849 | 0.4749160 | 108.964131 | 1.0110046 | 0.9131413 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9010985 |  112.604673 | 0.5114844 |  80.565665 | 1.1662463 | 0.6620597 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0969903 |  153.204043 | 0.6226772 | 124.954462 | 1.3354953 | 0.1560393 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9287540 |  113.908045 | 0.5271823 | 127.282729 | 1.0875033 | 0.0948843 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9777104 |  115.376989 | 0.5549711 | 120.150407 | 1.1791394 | 0.0281415 |
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
    ## 1 healthy~         8 NNAR        Test  0.0336  13.7  0.0239   9.35 0.0426  0.999
    ## 2 healthyR         7 EARTH       Test  0.0170   8.29 0.0198   7.16 0.0190  1.00 
    ## 3 healthy~         7 EARTH       Test  0.0321   2.75 0.0214   2.70 0.0692  0.997
    ## 4 healthy~         8 NNAR        Test  0.00576  1.81 0.00510  1.76 0.00726 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0314   2.61 0.0178   2.59 0.0472  0.998

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
    ## 1 healthyR.data <tibble [393 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [383 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [333 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [308 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [122 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
