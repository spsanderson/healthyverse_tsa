Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,007
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

The last day in the data set is 2022-01-17 20:51:46, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1180.22
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27007          |
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
| r\_version     |      18067 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18067 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18067 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2264 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-17 | 2021-08-09 |       421 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1533429.6 | 1878457.39 | 357 | 16916 | 238827 | 3247239 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8050.3 |   15267.84 |   1 |   221 |   2802 |    8244 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-17 20:51:46 | 2021-08-09 13:31:32 |     15756 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 51M 36S |        60 |

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
    ## 1 healthyR.data <tibble [392 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [382 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [332 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [307 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [121 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8080421 | 334.072338 | 0.5908578 | 110.393498 | 1.0011221 | 0.4776479 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1439041 |  27.631416 | 0.1052257 |  27.885652 | 0.3309953 | 0.9194756 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9326455 | 325.000798 | 0.6819704 | 109.125936 | 1.2117449 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0734937 |  23.222986 | 0.0537402 |  17.670575 | 0.0914313 | 0.9945593 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0308675 |  10.163258 | 0.0225709 |   7.032388 | 0.0466585 | 0.9980737 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0194471 |   6.053989 | 0.0142201 |   8.073950 | 0.0290319 | 0.9992307 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0787599 |  24.460050 | 0.0575909 |  18.112989 | 0.1102393 | 0.9920524 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5016134 | 219.013094 | 0.3667905 |  87.908863 | 0.6069377 | 0.9939990 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0394235 | 510.237617 | 0.7600488 | 102.882408 | 1.2908031 | 0.5621727 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3047841 | 368.119814 | 0.9540862 | 138.986898 | 1.4801181 | 0.1614877 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8732345 | 265.363451 | 0.6385278 | 105.291036 | 1.1605677 | 0.1521119 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9317065 | 324.463039 | 0.6812838 | 109.097243 | 1.2107144 | 0.0710935 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8675731 | 459.028346 | 1.0155668 | 157.605934 | 0.9491495 | 0.7359199 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0364474 |  13.899809 | 0.0426647 |  15.891314 | 0.0450106 | 0.9987802 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.7941203 | 377.012795 | 0.9295842 | 151.094429 | 0.9427137 | 0.1399242 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0371678 |  18.875005 | 0.0435080 |  21.203860 | 0.0471366 | 0.9989314 |
| healthyR      |          7 | EARTH                      | Test  | 0.0174179 |   7.506562 | 0.0203891 |   6.672492 | 0.0192283 | 0.9996240 |
| healthyR      |          8 | NNAR                       | Test  | 0.0140828 |   4.147198 | 0.0164851 |   3.976216 | 0.0240227 | 0.9989676 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0392851 |  18.617209 | 0.0459865 |  18.565369 | 0.0549866 | 0.9981046 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4379552 | 285.579212 | 0.5126632 | 128.584418 | 0.4934729 | 0.9965464 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1600147 | 956.598992 | 1.3578941 | 126.233616 | 1.4157253 | 0.6371909 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0362070 | 691.641584 | 1.2129669 | 124.195722 | 1.3702156 | 0.4106478 |
| healthyR      |         13 | TBATS                      | Test  | 0.7856737 | 358.590348 | 0.9196967 | 151.262577 | 0.9093975 | 0.4337970 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8107911 | 437.745722 | 0.9490987 | 148.833707 | 0.9677904 | 0.0080396 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7623983 | 267.003395 | 0.5065630 | 116.038914 | 0.9253550 | 0.3130115 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0658328 |  58.578715 | 0.0437415 |  21.668455 | 0.0758131 | 0.9959853 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.9055302 | 546.473840 | 0.6016646 | 112.586291 | 1.0877401 | 0.2168498 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0788125 |  55.597774 | 0.0523657 |  21.397993 | 0.0976518 | 0.9963460 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0317249 |   2.831274 | 0.0210791 |   2.782419 | 0.0688896 | 0.9966406 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0390450 |  12.438808 | 0.0259428 |   9.697661 | 0.0817086 | 0.9941348 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1072663 |  81.104941 | 0.0712714 |  30.190166 | 0.1327165 | 0.9953937 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4210125 | 317.753869 | 0.2797348 |  85.058087 | 0.5158646 | 0.9894062 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2202933 | 624.469974 | 0.8108039 | 122.944409 | 1.4580195 | 0.5493247 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3276777 | 796.800729 | 0.8821536 | 130.935541 | 1.6036298 | 0.1714879 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6579800 | 208.638075 | 0.4371840 | 108.665889 | 0.8190030 | 0.4617619 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9334962 | 595.009355 | 0.6202462 | 112.082524 | 1.1622349 | 0.0373807 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9911562 | 354.629276 | 0.8828166 | 164.093463 | 1.1569236 | 0.6492526 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0646775 |  27.009328 | 0.0576078 |  19.059075 | 0.0774334 | 0.9962907 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7280030 | 148.900311 | 0.6484277 | 147.362185 | 0.9031078 | 0.2515875 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0652222 |  22.921067 | 0.0580930 |  17.107727 | 0.0763468 | 0.9966244 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0104248 |   2.991924 | 0.0092853 |   2.837992 | 0.0150779 | 0.9997638 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0046203 |   1.578807 | 0.0041153 |   1.502892 | 0.0096607 | 0.9998557 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0647695 |  21.633497 | 0.0576897 |  16.825671 | 0.0755153 | 0.9960997 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6015130 | 265.381720 | 0.5357638 | 132.516900 | 0.6788823 | 0.9852026 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9753330 | 683.348035 | 0.8687229 | 109.849635 | 1.1800852 | 0.5748610 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0988879 | 784.808806 | 0.9787725 | 120.449734 | 1.3950075 | 0.2106926 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5632763 | 165.017267 | 0.5017067 | 119.453524 | 0.6938904 | 0.4772053 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8187375 | 179.310726 | 0.7292443 | 171.960196 | 1.0096544 | 0.0341410 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8216516 | 107.890811 | 0.4661651 | 108.597063 | 1.0515811 | 0.4731619 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1169715 |  17.827711 | 0.0663639 |  19.911778 | 0.1325281 | 0.9985924 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9472956 | 122.323790 | 0.5374494 | 124.570361 | 1.1304784 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1187770 |  18.050471 | 0.0673883 |  20.101278 | 0.1338686 | 0.9986273 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0276161 |   2.753503 | 0.0156680 |   2.717806 | 0.0399374 | 0.9987966 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3194705 |  34.870845 | 0.1812520 |  46.081639 | 0.3853936 | 0.9711349 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1550981 |  23.848841 | 0.0879951 |  24.859229 | 0.1723865 | 0.9973171 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8264060 | 149.881789 | 0.4688625 | 109.545374 | 0.9918785 | 0.9330208 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8248729 | 130.247631 | 0.4679927 |  77.737802 | 1.0382926 | 0.6882593 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1446798 | 204.747199 | 0.6494356 | 127.574263 | 1.3527921 | 0.1710790 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8925316 | 117.792229 | 0.5063790 | 119.597639 | 1.0922002 | 0.0737542 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9705504 | 134.502868 | 0.5506430 | 118.043850 | 1.1863057 | 0.0107130 |
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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.0194   6.05 0.0142   8.07 0.0290  0.999
    ## 2 healthyR         7 EARTH       Test  0.0174   7.51 0.0204   6.67 0.0192  1.00 
    ## 3 healthy~         7 EARTH       Test  0.0317   2.83 0.0211   2.78 0.0689  0.997
    ## 4 healthy~         8 NNAR        Test  0.00462  1.58 0.00412  1.50 0.00966 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0276   2.75 0.0157   2.72 0.0399  0.999

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
    ## 1 healthyR.data <tibble [392 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [382 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [332 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [307 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [121 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
