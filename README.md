Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
31 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,026
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

The last day in the data set is 2021-12-29 21:45:06, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -725.11
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26026          |
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
| r\_version     |      17463 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17463 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17463 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2210 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-29 | 2021-08-04 |       402 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1528583.67 | 1877902.45 | 357 | 17597 | 238433 | 3246357 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8057.96 |   15273.93 |   1 |   204 |   2823 |    8218 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-29 21:45:06 | 2021-08-04 04:49:11 |     15151 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     20 |        60 |

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
    ## 1 healthyR.data <tibble [373 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [363 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [314 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [288 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [103 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8089453 |  92.532411 | 0.6313800 |  82.934554 | 1.1717163 | 0.5362766 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2260794 |  11.608472 | 0.1764545 |  12.371580 | 0.7085467 | 0.7633265 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0215353 |  93.436830 | 0.7973060 | 107.383773 | 1.4611184 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2274662 |  11.844215 | 0.1775369 |  12.628251 | 0.7089617 | 0.7629737 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2030197 |   6.165647 | 0.1584564 |   6.770242 | 0.6702019 | 0.7769739 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2411715 |   8.065008 | 0.1882338 |   8.458899 | 0.7431341 | 0.7276045 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2192192 |  10.317895 | 0.1711001 |  11.050227 | 0.7020147 | 0.7695203 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5200369 |  38.578977 | 0.4058876 |  50.288842 | 0.9484336 | 0.7405482 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9684814 | 134.963159 | 0.7558976 |  92.821830 | 1.1620213 | 0.5643019 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2512867 | 138.638026 | 0.9766265 | 120.790370 | 1.5253819 | 0.1141581 |
| healthyR.data |         13 | BATS                       | Test  | 1.0562059 |  92.319433 | 0.8243664 | 117.810359 | 1.4855032 | 0.0099565 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0253711 |  92.623002 | 0.8002999 | 108.851197 | 1.4641479 | 0.2696943 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7373488 | 147.408120 | 0.7273220 | 108.890393 | 0.8721318 | 0.6244516 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0929615 |   9.627305 | 0.0916974 |   9.367717 | 0.2215488 | 0.9671407 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8621161 | 158.201224 | 0.8503927 | 116.754120 | 1.1236946 | 0.0209606 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0966501 |  11.423261 | 0.0953358 |  10.855076 | 0.2205774 | 0.9671713 |
| healthyR      |          7 | EARTH                      | Test  | 0.0374898 |   2.383453 | 0.0369800 |   2.301369 | 0.1047626 | 0.9938332 |
| healthyR      |          8 | NNAR                       | Test  | 0.1067401 |   8.236453 | 0.1052886 |   7.626597 | 0.2164368 | 0.9868652 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0953734 |  11.353418 | 0.0940765 |  10.691804 | 0.2160191 | 0.9688473 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3346381 |  63.607048 | 0.3300875 |  66.214799 | 0.4637999 | 0.9829292 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0419408 | 236.850957 | 1.0277721 | 110.046734 | 1.2944502 | 0.5506317 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2653355 | 252.769038 | 1.2481290 | 132.411511 | 1.5171602 | 0.1069670 |
| healthyR      |         13 | TBATS                      | Test  | 0.8621397 | 165.675254 | 0.8504160 | 114.391180 | 1.1251782 | 0.0200119 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8732591 | 150.216826 | 0.8613842 | 117.463761 | 1.1340219 | 0.3282402 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9202761 | 101.367424 | 0.5763015 |  99.670810 | 1.1191664 | 0.6312458 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1267982 |   7.172566 | 0.0794045 |   7.409195 | 0.3358028 | 0.9516899 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1383535 | 104.265344 | 0.7128675 | 129.019875 | 1.3765939 | 0.1073188 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1267613 |   7.276097 | 0.0793813 |   7.488350 | 0.3361705 | 0.9518377 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1257214 |   5.662839 | 0.0787301 |   5.461055 | 0.2241700 | 0.9763446 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1779931 |  10.610286 | 0.1114641 |   9.587475 | 0.3301581 | 0.9505502 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1300857 |   8.000581 | 0.0814631 |   8.128272 | 0.3322954 | 0.9535550 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5274090 |  38.588225 | 0.3302776 |  48.231620 | 0.6911928 | 0.9596173 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2061277 | 185.081121 | 0.7553095 | 107.389211 | 1.3851960 | 0.5648269 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5185061 | 212.415097 | 0.9509293 | 137.195684 | 1.7820074 | 0.0887060 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3357311 | 104.766805 | 0.8364706 | 164.495546 | 1.6000517 | 0.1008438 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1729425 | 104.120497 | 0.7345281 | 125.101914 | 1.4538330 | 0.2843183 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7752234 | 179.689060 | 0.7384470 | 134.670391 | 0.9538351 | 0.5942202 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1034605 |  19.709486 | 0.0985523 |  21.651997 | 0.1983426 | 0.9757822 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9105122 | 192.957127 | 0.8673177 | 156.198783 | 1.1373590 | 0.1430596 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1076592 |  22.746942 | 0.1025518 |  24.034261 | 0.2000027 | 0.9757042 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0664448 |   7.057814 | 0.0632927 |   7.002005 | 0.1243407 | 0.9916475 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0467832 |   3.718964 | 0.0445638 |   3.555166 | 0.1205684 | 0.9942009 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0996406 |  18.510104 | 0.0949137 |  19.978419 | 0.1916095 | 0.9773996 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3640174 |  45.259843 | 0.3467485 |  56.600450 | 0.5188722 | 0.9851672 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8344599 | 354.961794 | 0.7948734 | 102.246448 | 1.0200541 | 0.6896038 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2415842 | 546.198554 | 1.1826837 | 142.957406 | 1.4135274 | 0.1295575 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8772545 | 108.079259 | 0.8356377 | 144.116125 | 1.1602366 | 0.3443732 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9145870 | 123.736412 | 0.8711992 | 158.114410 | 1.2166838 | 0.3503788 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7604138 |  93.142498 | 0.6131545 | 101.987581 | 0.9737219 | 0.8817358 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1436283 |  16.303481 | 0.1158137 |  19.269220 | 0.2137148 | 0.9673150 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1394375 | 101.587218 | 0.9187777 | 175.916300 | 1.3891380 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1435886 |  16.323480 | 0.1157817 |  19.320777 | 0.2133495 | 0.9674889 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0642382 |   5.369959 | 0.0517981 |   5.345067 | 0.1274153 | 0.9879579 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3216906 |  35.411941 | 0.2593930 |  37.757567 | 0.4676578 | 0.8763880 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1331677 |  14.963663 | 0.1073788 |  16.852658 | 0.2104151 | 0.9661394 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5064652 |  90.981671 | 0.4083847 |  75.892083 | 0.6705672 | 0.8635954 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9903921 | 159.724191 | 0.7985959 |  79.104792 | 1.2998780 | 0.5782864 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3464067 | 181.356039 | 1.0856658 | 138.403930 | 1.5815793 | 0.1550979 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.1294464 | 120.132928 | 0.9107214 | 153.117169 | 1.3603628 | 0.0677224 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9943841 | 105.973679 | 0.8018148 | 140.325302 | 1.2149914 | 0.1208873 |
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
    ##   package      .model_id .model_desc .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>            <int> <chr>       <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR.da~         7 EARTH       Test  0.203   6.17 0.158   6.77 0.670 0.777
    ## 2 healthyR             7 EARTH       Test  0.0375  2.38 0.0370  2.30 0.105 0.994
    ## 3 healthyR.ts          7 EARTH       Test  0.126   5.66 0.0787  5.46 0.224 0.976
    ## 4 healthyverse         8 NNAR        Test  0.0468  3.72 0.0446  3.56 0.121 0.994
    ## 5 healthyR.ai          7 EARTH       Test  0.0642  5.37 0.0518  5.35 0.127 0.988

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
    ## 1 healthyR.data <tibble [373 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [363 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [314 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [288 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [103 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
