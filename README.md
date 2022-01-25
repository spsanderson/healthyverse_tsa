Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
25 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,379
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

The last day in the data set is 2022-01-23 22:26:25, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1325.8
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27379          |
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
| r\_version     |      18292 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18292 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18292 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2296 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-23 | 2021-08-12 |       427 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1531119.73 | 1877451.34 | 357 | 17597 | 238827 | 3247727 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8065.92 |   15298.53 |   1 |   227 |   2781 |    8243 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-23 22:26:25 | 2021-08-12 19:45:41 |     16011 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 52M 40S |        60 |

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
    ## 1 healthyR.data <tibble [398 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [388 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [338 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [313 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [127 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7361854 |  450.535434 | 0.5360410 | 108.881171 | 0.9358472 | 0.2746216 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1046027 |   19.692870 | 0.0761647 |  22.608953 | 0.1637423 | 0.9713865 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.7858336 |  283.045018 | 0.5721915 | 116.681835 | 1.0074270 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0706721 |   30.494971 | 0.0514587 |  18.159494 | 0.0866990 | 0.9936251 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0304022 |   15.000508 | 0.0221368 |   8.087210 | 0.0459124 | 0.9979580 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0161456 |    3.893860 | 0.0117562 |   4.205207 | 0.0280695 | 0.9992328 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0723553 |   27.765874 | 0.0526843 |  18.551607 | 0.0993594 | 0.9914743 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4616637 |  285.706918 | 0.3361526 |  87.125371 | 0.5606444 | 0.9961644 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1410643 |  837.511979 | 0.8308467 | 126.019023 | 1.3605801 | 0.4231790 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3333526 |  746.554357 | 0.9708582 | 148.614399 | 1.5721913 | 0.0600223 |
| healthyR.data |         13 | TBATS                      | Test  | 0.7625630 |  275.999399 | 0.5552474 | 107.042278 | 1.0198938 | 0.0445230 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.7854178 |  282.466608 | 0.5718887 | 116.751963 | 1.0065430 | 0.0950886 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6606297 |  500.446839 | 0.8162885 | 153.009214 | 0.7607717 | 0.6357836 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0378311 |   17.806345 | 0.0467450 |  16.842326 | 0.0456857 | 0.9983356 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.4855403 |  225.837675 | 0.5999441 | 127.331267 | 0.6121216 | 0.0601404 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0367019 |   23.777703 | 0.0453496 |  23.042343 | 0.0444810 | 0.9986189 |
| healthyR      |          7 | EARTH                      | Test  | 0.0184992 |   10.328278 | 0.0228580 |   8.403512 | 0.0202343 | 0.9995546 |
| healthyR      |          8 | NNAR                       | Test  | 0.0069957 |    4.481015 | 0.0086440 |   3.691159 | 0.0111546 | 0.9996712 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0406120 |   18.358052 | 0.0501811 |  16.065488 | 0.0538773 | 0.9976917 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4223548 |  406.539932 | 0.5218708 | 132.091063 | 0.4676982 | 0.9976131 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2003413 | 1531.923238 | 1.4831679 | 124.662677 | 1.4197270 | 0.6765032 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 0.9715558 | 1514.805538 | 1.2004756 | 116.595368 | 1.1590482 | 0.4885577 |
| healthyR      |         13 | TBATS                      | Test  | 0.4662023 |  216.755727 | 0.5760498 | 120.001492 | 0.5926405 | 0.1032769 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.5193830 |   96.664246 | 0.6417610 | 178.411185 | 0.6303445 | 0.0301141 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.7883526 |  630.056324 | 0.5626183 | 111.589672 | 0.9927549 | 0.3837523 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0630469 |   96.599727 | 0.0449942 |  21.617901 | 0.0752274 | 0.9958626 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7349980 |  407.085755 | 0.5245410 | 123.280348 | 0.9298305 | 0.2666258 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0682401 |   91.377111 | 0.0487004 |  23.273141 | 0.0822866 | 0.9961717 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0316001 |    3.061011 | 0.0225518 |   3.009408 | 0.0707012 | 0.9966332 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0331963 |   11.581731 | 0.0236910 |  15.139855 | 0.0691688 | 0.9959982 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0756853 |  104.427208 | 0.0540138 |  22.544313 | 0.1009236 | 0.9949529 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4620375 |  522.351830 | 0.3297392 |  96.204501 | 0.5390504 | 0.9718600 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2313797 |  780.510323 | 0.8787905 | 126.066592 | 1.4620841 | 0.5626176 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2560689 | 1017.880674 | 0.8964102 | 140.527529 | 1.4438735 | 0.1977821 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6617888 |  278.138151 | 0.4722943 | 123.707443 | 0.8224483 | 0.4187256 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8660213 |  565.541992 | 0.6180475 | 129.988625 | 1.0545079 | 0.0019206 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8201644 |  436.684351 | 0.8307962 | 165.278264 | 0.9471195 | 0.2781287 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0593683 |   30.586746 | 0.0601379 |  17.770516 | 0.0696665 | 0.9969126 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.5525881 |  392.387085 | 0.5597513 | 113.936215 | 0.6404583 | 0.1147904 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0554219 |   21.423128 | 0.0561404 |  15.018933 | 0.0639667 | 0.9970495 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0102571 |    3.874889 | 0.0103900 |   3.465176 | 0.0148311 | 0.9997420 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0050167 |    1.090175 | 0.0050817 |   1.126073 | 0.0171156 | 0.9994365 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0780678 |   28.806864 | 0.0790798 |  24.880026 | 0.0877550 | 0.9955924 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4670223 |  270.405972 | 0.4730763 | 124.160945 | 0.5246542 | 0.9858215 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9821001 |  891.676625 | 0.9948310 | 114.557890 | 1.1517207 | 0.4927277 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1871009 | 1299.096040 | 1.2024892 | 128.679428 | 1.3783440 | 0.1405394 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5336284 |  208.951311 | 0.5405458 | 115.979902 | 0.6587485 | 0.1766735 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.5510056 |  299.341392 | 0.5581483 | 104.080470 | 0.6797450 | 0.0446826 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1156213 |  211.230542 | 0.6725967 | 126.083205 | 1.3376166 | 0.5944034 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0532076 |    8.943755 | 0.0320783 |   9.932966 | 0.0641160 | 0.9985473 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9957265 |  144.866934 | 0.6003133 | 126.402095 | 1.2221008 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0512159 |    8.141019 | 0.0308775 |   8.529630 | 0.0617429 | 0.9986537 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0228441 |    2.402286 | 0.0137725 |   2.392614 | 0.0363215 | 0.9989225 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.1173163 |   21.121242 | 0.0707288 |  25.708878 | 0.1563365 | 0.9824413 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0622618 |   11.211862 | 0.0375370 |  13.090977 | 0.0762626 | 0.9973756 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9497703 |  194.432959 | 0.5726068 | 128.256047 | 1.0787149 | 0.9150328 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1182969 |  249.706674 | 0.6742097 | 113.067218 | 1.2928079 | 0.5660457 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3177389 |  286.045903 | 0.7944513 | 133.690828 | 1.5069783 | 0.1579501 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9610412 |  156.807912 | 0.5794019 | 128.311556 | 1.1393755 | 0.1307648 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0840692 |  174.613442 | 0.6535742 | 127.377793 | 1.3162378 | 0.0082342 |
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
    ## 1 healthyR~         8 NNAR        Test  0.0161   3.89 0.0118   4.21 0.0281 0.999
    ## 2 healthyR          8 NNAR        Test  0.00700  4.48 0.00864  3.69 0.0112 1.00 
    ## 3 healthyR~         8 NNAR        Test  0.0332  11.6  0.0237  15.1  0.0692 0.996
    ## 4 healthyv~         7 EARTH       Test  0.0103   3.87 0.0104   3.47 0.0148 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0228   2.40 0.0138   2.39 0.0363 0.999

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
    ## 1 healthyR.data <tibble [398 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [388 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [338 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [313 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [127 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
