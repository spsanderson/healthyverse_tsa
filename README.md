Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
26 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,593
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

The last day in the data set is 2022-01-24 22:37:05, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1349.98
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27593          |
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
| r\_version     |      18414 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18414 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18414 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2315 |           0.92 |   2 |   2 |     0 |       100 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-24 | 2021-08-14 |       428 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1531232.97 | 1877422.73 | 357 | 19045 | 238833 | 3247923 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8050.35 |   15266.21 |   1 |   238 |   2757 |    8208 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-24 22:37:05 | 2021-08-14 13:31:54 |     16127 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 53M 30S |        60 |

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
    ## 1 healthyR.data <tibble [399 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [389 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [339 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [314 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [128 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7896307 |  340.234391 | 0.5622401 | 114.227092 | 0.9949314 | 0.2968461 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1044072 |   19.833397 | 0.0743409 |  22.556570 | 0.1624237 | 0.9729194 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8477300 |  224.396116 | 0.6036084 | 120.873886 | 1.0782781 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0678281 |   21.982893 | 0.0482956 |  17.627377 | 0.0841776 | 0.9937421 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0304625 |   10.875543 | 0.0216902 |   7.299733 | 0.0458032 | 0.9980414 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0106087 |    6.822968 | 0.0075537 |   4.818631 | 0.0149086 | 0.9998642 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0722803 |   22.585736 | 0.0514657 |  18.175231 | 0.0990457 | 0.9916031 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5286775 |  224.073537 | 0.3764338 |  95.282206 | 0.6279546 | 0.9960126 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1738058 |  623.679910 | 0.8357839 | 131.347160 | 1.3657418 | 0.3823756 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4263328 |  568.072934 | 1.0155905 | 152.687967 | 1.6676267 | 0.0324939 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8258194 |  218.303544 | 0.5880074 | 113.095263 | 1.0819834 | 0.0556491 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8471817 |  223.926303 | 0.6032180 | 120.946438 | 1.0771841 | 0.1183257 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7186072 |  370.762098 | 0.8647441 | 158.289855 | 0.8380256 | 0.5530691 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0366652 |   12.216520 | 0.0441215 |  14.591540 | 0.0446526 | 0.9986770 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5257824 |  119.247199 | 0.6327062 | 134.038947 | 0.6798256 | 0.1232133 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0348074 |   15.815356 | 0.0418859 |  18.251847 | 0.0422753 | 0.9988869 |
| healthyR      |          7 | EARTH                      | Test  | 0.0182780 |    8.094042 | 0.0219951 |   7.120579 | 0.0201245 | 0.9995067 |
| healthyR      |          8 | NNAR                       | Test  | 0.0154125 |    8.036569 | 0.0185468 |  10.130483 | 0.0343427 | 0.9973707 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0386091 |   13.200848 | 0.0464607 |  14.543725 | 0.0520249 | 0.9979024 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4609334 |  301.703070 | 0.5546694 | 138.626508 | 0.5075751 | 0.9945484 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2405619 | 1077.103552 | 1.4928443 | 126.317543 | 1.4461412 | 0.6005633 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0736814 | 1039.419985 | 1.2920268 | 123.538516 | 1.2605484 | 0.3658534 |
| healthyR      |         13 | TBATS                      | Test  | 0.4849537 |  154.402569 | 0.5835746 | 115.054878 | 0.6412847 | 0.2190586 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.5852765 |  132.324076 | 0.7042991 | 190.911264 | 0.7035000 | 0.0469135 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.6641736 |  155.528612 | 0.5044439 | 123.786025 | 0.8216635 | 0.2844070 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0658121 |   64.462131 | 0.0499847 |  21.914869 | 0.0765666 | 0.9952343 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7059469 |  253.613613 | 0.5361709 | 152.175484 | 0.8934735 | 0.2312354 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0708773 |   61.917798 | 0.0538318 |  23.165806 | 0.0837959 | 0.9955655 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0308257 |    2.913628 | 0.0234123 |   2.858671 | 0.0696066 | 0.9963271 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0477757 |   13.202690 | 0.0362859 |  12.661494 | 0.0986728 | 0.9901450 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0771039 |   69.643749 | 0.0585609 |  22.458953 | 0.1000772 | 0.9942546 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4520922 |  370.841940 | 0.3433668 | 101.823162 | 0.5321405 | 0.9825753 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2131107 |  607.747942 | 0.9213650 | 129.444060 | 1.4392916 | 0.5415791 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2119007 |  763.643821 | 0.9204460 | 137.931922 | 1.4283665 | 0.1950756 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6437988 |  231.552338 | 0.4889691 | 123.308750 | 0.8055037 | 0.3510096 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.7915285 |  165.977675 | 0.6011707 | 161.995633 | 0.9990676 | 0.0158757 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8359097 |  335.565293 | 0.8426408 | 164.866262 | 0.9656357 | 0.3024953 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0611062 |   24.145385 | 0.0615983 |  17.231164 | 0.0703484 | 0.9968058 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.5903040 |  228.011085 | 0.5950574 | 125.416750 | 0.7055343 | 0.0852802 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0566218 |   18.282041 | 0.0570777 |  14.520527 | 0.0648058 | 0.9968900 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0100596 |    3.146084 | 0.0101406 |   2.969184 | 0.0147806 | 0.9997062 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0074813 |    3.978674 | 0.0075415 |   3.333264 | 0.0264809 | 0.9984755 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0821762 |   27.494276 | 0.0828379 |  25.552114 | 0.0913305 | 0.9959852 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5135295 |  219.371618 | 0.5176646 | 130.231011 | 0.5688567 | 0.9890191 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9940362 |  664.515056 | 1.0020406 | 117.009371 | 1.1501533 | 0.4344233 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3146091 |  952.471716 | 1.3251948 | 138.424506 | 1.4906752 | 0.0960968 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9149830 |  252.663674 | 0.9223507 | 168.856011 | 1.0985668 | 0.0516322 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.5730199 |  144.619480 | 0.5776340 | 114.448369 | 0.7336182 | 0.0376856 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9849349 |  188.360830 | 0.6106881 | 121.127797 | 1.2181285 | 0.5084287 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0551327 |   10.553939 | 0.0341838 |  11.913174 | 0.0651281 | 0.9981434 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.8861751 |  123.032959 | 0.5494541 | 126.914772 | 1.0941477 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0527288 |    9.615049 | 0.0326934 |  10.221884 | 0.0616850 | 0.9983444 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0217709 |    2.534635 | 0.0134986 |   2.526789 | 0.0350867 | 0.9989888 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.1151443 |   22.186307 | 0.0713928 |  24.819548 | 0.1469251 | 0.9795557 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0694799 |   14.150352 | 0.0430795 |  16.980650 | 0.0858259 | 0.9966373 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6926021 |  163.164439 | 0.4294333 | 114.052805 | 0.8246177 | 0.9707476 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1473376 |  276.889790 | 0.7113824 | 119.572370 | 1.3303436 | 0.5126371 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3281327 |  310.802371 | 0.8234807 | 139.833572 | 1.4830759 | 0.1176111 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9144462 |  146.528505 | 0.5669831 | 130.334212 | 1.1070254 | 0.0701739 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9409434 |  149.426065 | 0.5834121 | 124.780561 | 1.1629256 | 0.0289267 |
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
    ##   package    .model_id .model_desc .type    mae  mape    mase smape   rmse   rsq
    ##   <chr>          <int> <chr>       <chr>  <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR.~         8 NNAR        Test  0.0106  6.82 0.00755  4.82 0.0149 1.00 
    ## 2 healthyR           7 EARTH       Test  0.0183  8.09 0.0220   7.12 0.0201 1.00 
    ## 3 healthyR.~         7 EARTH       Test  0.0308  2.91 0.0234   2.86 0.0696 0.996
    ## 4 healthyve~         7 EARTH       Test  0.0101  3.15 0.0101   2.97 0.0148 1.00 
    ## 5 healthyR.~         7 EARTH       Test  0.0218  2.53 0.0135   2.53 0.0351 0.999

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
    ## 1 healthyR.data <tibble [399 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [389 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [339 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [314 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [128 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
