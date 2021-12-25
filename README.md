Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
25 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,703
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

The last day in the data set is 2021-12-23 19:48:10, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -579.16
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25703          |
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
| r\_version     |      17205 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17205 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17205 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2179 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-23 | 2021-08-04 |       396 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1533188.09 | 1879302.49 | 357 | 23389 | 238655 | 3246432 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8127.17 |   15346.15 |   1 |   227 |   2853 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-23 19:48:10 | 2021-08-04 00:22:37 |     14988 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 46M 26S |        60 |

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
    ## 1 healthyR.data <tibble [367 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [357 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [308 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [282 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [97 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.7974652 |  82.436666 | 0.6827202 |  75.962360 | 1.1277915 | 0.2510015 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.3198443 |  25.449973 | 0.2738228 |  25.635723 | 0.6970545 | 0.7109839 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.2133166 |  91.032396 | 1.0387359 | 151.244365 | 1.6075548 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2534191 |  16.226721 | 0.2169553 |  16.328160 | 0.6981218 | 0.7125965 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2358948 |   8.338910 | 0.2019526 |   8.634756 | 0.6509577 | 0.7379364 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2191812 |   7.157947 | 0.1876439 |   7.417689 | 0.6507781 | 0.7383229 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2477028 |  15.440740 | 0.2120616 |  15.449970 | 0.6894222 | 0.7215554 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6077888 |  42.365023 | 0.5203358 |  56.030688 | 1.0057898 | 0.6696839 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0537426 | 132.155728 | 0.9021226 |  83.357319 | 1.4017287 | 0.3499911 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4507356 | 138.212400 | 1.2419934 | 110.020798 | 1.8900917 | 0.0425700 |
| healthyR.data |         13 | TBATS                      | Test  | 1.1598408 |  96.196236 | 0.9929546 | 147.135941 | 1.5583078 | 0.0355450 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.2380118 |  92.804967 | 1.0598778 | 160.304363 | 1.6286932 | 0.0813568 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6768772 | 126.651367 | 0.7283843 |  91.899213 | 0.8196276 | 0.4884333 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0990220 |   9.929766 | 0.1065571 |   9.565357 | 0.2184860 | 0.9606681 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8486605 | 123.661724 | 0.9132394 | 123.871909 | 1.1016651 | 0.0048537 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1072506 |  12.734411 | 0.1154118 |  11.666924 | 0.2179385 | 0.9606890 |
| healthyR      |          7 | EARTH                      | Test  | 0.0376581 |   2.320058 | 0.0405237 |   2.243186 | 0.1029585 | 0.9926685 |
| healthyR      |          8 | NNAR                       | Test  | 0.0943290 |   6.904535 | 0.1015069 |   6.445193 | 0.2121489 | 0.9793996 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1091058 |  14.305132 | 0.1174082 |  12.625909 | 0.2130166 | 0.9628559 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2978548 |  52.825256 | 0.3205201 |  49.306416 | 0.4274842 | 0.9793006 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8586709 | 205.280491 | 0.9240115 |  97.379936 | 1.1880573 | 0.5085014 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1684098 | 205.688928 | 1.2573201 | 138.842836 | 1.4243319 | 0.0782911 |
| healthyR      |         13 | TBATS                      | Test  | 0.8026798 | 135.349546 | 0.8637598 | 109.215422 | 1.0601219 | 0.0141474 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8322719 | 117.133036 | 0.8956037 | 113.836186 | 1.0918023 | 0.0494119 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9062881 |  86.788437 | 0.6234618 |  91.840604 | 1.1249582 | 0.4090656 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1380121 |   8.358361 | 0.0949425 |   8.479473 | 0.3308293 | 0.9459029 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1280523 |  93.473587 | 0.7760199 | 125.191307 | 1.3732907 | 0.0693358 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1379213 |   8.223077 | 0.0948801 |   8.308678 | 0.3275009 | 0.9461094 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1251990 |   5.759238 | 0.0861280 |   5.564666 | 0.2194038 | 0.9741646 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1728763 |  10.009085 | 0.1189266 |   9.146900 | 0.3156675 | 0.9483040 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1406775 |   9.105923 | 0.0967761 |   9.131670 | 0.3211182 | 0.9483704 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5637165 |  38.814555 | 0.3877969 |  49.741871 | 0.7231741 | 0.9536275 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1071516 | 166.010117 | 0.7616417 |  90.839343 | 1.3660786 | 0.4922856 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4993878 | 188.228116 | 1.0314723 | 129.311943 | 1.7855592 | 0.0465176 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3503426 |  98.910663 | 0.9289398 | 159.324208 | 1.6375602 | 0.0237178 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2194344 |  94.846082 | 0.8388843 | 137.903081 | 1.4950788 | 0.0110867 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7550990 | 117.802681 | 0.7666126 | 126.795827 | 0.9411845 | 0.5066648 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1027913 |  15.007803 | 0.1043586 |  16.059100 | 0.1935833 | 0.9715575 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9809350 | 109.204975 | 0.9958922 | 164.304772 | 1.2272870 | 0.1059506 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1050339 |  13.061952 | 0.1066354 |  13.008035 | 0.1913745 | 0.9717623 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0791678 |   7.583381 | 0.0803749 |   7.420589 | 0.1345425 | 0.9894503 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0695366 |   5.833336 | 0.0705969 |   5.543124 | 0.1667211 | 0.9808356 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1049884 |  15.014735 | 0.1065893 |  15.731979 | 0.1855487 | 0.9732056 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4279600 |  53.613265 | 0.4344854 |  67.065733 | 0.5635744 | 0.9842187 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8132733 | 255.199637 | 0.8256740 |  87.264315 | 1.0619752 | 0.5905850 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1981706 | 385.591867 | 1.2164401 | 132.012946 | 1.4051637 | 0.0780897 |
| healthyverse  |         13 | TBATS                      | Test  | 1.0167579 | 144.971732 | 1.0322612 | 154.392108 | 1.3148496 | 0.0310748 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9970321 | 108.063974 | 1.0122347 | 179.171168 | 1.2841060 | 0.0650650 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.6879370 |  79.230129 | 0.6997407 |  88.673157 | 0.9442244 | 0.8831227 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1375666 |  15.249873 | 0.1399270 |  15.045798 | 0.2106740 | 0.9646157 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1348549 | 111.325808 | 1.1543268 | 179.484409 | 1.3807049 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1446796 |  17.000490 | 0.1471621 |  16.738309 | 0.2055897 | 0.9683348 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0643446 |   5.542783 | 0.0654486 |   5.560796 | 0.1318638 | 0.9861303 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2780240 |  28.627095 | 0.2827944 |  36.405771 | 0.4173918 | 0.8590181 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1498434 |  18.757049 | 0.1524144 |  18.452268 | 0.2037536 | 0.9696765 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4179708 |  65.540318 | 0.4251423 |  58.413584 | 0.5954094 | 0.8469000 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9145069 | 174.665064 | 0.9301981 |  78.984529 | 1.2708363 | 0.4314511 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4418322 | 217.900113 | 1.4665713 | 153.368628 | 1.7181904 | 0.0638686 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.2401492 | 148.702064 | 1.2614278 | 162.698419 | 1.4646931 | 0.0326831 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9685906 |  93.355228 | 0.9852098 | 150.927583 | 1.2157341 | 0.0065565 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = .2,
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
    ## 1 healthyR.da~         8 NNAR        Test  0.219   7.16 0.188   7.42 0.651 0.738
    ## 2 healthyR             7 EARTH       Test  0.0377  2.32 0.0405  2.24 0.103 0.993
    ## 3 healthyR.ts          7 EARTH       Test  0.125   5.76 0.0861  5.56 0.219 0.974
    ## 4 healthyverse         7 EARTH       Test  0.0792  7.58 0.0804  7.42 0.135 0.989
    ## 5 healthyR.ai          7 EARTH       Test  0.0643  5.54 0.0654  5.56 0.132 0.986

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
    ## 1 healthyR.data <tibble [367 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [357 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [308 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [282 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [97 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
