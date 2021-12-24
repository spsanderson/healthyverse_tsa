Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
24 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,668
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

The last day in the data set is 2021-12-22 21:47:54, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -557.16
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25668          |
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
| r\_version     |      17194 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17194 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17194 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2170 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-22 | 2021-08-03 |       395 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532370.89 | 1879264.29 | 357 | 22373.75 | 238637 | 3246416 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8133.39 |   15353.01 |   1 |   232.00 |   2854 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-22 21:47:54 | 2021-08-03 23:47:07 |     14957 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |      9 |        60 |

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
    ## 1 healthyR.data <tibble [366 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [356 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [307 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [281 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [96 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8043954 |  80.839472 | 0.6768128 |  75.896694 | 1.1421716 | 0.2295077 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.3270808 |  26.435875 | 0.2752036 |  27.079332 | 0.6943647 | 0.7126586 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.2244818 |  91.885211 | 1.0302706 | 155.188854 | 1.6153967 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2616221 |  17.335212 | 0.2201270 |  17.394129 | 0.6941522 | 0.7142398 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2306294 |   8.132695 | 0.1940500 |   8.522709 | 0.6509807 | 0.7372643 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2700773 |  11.549261 | 0.2272412 |  10.705163 | 0.6760928 | 0.7249452 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2563012 |  16.708427 | 0.2156501 |  16.656759 | 0.6856882 | 0.7228464 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5898689 |  40.875757 | 0.4963116 |  52.792743 | 0.9926625 | 0.6688366 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0044543 | 121.150198 | 0.8451409 |  83.074890 | 1.3611354 | 0.3705235 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4169780 | 121.989666 | 1.1922355 | 113.247447 | 1.8772267 | 0.0399172 |
| healthyR.data |         13 | TBATS                      | Test  | 1.1659134 |  97.729380 | 0.9809915 | 148.463874 | 1.5612507 | 0.0361329 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.2498449 |  93.610155 | 1.0516109 | 164.678869 | 1.6378199 | 0.0489649 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6781191 | 126.083811 | 0.7278158 |  91.792929 | 0.8183792 | 0.4791195 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0996809 |   9.660643 | 0.1069862 |   9.335165 | 0.2176004 | 0.9604340 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8237637 | 121.441817 | 0.8841342 | 115.762536 | 1.0844356 | 0.0073550 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1092432 |  12.758727 | 0.1172492 |  11.620394 | 0.2171492 | 0.9604707 |
| healthyR      |          7 | EARTH                      | Test  | 0.0375457 |   2.285632 | 0.0402973 |   2.209221 | 0.1026975 | 0.9926538 |
| healthyR      |          8 | NNAR                       | Test  | 0.0972698 |   9.802086 | 0.1043983 |   9.506405 | 0.1936933 | 0.9834529 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1111440 |  14.684159 | 0.1192893 |  12.738737 | 0.2125236 | 0.9626689 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2954630 |  52.089568 | 0.3171164 |  47.804808 | 0.4285752 | 0.9794247 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9079225 | 218.565688 | 0.9744607 | 102.492610 | 1.1766435 | 0.4988904 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1901436 | 218.029832 | 1.2773647 | 133.337990 | 1.4337636 | 0.0724023 |
| healthyR      |         13 | TBATS                      | Test  | 0.7708680 | 136.143891 | 0.8273619 | 104.185662 | 1.0418755 | 0.0300303 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8390462 | 115.567262 | 0.9005366 | 115.808941 | 1.0995244 | 0.0311019 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9100252 |  87.033681 | 0.6307190 |  93.578433 | 1.1196995 | 0.4400599 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1401796 |   8.760334 | 0.0971555 |   8.815448 | 0.3292947 | 0.9460071 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.2128792 |  91.925061 | 0.8406205 | 138.726584 | 1.4631741 | 0.0681471 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1409828 |   8.864533 | 0.0977121 |   8.838860 | 0.3249368 | 0.9463672 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1248680 |   5.755556 | 0.0865433 |   5.561032 | 0.2187631 | 0.9743124 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1523651 |   8.113931 | 0.1056010 |   7.313885 | 0.3058205 | 0.9512162 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1425823 |   9.599672 | 0.0988207 |   9.553996 | 0.3181345 | 0.9487403 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5771534 |  40.208064 | 0.4000126 |  52.194066 | 0.7376583 | 0.9533522 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0592957 | 157.860197 | 0.7341751 |  90.203549 | 1.3108559 | 0.5202446 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4965342 | 185.947684 | 1.0372158 | 130.884175 | 1.7916210 | 0.0463243 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3484308 |  97.701138 | 0.9345684 | 156.073675 | 1.6443992 | 0.0244270 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2647063 |  95.995411 | 0.8765408 | 151.976803 | 1.5371505 | 0.0046300 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7528761 | 114.940301 | 0.7746790 | 125.893495 | 0.9397690 | 0.4900892 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1102657 |  13.756738 | 0.1134589 |  13.780921 | 0.1913882 | 0.9715170 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.0015050 | 110.107481 | 1.0305081 | 168.423719 | 1.2434495 | 0.1119383 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1109262 |  13.307073 | 0.1141386 |  13.145475 | 0.1914138 | 0.9715440 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0790946 |   7.588889 | 0.0813851 |   7.461729 | 0.1342187 | 0.9894345 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0481110 |   4.152333 | 0.0495042 |   4.037710 | 0.1213171 | 0.9900990 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1097110 |  15.269981 | 0.1128882 |  16.056105 | 0.1855823 | 0.9730424 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4367850 |  60.457562 | 0.4494340 |  65.599420 | 0.5690338 | 0.9835640 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7845348 | 248.120404 | 0.8072546 |  80.454110 | 1.0858927 | 0.5906222 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1698393 | 365.746279 | 1.2037173 | 129.846004 | 1.3831881 | 0.0756822 |
| healthyverse  |         13 | TBATS                      | Test  | 1.0295661 | 138.751599 | 1.0593819 | 150.460994 | 1.3186201 | 0.0268516 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.0344165 | 119.406943 | 1.0643728 | 173.341405 | 1.3173034 | 0.0527511 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7355193 |  89.519494 | 0.7498805 |  99.127930 | 1.0115032 | 0.1289987 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1473729 |  17.978843 | 0.1502504 |  18.059038 | 0.2089869 | 0.9666480 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1112201 | 113.230614 | 1.1329169 | 178.698707 | 1.3648586 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1599696 |  20.983306 | 0.1630931 |  21.258106 | 0.2131385 | 0.9678042 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0581046 |   5.165016 | 0.0592391 |   5.204624 | 0.1249118 | 0.9876944 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2745232 |  32.833086 | 0.2798833 |  38.654364 | 0.4032232 | 0.8890360 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1661841 |  22.762558 | 0.1694289 |  23.350620 | 0.2139154 | 0.9687036 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3994839 |  62.807521 | 0.4072839 |  56.915240 | 0.5872990 | 0.8582483 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8192249 | 161.767042 | 0.8352205 |  71.560184 | 1.2256205 | 0.4488793 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3630995 | 212.447342 | 1.3897143 | 155.546318 | 1.6543887 | 0.0743230 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.2242725 | 159.565465 | 1.2481767 | 160.412324 | 1.4441748 | 0.0453248 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9571130 |  93.563072 | 0.9758009 | 157.679021 | 1.2135141 | 0.0097931 |
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
    ## 1 healthyR.da~         7 EARTH       Test  0.231   8.13 0.194   8.52 0.651 0.737
    ## 2 healthyR             7 EARTH       Test  0.0375  2.29 0.0403  2.21 0.103 0.993
    ## 3 healthyR.ts          7 EARTH       Test  0.125   5.76 0.0865  5.56 0.219 0.974
    ## 4 healthyverse         8 NNAR        Test  0.0481  4.15 0.0495  4.04 0.121 0.990
    ## 5 healthyR.ai          7 EARTH       Test  0.0581  5.17 0.0592  5.20 0.125 0.988

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
    ## 1 healthyR.data <tibble [366 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [356 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [307 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [281 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [96 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
