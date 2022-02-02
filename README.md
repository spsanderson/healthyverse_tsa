Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
02 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,104
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

The last day in the data set is 2022-01-31 23:58:25, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1519.33
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28104          |
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
| r\_version     |      18681 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18681 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18681 |           0.34 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2363 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-31 | 2021-08-20 |       435 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1527464.07 | 1875495.38 | 357 | 23901.50 | 267276 | 3247929 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8212.11 |   15695.54 |   1 |   237.75 |   2806 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-31 23:58:25 | 2021-08-20 02:06:48 |     16465 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   34.5 |        60 |

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
    ## 1 healthyR.data <tibble [406 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [396 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [346 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [321 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [135 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9789939 |  107.1449535 | 0.6875016 | 122.3263564 | 1.2270480 | 0.5236596 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0878590 |   11.2721835 | 0.0616993 |  12.5895198 | 0.1422434 | 0.9810191 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9869009 |   96.3359627 | 0.6930543 | 131.5838038 | 1.2159035 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0634971 |    8.0080399 | 0.0445910 |   7.8114125 | 0.0776119 | 0.9949437 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0291561 |    3.3996813 | 0.0204750 |   3.3620509 | 0.0385533 | 0.9988008 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0095612 |    1.1554244 | 0.0067144 |   1.1586150 | 0.0142654 | 0.9998416 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0637217 |    8.3179042 | 0.0447488 |   8.3479400 | 0.0863985 | 0.9937498 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5884652 |   68.5913887 | 0.4132516 |  91.6511523 | 0.7035330 | 0.9940969 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0510094 |  145.3749662 | 0.7380747 |  95.8653580 | 1.3174137 | 0.5150496 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2759271 |  179.8637039 | 0.8960239 | 117.9944243 | 1.6560839 | 0.0801038 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9986137 |  101.4142407 | 0.7012797 | 124.3870336 | 1.2590562 | 0.1065674 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9863311 |   96.2571123 | 0.6926542 | 131.6459133 | 1.2149132 | 0.1444937 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8145870 |  667.0518253 | 0.9884801 | 164.8084647 | 1.0038717 | 0.4561238 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0470790 |   35.4216639 | 0.0571291 |  20.0914316 | 0.0612398 | 0.9943267 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5598026 |  327.7967583 | 0.6793059 | 122.9992604 | 0.7434504 | 0.2488616 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0446032 |   35.1652251 | 0.0541248 |  25.8314894 | 0.0598559 | 0.9943298 |
| healthyR      |          7 | EARTH                      | Test  | 0.0291908 |   14.5400721 | 0.0354222 |  10.8684618 | 0.0699732 | 0.9932671 |
| healthyR      |          8 | NNAR                       | Test  | 0.0125691 |    5.1412691 | 0.0152523 |   4.1174362 | 0.0238864 | 0.9991478 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0482384 |   36.4243051 | 0.0585360 |  19.6910813 | 0.0663322 | 0.9931612 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4953917 |  572.9459512 | 0.6011450 | 126.8070573 | 0.5909902 | 0.9652986 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2420098 | 1932.3603900 | 1.5071466 | 126.4753210 | 1.4517100 | 0.5077752 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1327196 | 2008.3034988 | 1.3745257 | 125.9427263 | 1.3859323 | 0.1979400 |
| healthyR      |         13 | TBATS                      | Test  | 0.5602814 |  274.8261462 | 0.6798869 | 128.7615340 | 0.7405014 | 0.2970505 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6205501 |  286.4564795 | 0.7530215 | 144.5965323 | 0.8174860 | 0.1927599 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.8006023 |  528.3379315 | 0.6437372 | 138.2019948 | 1.0127731 | 0.5088217 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0729684 |  141.6472190 | 0.0586714 |  27.9241227 | 0.0862140 | 0.9945849 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7809915 |  453.1680014 | 0.6279689 | 153.6088172 | 1.0242245 | 0.2163845 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0715585 |  117.0941189 | 0.0575378 |  26.1345977 | 0.0863336 | 0.9949408 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0342775 |    3.3970436 | 0.0275613 |   3.3276860 | 0.0775239 | 0.9961297 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0446272 |   35.8529280 | 0.0358832 |  16.4183186 | 0.1184808 | 0.9897386 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0765209 |  145.2824230 | 0.0615279 |  25.4482569 | 0.0954615 | 0.9937475 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4938208 |  720.1472615 | 0.3970646 | 106.1236440 | 0.6057737 | 0.9863128 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2149567 | 1389.9165327 | 0.9769056 | 127.0790667 | 1.4419885 | 0.5972857 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1926039 | 1728.2970824 | 0.9589325 | 125.9434405 | 1.4526453 | 0.2024497 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.7933308 |  454.7499141 | 0.6378905 | 144.9629463 | 1.0248355 | 0.2015564 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8635981 |  126.7737810 | 0.6943900 | 189.6930737 | 1.1414185 | 0.2988907 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0315710 |  258.9009619 | 0.9426067 | 178.2937683 | 1.2284717 | 0.6527482 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0644948 |   16.4708926 | 0.0589327 |  12.5989735 | 0.0831347 | 0.9909374 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7211379 |  432.6796634 | 0.6589459 | 124.9060617 | 0.8857862 | 0.0487068 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0593359 |    9.8935341 | 0.0542187 |   9.2787521 | 0.0817223 | 0.9909179 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0186989 |    4.1846033 | 0.0170863 |   3.6829184 | 0.0370646 | 0.9992357 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0075800 |    0.5621961 | 0.0069263 |   0.5661494 | 0.0212505 | 0.9996400 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0969971 |   25.6024216 | 0.0886319 |  28.7642119 | 0.1073638 | 0.9911228 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5659176 |  245.4067959 | 0.5171120 | 117.3383958 | 0.6739297 | 0.9711072 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3084603 | 1247.6869702 | 1.1956166 | 124.8560544 | 1.4751262 | 0.3166048 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4070888 | 1436.9895200 | 1.2857393 | 138.5723510 | 1.6479431 | 0.0447614 |
| healthyverse  |         13 | TBATS                      | Test  | 0.7752907 |  332.1806586 | 0.7084284 | 145.7857894 | 0.9894398 | 0.0335756 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.6849148 |  300.4598233 | 0.6258467 | 105.8597722 | 0.8934695 | 0.1612491 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0275636 |  553.5067505 | 0.7310440 | 138.4995832 | 1.2229018 | 0.3103438 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0545185 |   37.9424803 | 0.0387863 |  15.8746483 | 0.0697323 | 0.9962790 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9148463 |  322.7764226 | 0.6508530 | 156.7695649 | 1.0426132 | 0.1025909 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0515047 |   27.9275306 | 0.0366422 |  14.7297921 | 0.0673942 | 0.9962536 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0144733 |    3.4206815 | 0.0102968 |   3.0239736 | 0.0217108 | 0.9996170 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0851564 |   22.5193343 | 0.0605832 |  23.9141164 | 0.1120334 | 0.9900639 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0637422 |   58.4823210 | 0.0453484 |  21.6542150 | 0.0789620 | 0.9951085 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6622965 |  511.9818886 | 0.4711804 | 117.5162149 | 0.7958010 | 0.9595179 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8646419 |  262.6123978 | 0.6151359 | 100.3032041 | 1.1060978 | 0.6440036 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2313259 |  643.9165242 | 0.8760075 | 136.2737718 | 1.5246640 | 0.1213500 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9065176 |  411.0213063 | 0.6449277 | 159.2205498 | 1.0500872 | 0.0855907 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9124415 |  228.0914741 | 0.6491422 | 150.0556902 | 1.1402071 | 0.2573292 |
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
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR        Test  0.00956 1.16  0.00671 1.16  0.0143 1.00 
    ## 2 healthyR          8 NNAR        Test  0.0126  5.14  0.0153  4.12  0.0239 0.999
    ## 3 healthyR~         7 EARTH       Test  0.0343  3.40  0.0276  3.33  0.0775 0.996
    ## 4 healthyv~         8 NNAR        Test  0.00758 0.562 0.00693 0.566 0.0213 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0145  3.42  0.0103  3.02  0.0217 1.00

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
    ## 1 healthyR.data <tibble [406 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [396 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [346 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [321 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [135 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
