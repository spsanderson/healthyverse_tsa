Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
07 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,393
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

The last day in the data set is 2022-02-05 23:16:05, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1638.63
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28393          |
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
| r\_version     |      18804 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18804 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18804 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2390 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-05 | 2021-08-23 |       440 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1527686.07 | 1875070.1 | 357 | 26285 | 271098 | 3247952 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8273.99 |   15795.5 |   1 |   258 |   2816 |    8298 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-05 23:16:05 | 2021-08-23 08:32:25 |     16651 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 54M 48S |        60 |

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
    ## 1 healthyR.data <tibble [411 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [401 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [351 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [326 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [140 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1555543 |  117.5533359 | 0.7262030 | 135.5710518 | 1.4048573 | 0.6396717 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0533086 |    6.4217752 | 0.0335016 |   6.4826759 | 0.0644057 | 0.9963353 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.1407356 |  105.1304056 | 0.7168903 | 137.5889534 | 1.3895514 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0522853 |    5.9106315 | 0.0328585 |   5.8493009 | 0.0679862 | 0.9961787 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0293815 |    3.1970056 | 0.0184647 |   3.1570768 | 0.0382518 | 0.9987442 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0064570 |    0.5862950 | 0.0040579 |   0.5871893 | 0.0108342 | 0.9998834 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0617620 |    7.7145488 | 0.0388141 |   7.8368987 | 0.0832146 | 0.9944512 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6527100 |   70.1591393 | 0.4101927 |  94.7955905 | 0.7536693 | 0.9936131 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9160688 |  131.5366613 | 0.5756994 |  75.7685175 | 1.2420053 | 0.5592770 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3327902 |  179.6743378 | 0.8375861 | 127.1513161 | 1.7000398 | 0.1161446 |
| healthyR.data |         13 | TBATS                      | Test  | 1.1612509 |  112.5409703 | 0.7297830 | 137.2380132 | 1.4122136 | 0.0753147 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.1404228 |  105.0965839 | 0.7166937 | 137.6194511 | 1.3889995 | 0.0245031 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8923644 | 2148.6948443 | 1.0192527 | 168.6596518 | 1.0707294 | 0.5480096 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0518409 |   78.0673036 | 0.0592123 |  24.4350011 | 0.0670112 | 0.9940600 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6237933 |  835.1772724 | 0.7124925 | 128.6821096 | 0.8042783 | 0.2548039 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0484731 |   72.1584059 | 0.0553657 |  22.8885271 | 0.0638036 | 0.9940879 |
| healthyR      |          7 | EARTH                      | Test  | 0.0285585 |   30.5505669 | 0.0326193 |  24.8574166 | 0.0688569 | 0.9934520 |
| healthyR      |          8 | NNAR                       | Test  | 0.0060814 |    6.1507015 | 0.0069461 |   8.1853105 | 0.0144533 | 0.9996991 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0548715 |  108.8912947 | 0.0626738 |  22.8942532 | 0.0714701 | 0.9927305 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5263504 | 1506.8061892 | 0.6011939 | 124.8921392 | 0.6158277 | 0.9653724 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1207604 | 2590.2259331 | 1.2801251 | 106.9759976 | 1.3724412 | 0.5827232 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 0.9982038 | 2587.0440231 | 1.1401417 | 123.1837567 | 1.3114188 | 0.2679095 |
| healthyR      |         13 | TBATS                      | Test  | 0.5731534 |  494.0243542 | 0.6546519 | 124.1481631 | 0.7560029 | 0.3670862 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6971459 |  307.2863819 | 0.7962754 | 177.6482445 | 0.8934614 | 0.0815668 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0556489 | 1564.1202015 | 0.8338875 | 135.3742281 | 1.3352416 | 0.5762643 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0779607 |  331.8390092 | 0.0615834 |  21.8588527 | 0.0946609 | 0.9942615 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.0284620 |  879.8986384 | 0.8124118 | 149.0424576 | 1.2752289 | 0.1200139 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0767581 |  291.5924837 | 0.0606334 |  22.6285722 | 0.0916200 | 0.9946367 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0297021 |    6.6981230 | 0.0234626 |   5.7832579 | 0.0643365 | 0.9973008 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0483146 |   14.9792876 | 0.0381651 |  12.9738535 | 0.1907981 | 0.9817633 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0846659 |  395.9388484 | 0.0668800 |  23.0873984 | 0.1039290 | 0.9933136 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5817283 | 1433.0625545 | 0.4595240 | 107.3797905 | 0.7143362 | 0.9825113 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1380844 | 2383.8512835 | 0.8990057 | 116.2646710 | 1.4186700 | 0.5735751 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3519542 | 3017.7383096 | 1.0679477 | 131.2016844 | 1.6696715 | 0.1321307 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9400429 |  760.6827889 | 0.7425670 | 138.8361873 | 1.1939510 | 0.3281995 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.0774186 | 1544.4766495 | 0.8510840 | 146.4388221 | 1.3487320 | 0.4007826 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2125846 |  160.0997721 | 1.1321079 | 179.2233836 | 1.4127667 | 0.5064840 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0699970 |    9.1562040 | 0.0653514 |   8.6776627 | 0.0904862 | 0.9905969 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9199865 |  112.9474181 | 0.8589289 | 142.7930291 | 1.1126031 | 0.0777644 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0652042 |    8.3991098 | 0.0608768 |   7.9667010 | 0.0906610 | 0.9905647 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0224299 |    2.0057580 | 0.0209413 |   2.0236924 | 0.0439310 | 0.9989248 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0123745 |    0.6817694 | 0.0115533 |   0.6904270 | 0.0331708 | 0.9992437 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1120743 |   23.3929676 | 0.1046362 |  25.7986307 | 0.1218963 | 0.9898665 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6479381 |   88.6213594 | 0.6049358 | 104.5087707 | 0.7658970 | 0.9638781 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1998771 |  284.5341730 | 1.1202438 | 108.2434625 | 1.3385951 | 0.4580745 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3723460 |  286.8665867 | 1.2812663 | 135.7109942 | 1.6214410 | 0.0992273 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9601044 |  102.9959763 | 0.8963842 | 143.8856915 | 1.1734900 | 0.1476290 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8944169 |   96.0102541 | 0.8350563 | 124.8125219 | 1.1295431 | 0.2246196 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1517334 | 2761.2290686 | 0.8481471 | 153.4032587 | 1.3451569 | 0.4847125 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0611832 |  136.1937547 | 0.0450559 |  24.8211703 | 0.0762726 | 0.9954298 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9751165 | 1133.6108265 | 0.7180848 | 171.0110184 | 1.1080559 | 0.0699446 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0538178 |   62.8568854 | 0.0396319 |  17.4382038 | 0.0694892 | 0.9956244 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0129019 |    7.4399383 | 0.0095011 |  11.4065500 | 0.0183815 | 0.9996683 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0607455 |   56.0636011 | 0.0447335 |  22.4964741 | 0.1018475 | 0.9916464 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0672778 |  197.1443421 | 0.0495440 |  27.6012255 | 0.0867024 | 0.9937490 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9932211 | 3249.5656074 | 0.7314172 | 155.2035646 | 1.1139170 | 0.8911642 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1754162 |  868.5388454 | 0.8655874 | 133.6913776 | 1.4122842 | 0.4182579 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2670855 | 1578.0558999 | 0.9330935 | 135.8052503 | 1.6615222 | 0.0649526 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0344697 | 1639.4651777 | 0.7617931 | 164.4178838 | 1.1513039 | 0.1138734 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0278171 | 1325.5929191 | 0.7568940 | 160.9162591 | 1.2313171 | 0.1920693 |
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
    ##   package  .model_id .model_desc .type     mae  mape    mase  smape   rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl>  <dbl>  <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.00646 0.586 0.00406  0.587 0.0108 1.00 
    ## 2 healthyR         8 NNAR        Test  0.00608 6.15  0.00695  8.19  0.0145 1.00 
    ## 3 healthy~         7 EARTH       Test  0.0297  6.70  0.0235   5.78  0.0643 0.997
    ## 4 healthy~         8 NNAR        Test  0.0124  0.682 0.0116   0.690 0.0332 0.999
    ## 5 healthy~         7 EARTH       Test  0.0129  7.44  0.00950 11.4   0.0184 1.00

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
    ## 1 healthyR.data <tibble [411 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [401 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [351 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [326 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [140 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
