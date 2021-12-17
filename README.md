Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
17 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,238
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

The last day in the data set is 2021-12-15 20:11:33, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -387.55
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25238          |
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
| r\_version     |      16906 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      16906 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      16906 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2141 |           0.92 |   2 |   2 |     0 |        98 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-15 | 2021-08-01 |       388 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|--------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1534135 | 1879745.68 | 357 | 26178.5 | 238637 | 3246342 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8143 |   15382.35 |   1 |   232.0 |   2930 |    8310 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-15 20:11:33 | 2021-08-01 05:27:07 |     14722 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     29 |        60 |

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
    ## 1 healthyR.data <tibble [359 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [349 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [300 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [274 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [89 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9977232 |  71.070793 | 0.7278916 |  97.628493 | 1.4560760 | 0.3034288 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2508425 |   9.438612 | 0.1830028 |  10.739459 | 0.7704064 | 0.7431447 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4378150 | 102.816144 | 1.0489616 | 195.871807 | 1.8862245 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2599265 |  11.338269 | 0.1896300 |  12.319740 | 0.7618845 | 0.7425277 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2483624 |   8.699269 | 0.1811935 |   9.086859 | 0.6468670 | 0.7793694 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2480998 |   9.086401 | 0.1810019 |   8.860480 | 0.6322750 | 0.7894518 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2492546 |   9.665616 | 0.1818443 |  10.917556 | 0.7650456 | 0.7479523 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7360685 |  48.985086 | 0.5370007 |  68.471991 | 1.1506009 | 0.7319624 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7843296 |  85.337918 | 0.5722097 |  70.615952 | 1.0929757 | 0.5305629 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5098204 | 123.424434 | 1.1014934 | 128.512501 | 1.9657795 | 0.1059116 |
| healthyR.data |         13 | BATS                       | Test  | 1.4001386 |  95.137147 | 1.0214748 | 176.953271 | 1.8729849 | 0.0191781 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4707196 | 105.896948 | 1.0729673 | 191.720971 | 1.9188244 | 0.0180812 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6723235 |  69.151009 | 0.6503532 |  77.820478 | 0.9086589 | 0.7537360 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1195226 |   7.963342 | 0.1156169 |   8.031760 | 0.2565023 | 0.9634137 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.2849769 | 121.092349 | 1.2429861 | 166.677140 | 1.5842206 | 0.0221074 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1220319 |   8.442996 | 0.1180441 |   8.450879 | 0.2543901 | 0.9633970 |
| healthyR      |          7 | EARTH                      | Test  | 0.0787554 |   3.798965 | 0.0761818 |   3.597942 | 0.1690400 | 0.9912980 |
| healthyR      |          8 | NNAR                       | Test  | 0.1678131 |   8.167168 | 0.1623293 |   7.270433 | 0.3572142 | 0.9803223 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1155017 |   7.616050 | 0.1117273 |   7.713027 | 0.2529654 | 0.9654617 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3903770 |  31.672544 | 0.3776201 |  41.946317 | 0.5686158 | 0.9702991 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7646979 | 119.735965 | 0.7397090 |  68.412249 | 1.0140414 | 0.6882804 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4149652 | 170.060912 | 1.3687267 | 141.159033 | 1.6438185 | 0.2450006 |
| healthyR      |         13 | TBATS                      | Test  | 1.2704559 | 119.466554 | 1.2289397 | 168.357713 | 1.5641678 | 0.0230861 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3029270 | 112.641366 | 1.2603497 | 176.242894 | 1.5989345 | 0.0450100 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0700610 |  83.047563 | 0.8334569 | 116.582407 | 1.3448559 | 0.6524877 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1603510 |   9.622191 | 0.1248954 |   9.721032 | 0.3463856 | 0.9490557 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.6300754 | 128.832654 | 1.2696451 | 167.293370 | 1.9120274 | 0.2638127 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1603482 |   9.638285 | 0.1248931 |   9.719562 | 0.3447674 | 0.9491131 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1460794 |   7.261395 | 0.1137794 |   6.974825 | 0.2396846 | 0.9731351 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2000883 |   9.538736 | 0.1558462 |   8.681820 | 0.3769632 | 0.9511882 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1562360 |  10.091585 | 0.1216903 |  10.031546 | 0.3422494 | 0.9511268 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7020271 |  47.303589 | 0.5468000 |  61.273948 | 0.9393171 | 0.8983648 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0464167 | 146.352377 | 0.8150407 |  89.666546 | 1.2317372 | 0.6571729 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7565944 | 209.162468 | 1.3681892 | 138.834951 | 2.1024426 | 0.1891420 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.5387842 | 117.850605 | 1.1985396 | 167.721785 | 1.8234694 | 0.3033057 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6607097 | 118.231143 | 1.2935058 | 174.511549 | 1.9751821 | 0.1323941 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8548055 |  85.622897 | 0.7872905 | 124.302409 | 1.0876434 | 0.7976918 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1155124 |  12.899150 | 0.1063889 |  13.884335 | 0.2137147 | 0.9726580 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.4008802 | 198.738178 | 1.2902346 | 176.411727 | 1.6526607 | 0.3355972 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1142590 |  13.768109 | 0.1052345 |  16.382743 | 0.2130651 | 0.9727461 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1216628 |  10.753571 | 0.1120535 |  10.599122 | 0.1898039 | 0.9883956 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0751261 |   6.581387 | 0.0691924 |   5.862512 | 0.1643326 | 0.9874970 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1120245 |  16.021301 | 0.1031765 |  17.099676 | 0.2057631 | 0.9739526 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5846326 |  87.642609 | 0.5384566 |  84.833362 | 0.7361678 | 0.9707717 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7905450 | 260.868322 | 0.7281054 |  86.256761 | 0.9903681 | 0.6786019 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5068992 | 250.743210 | 1.3878800 | 149.999480 | 1.7908115 | 0.1866650 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3711132 | 195.609925 | 1.2628187 | 175.203906 | 1.6309335 | 0.3928048 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4827008 | 228.256495 | 1.3655928 | 180.206468 | 1.7468626 | 0.0383458 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.9093230 |  80.283201 | 0.8038339 | 119.516448 | 1.2035330 | 0.4585340 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1138118 |   9.303480 | 0.1006087 |   9.141002 | 0.2143114 | 0.9747849 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.3194302 | 132.528157 | 1.1663653 | 184.555134 | 1.5866551 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1298917 |  12.817891 | 0.1148232 |  12.123576 | 0.2119089 | 0.9745133 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1027385 |   5.178356 | 0.0908200 |   5.495298 | 0.2542695 | 0.9740142 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3561272 |  26.921876 | 0.3148135 |  34.339036 | 0.5729016 | 0.9283157 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1321404 |  12.684625 | 0.1168110 |  11.961861 | 0.2105730 | 0.9748992 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5312145 |  35.447786 | 0.4695892 |  45.518541 | 0.8416507 | 0.8033413 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9726983 | 189.137511 | 0.8598572 |  84.349735 | 1.3512198 | 0.4953081 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4044560 | 198.348285 | 1.2415274 | 130.869334 | 1.7299838 | 0.1287340 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.3274415 | 138.897520 | 1.1734472 | 174.463534 | 1.5893517 | 0.0782006 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.3293761 | 134.331037 | 1.1751574 | 183.864977 | 1.5968878 | 0.0419663 |
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
    ##   package   .model_id .model_desc    .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>         <int> <chr>          <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR~         8 NNAR           Test  0.248   9.09 0.181   8.86 0.632 0.789
    ## 2 healthyR          7 EARTH          Test  0.0788  3.80 0.0762  3.60 0.169 0.991
    ## 3 healthyR~         7 EARTH          Test  0.146   7.26 0.114   6.97 0.240 0.973
    ## 4 healthyv~         8 NNAR           Test  0.0751  6.58 0.0692  5.86 0.164 0.987
    ## 5 healthyR~         9 PROPHET W REG~ Test  0.132  12.7  0.117  12.0  0.211 0.975

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
    ## 1 healthyR.data <tibble [359 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [349 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [300 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [274 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [89 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
