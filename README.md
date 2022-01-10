Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
10 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,386
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

The last day in the data set is 2022-01-08 21:46:13, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -965.13
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26386          |
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
| r\_version     |      17648 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17648 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17648 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2223 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-08 | 2021-08-04 |       412 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |      p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|---------:|--------:|--------:|:------|
| size           |          0 |              1 | 1533519.43 | 1879092.36 | 357 | 17597.00 | 238779.5 | 3246657 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8092.31 |   15328.17 |   1 |   204.25 |   2823.0 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-08 21:46:13 | 2021-08-04 16:17:52 |     15399 |

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
    ## 1 healthyR.data <tibble [383 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [373 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [323 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [298 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [112 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0557994 |  379.900980 | 1.0003921 | 109.576924 | 1.2919364 | 0.0595374 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1704078 |   48.790172 | 0.1614649 |  43.834406 | 0.2117360 | 0.9422817 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4693388 |  608.251427 | 1.3922294 | 123.474418 | 1.6784332 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.1069364 |   36.721214 | 0.1013245 |  29.099533 | 0.1248254 | 0.9919634 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0218117 |    8.806024 | 0.0206670 |   6.801231 | 0.0303363 | 0.9992674 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0229589 |    4.579423 | 0.0217540 |   4.563949 | 0.0407543 | 0.9975334 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.1144482 |   37.637188 | 0.1084421 |  29.664873 | 0.1380820 | 0.9901604 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5021648 |  228.979277 | 0.4758117 |  92.499166 | 0.5829177 | 0.9907098 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1526529 |  554.497760 | 1.0921629 | 109.754039 | 1.4463234 | 0.4967547 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7701655 |  369.789250 | 1.6772690 | 132.584043 | 2.1694132 | 0.1424401 |
| healthyR.data |         13 | BATS                       | Test  | 1.6032028 |  657.846170 | 1.5190684 | 126.830915 | 1.8025160 | 0.1659411 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4680107 |  607.717572 | 1.3909710 | 123.434950 | 1.6772954 | 0.0177304 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 1.3997119 |  516.147344 | 1.4743679 | 159.285271 | 1.4962782 | 0.2862945 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0362382 |   13.745496 | 0.0381710 |  16.866260 | 0.0425564 | 0.9989378 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.5670329 |  659.836756 | 1.6506133 | 159.511473 | 1.6780155 | 0.0266962 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0397086 |   17.353520 | 0.0418265 |  18.547030 | 0.0482018 | 0.9989871 |
| healthyR      |          7 | EARTH                      | Test  | 0.0181940 |    5.392074 | 0.0191644 |   5.078396 | 0.0211193 | 0.9993237 |
| healthyR      |          8 | NNAR                       | Test  | 0.0741566 |    9.797687 | 0.0781119 |   8.632896 | 0.2611175 | 0.9290965 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0434644 |   18.594429 | 0.0457827 |  19.359924 | 0.0570638 | 0.9979782 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4380181 |  195.334748 | 0.4613806 | 123.510128 | 0.4981183 | 0.9949561 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3644690 |  697.024751 | 1.4372452 | 129.861619 | 1.5988920 | 0.4927407 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.9024998 |  566.622595 | 2.0039729 | 162.600621 | 2.2951202 | 0.2276721 |
| healthyR      |         13 | TBATS                      | Test  | 1.5980091 |  676.671495 | 1.6832417 | 160.514095 | 1.6986790 | 0.0438249 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.5338572 |  683.764742 | 1.6156681 | 157.275185 | 1.6619856 | 0.0142497 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9605022 |  356.408877 | 0.6070386 | 110.270135 | 1.2156359 | 0.1611767 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0482632 |   23.877577 | 0.0305024 |  12.727621 | 0.0565154 | 0.9976800 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.6969194 |  741.880966 | 1.0724552 | 130.034038 | 1.9060333 | 0.1108326 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0703198 |   16.248015 | 0.0444422 |  15.588947 | 0.0862625 | 0.9978285 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0295295 |    2.110298 | 0.0186627 |   2.080812 | 0.0630617 | 0.9971622 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0565487 |    5.328958 | 0.0357389 |   5.175192 | 0.1612744 | 0.9771211 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0959021 |   28.708065 | 0.0606102 |  27.629872 | 0.1163763 | 0.9964955 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4052350 |  148.256119 | 0.2561090 |  86.491854 | 0.4774925 | 0.9886172 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3998377 |  466.906288 | 0.8846992 | 128.084774 | 1.5817280 | 0.4443197 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 2.0382101 |  938.210152 | 1.2881514 | 139.883768 | 2.4011896 | 0.0804148 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.8000486 |  795.474909 | 1.1376330 | 131.180387 | 2.0216920 | 0.0519832 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6263548 |  716.451065 | 1.0278583 | 125.742591 | 1.9111402 | 0.0103010 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.9659024 | 1148.976526 | 1.9610706 | 174.039590 | 2.0730649 | 0.2774787 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0795261 |   32.336736 | 0.0793306 |  25.967233 | 0.0937613 | 0.9969494 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.7252388 |  967.731581 | 1.7209985 | 171.402001 | 1.8361691 | 0.2370987 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0665336 |   25.267555 | 0.0663700 |  20.224674 | 0.0808520 | 0.9972843 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0080973 |    3.906143 | 0.0080774 |   3.679496 | 0.0119897 | 0.9998415 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0071192 |    2.197766 | 0.0071017 |   2.147611 | 0.0105090 | 0.9998350 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0578762 |   23.964285 | 0.0577339 |  18.326194 | 0.0707841 | 0.9969445 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5542854 |  321.203223 | 0.5529231 | 134.938818 | 0.6257637 | 0.9736956 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1789431 |  765.423207 | 1.1760455 | 129.220379 | 1.3390716 | 0.5487416 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.9735613 | 1156.851809 | 1.9687107 | 166.612838 | 2.3093900 | 0.2384048 |
| healthyverse  |         13 | TBATS                      | Test  | 1.2715778 |  688.327596 | 1.2684525 | 163.050162 | 1.4292858 | 0.1139019 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.6728195 |  929.347657 | 1.6687081 | 169.347495 | 1.8190182 | 0.0098758 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.7639429 |  386.430296 | 1.2323052 | 135.182899 | 1.9474987 | 0.4217582 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1212154 |   21.534178 | 0.0846821 |  22.705396 | 0.1413945 | 0.9961865 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2434387 |  278.124616 | 0.8686766 | 119.058658 | 1.5415253 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1188262 |   21.434875 | 0.0830130 |  22.984157 | 0.1377581 | 0.9969285 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0246793 |    3.471438 | 0.0172412 |   3.431595 | 0.0350847 | 0.9987151 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2387031 |   35.596103 | 0.1667600 |  42.357899 | 0.3320659 | 0.9416701 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1579436 |   27.768838 | 0.1103407 |  28.557776 | 0.1816250 | 0.9927480 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.0845504 |  244.671992 | 0.7576759 | 123.773080 | 1.2176778 | 0.8592817 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1913921 |  240.775850 | 0.8323164 | 119.955550 | 1.3320164 | 0.5414522 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6104772 |  363.283643 | 1.1250928 | 128.795228 | 1.9098999 | 0.1382189 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.2743547 |  288.030499 | 0.8902748 | 121.396522 | 1.5446320 | 0.0418268 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.4080645 |  315.708671 | 0.9836856 | 123.421756 | 1.7002454 | 0.0004803 |
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
    ## 1 healthyR~         7 EARTH       Test  0.0218   8.81 0.0207   6.80 0.0303 0.999
    ## 2 healthyR          7 EARTH       Test  0.0182   5.39 0.0192   5.08 0.0211 0.999
    ## 3 healthyR~         2 REGRESSION  Test  0.0483  23.9  0.0305  12.7  0.0565 0.998
    ## 4 healthyv~         8 NNAR        Test  0.00712  2.20 0.00710  2.15 0.0105 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0247   3.47 0.0172   3.43 0.0351 0.999

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
    ## 1 healthyR.data <tibble [383 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [373 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [323 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [298 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [112 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
