Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
05 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,338
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

The last day in the data set is 2022-02-03 23:53:02, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1591.24
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28338          |
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
| r\_version     |      18802 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18802 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18802 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2378 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-03 | 2021-08-23 |       438 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1526981.00 | 1874980.02 | 357 | 25673.25 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8272.74 |   15804.67 |   1 |   258.00 |   2806 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-03 23:53:02 | 2021-08-23 04:14:28 |     16614 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     47 |        60 |

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
    ## 1 healthyR.data <tibble [409 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [399 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [349 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [324 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [138 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0970837 |  111.721554 | 0.7220651 | 129.898855 | 1.3603357 | 0.5968711 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0566022 |    7.027282 | 0.0372538 |   6.989568 | 0.0680463 | 0.9962771 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0762533 |   97.622900 | 0.7083552 | 131.265856 | 1.3457767 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0565929 |    6.691445 | 0.0372476 |   6.555300 | 0.0710984 | 0.9960742 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0303430 |    3.396392 | 0.0199708 |   3.380230 | 0.0397446 | 0.9987647 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0105586 |    1.197668 | 0.0069494 |   1.235524 | 0.0219349 | 0.9995571 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0622748 |    7.916780 | 0.0409873 |   8.005758 | 0.0835740 | 0.9947120 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6500633 |   70.793405 | 0.4278507 |  92.696328 | 0.7598276 | 0.9936437 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9641850 |  140.490117 | 0.6345954 |  79.254427 | 1.3026022 | 0.5275325 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3326034 |  185.053688 | 0.8770766 | 133.439475 | 1.6761992 | 0.0837366 |
| healthyR.data |         13 | TBATS                      | Test  | 1.1096648 |  106.171355 | 0.7303456 | 128.957371 | 1.3928406 | 0.0842125 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0757689 |   97.571629 | 0.7080364 | 131.312820 | 1.3449138 | 0.1146844 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8586954 | 1109.598267 | 0.9676005 | 170.327215 | 1.0465529 | 0.5095048 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0478202 |   44.636033 | 0.0538851 |  23.466070 | 0.0642556 | 0.9940511 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6111803 |  579.849540 | 0.6886940 | 136.256179 | 0.8022227 | 0.1447912 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0451343 |   43.023530 | 0.0508585 |  30.783201 | 0.0619039 | 0.9940570 |
| healthyR      |          7 | EARTH                      | Test  | 0.0285486 |   18.389153 | 0.0321693 |  13.254851 | 0.0696188 | 0.9932675 |
| healthyR      |          8 | NNAR                       | Test  | 0.0128571 |   10.921954 | 0.0144877 |  13.178905 | 0.0285912 | 0.9988452 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0520012 |   58.663037 | 0.0585963 |  25.819281 | 0.0699225 | 0.9926021 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5519237 | 1001.350197 | 0.6219221 | 136.285913 | 0.6376949 | 0.9635707 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1299677 | 1996.963455 | 1.2732771 | 113.333035 | 1.3827335 | 0.5317053 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0532023 | 2223.251915 | 1.1867759 | 127.927546 | 1.3254429 | 0.2076236 |
| healthyR      |         13 | TBATS                      | Test  | 0.5929548 |  394.281240 | 0.6681569 | 138.048761 | 0.7882223 | 0.2855407 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6664477 |   92.531679 | 0.7509707 | 179.056975 | 0.8882373 | 0.0568500 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.8751171 |  538.324403 | 0.6652153 | 139.038429 | 1.0766816 | 0.4128782 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0785979 |  144.302219 | 0.0597458 |  22.655797 | 0.0952870 | 0.9936483 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.8450800 |  360.741630 | 0.6423829 | 145.690017 | 1.0819384 | 0.1098850 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0771478 |  129.378582 | 0.0586434 |  23.173186 | 0.0923435 | 0.9940715 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0319950 |    4.410177 | 0.0243208 |   4.507242 | 0.0684943 | 0.9971340 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0379039 |   10.048322 | 0.0288124 |  12.420587 | 0.1056563 | 0.9918057 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0851182 |  167.431332 | 0.0647022 |  24.019112 | 0.1046193 | 0.9923122 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5188357 |  585.617207 | 0.3943901 | 103.471886 | 0.6263103 | 0.9806403 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1778426 | 1115.597650 | 0.8953305 | 118.578529 | 1.4714890 | 0.5198287 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2509988 | 1386.814608 | 0.9509397 | 121.969446 | 1.6150666 | 0.1050440 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.8844974 |  398.881564 | 0.6723457 | 143.144374 | 1.1253652 | 0.0547579 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9130818 |  348.437623 | 0.6940740 | 156.823655 | 1.1526758 | 0.3442514 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0555433 |  277.341268 | 0.9685973 | 183.018287 | 1.2481004 | 0.5292511 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0645008 |   14.545757 | 0.0591878 |  11.766726 | 0.0830207 | 0.9904084 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7687117 |  473.197611 | 0.7053922 | 127.449638 | 0.9375712 | 0.0273193 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0581088 |   10.394766 | 0.0533223 |  10.594216 | 0.0812210 | 0.9903920 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0190500 |    5.393696 | 0.0174808 |   4.394641 | 0.0373990 | 0.9990889 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0101168 |    2.502575 | 0.0092835 |   3.020404 | 0.0271514 | 0.9992963 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1041428 |   24.608446 | 0.0955644 |  28.595035 | 0.1160147 | 0.9887074 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5896095 |  270.186493 | 0.5410429 | 113.154093 | 0.6936345 | 0.9636774 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2575133 | 1427.384128 | 1.1539308 | 120.750831 | 1.3875669 | 0.2943046 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3712247 | 1618.697687 | 1.2582757 | 143.284535 | 1.6013308 | 0.0300902 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8222102 |  313.455656 | 0.7544840 | 138.406490 | 1.0376594 | 0.0500164 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7595469 |  245.467793 | 0.6969824 | 119.466770 | 0.9776253 | 0.1519291 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0235442 |  500.995479 | 0.7313410 | 154.012207 | 1.2056966 | 0.1915959 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0600040 |   43.417783 | 0.0428740 |  16.780773 | 0.0750221 | 0.9958901 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9395924 |  275.588111 | 0.6713559 | 169.762676 | 1.0643286 | 0.0547585 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0546018 |   30.342944 | 0.0390140 |  15.158337 | 0.0698520 | 0.9960641 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0208383 |    6.754283 | 0.0148894 |   5.156392 | 0.0358976 | 0.9990362 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0544275 |   11.581514 | 0.0388894 |  14.799194 | 0.0859683 | 0.9949753 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0646653 |   53.838768 | 0.0462045 |  19.498592 | 0.0842900 | 0.9943088 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9971412 |  881.152439 | 0.7124756 | 148.194136 | 1.1251429 | 0.8901917 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2102242 |  515.120048 | 0.8647272 | 127.268731 | 1.4830627 | 0.3934448 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3276967 |  456.829218 | 0.9486635 | 140.487951 | 1.7014911 | 0.0496587 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9642344 |  272.018424 | 0.6889630 | 168.845563 | 1.0885503 | 0.0360911 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9467709 |  244.868275 | 0.6764850 | 157.390419 | 1.1644072 | 0.2966422 |
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
    ## 1 healthyR.~         8 NNAR        Test  0.0106  1.20 0.00695  1.24 0.0219 1.00 
    ## 2 healthyR           8 NNAR        Test  0.0129 10.9  0.0145  13.2  0.0286 0.999
    ## 3 healthyR.~         7 EARTH       Test  0.0320  4.41 0.0243   4.51 0.0685 0.997
    ## 4 healthyve~         8 NNAR        Test  0.0101  2.50 0.00928  3.02 0.0272 0.999
    ## 5 healthyR.~         7 EARTH       Test  0.0208  6.75 0.0149   5.16 0.0359 0.999

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
    ## 1 healthyR.data <tibble [409 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [399 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [349 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [324 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [138 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
