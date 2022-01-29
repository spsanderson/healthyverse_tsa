Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
29 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,823
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

The last day in the data set is 2022-01-27 22:33:31, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1421.92
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27823          |
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
| r\_version     |      18498 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18498 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18498 |           0.34 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2342 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-27 | 2021-08-17 |       431 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530399.43 | 1876583.73 | 357 | 23254 | 260742 | 3247926 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8199.54 |   15659.04 |   1 |   242 |   2802 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-27 22:33:31 | 2021-08-17 09:05:08 |     16304 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 53M 31S |        60 |

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
    ## 1 healthyR.data <tibble [402 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [392 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [342 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [317 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [131 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8046665 |  404.845610 | 0.5798794 | 114.923548 | 1.0149598 | 0.3133644 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0997127 |   20.551934 | 0.0718575 |  24.614241 | 0.1607325 | 0.9732105 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8000823 |  173.603554 | 0.5765758 | 129.120976 | 0.9915955 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0671980 |   21.434225 | 0.0484259 |  17.654993 | 0.0831590 | 0.9938463 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0325159 |   12.199417 | 0.0234324 |   7.686009 | 0.0547200 | 0.9970643 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0084780 |    1.359006 | 0.0061097 |   1.381024 | 0.0137891 | 0.9998171 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0661582 |   19.376952 | 0.0476766 |  18.337234 | 0.0905700 | 0.9923525 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4569713 |  207.970693 | 0.3293144 |  81.606836 | 0.5586585 | 0.9975367 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2025657 |  727.581349 | 0.8666237 | 126.350321 | 1.4340079 | 0.3258810 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4406591 |  737.719142 | 1.0382047 | 144.886806 | 1.7360890 | 0.0123446 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8210075 |  219.801248 | 0.5916555 | 116.383611 | 1.0580980 | 0.0054713 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.7999684 |  173.087689 | 0.5764937 | 129.430512 | 0.9906111 | 0.0841159 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6896618 |  563.089377 | 0.8356389 | 154.951738 | 0.8075638 | 0.5905597 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0366175 |   24.641826 | 0.0443681 |  19.292484 | 0.0430198 | 0.9987615 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.4584908 |  212.547722 | 0.5555371 | 118.325460 | 0.5849495 | 0.1818045 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0349381 |   24.974371 | 0.0423333 |  22.868472 | 0.0402676 | 0.9989451 |
| healthyR      |          7 | EARTH                      | Test  | 0.0171420 |    9.909301 | 0.0207704 |   8.431317 | 0.0193870 | 0.9995131 |
| healthyR      |          8 | NNAR                       | Test  | 0.0058544 |    3.076579 | 0.0070936 |   2.809757 | 0.0099895 | 0.9997803 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0390837 |   27.426086 | 0.0473564 |  20.382310 | 0.0485260 | 0.9979962 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4359924 |  426.345968 | 0.5282765 | 131.813239 | 0.4810361 | 0.9958861 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2179121 | 1345.589653 | 1.4757010 | 131.303710 | 1.4397543 | 0.5816714 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0902561 | 1406.125315 | 1.3210248 | 128.812309 | 1.2647430 | 0.3485279 |
| healthyR      |         13 | TBATS                      | Test  | 0.4855689 |  235.243268 | 0.5883467 | 129.140889 | 0.6224809 | 0.3141059 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.5358311 |  106.393523 | 0.6492476 | 196.987199 | 0.6723081 | 0.0011008 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7401822 |  305.431036 | 0.5899521 | 137.383269 | 0.8935599 | 0.2224566 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0701363 |  107.060383 | 0.0559012 |  27.791138 | 0.0812446 | 0.9949679 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7196561 |  356.102481 | 0.5735921 | 142.262121 | 0.9168681 | 0.1571991 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0701747 |   92.807486 | 0.0559318 |  28.376365 | 0.0808860 | 0.9953332 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0324666 |    3.393218 | 0.0258771 |   3.322124 | 0.0756436 | 0.9958249 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0547822 |   23.724650 | 0.0436634 |  20.750681 | 0.1396462 | 0.9853081 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0757965 |  113.073048 | 0.0604126 |  28.367930 | 0.0942260 | 0.9938497 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4583569 |  518.562538 | 0.3653271 | 104.142913 | 0.5237365 | 0.9801045 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2564093 |  970.221096 | 1.0014039 | 132.332421 | 1.4900152 | 0.5066259 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3143737 | 1204.775293 | 1.0476036 | 142.556530 | 1.5413068 | 0.1435682 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6994053 |  326.040441 | 0.5574515 | 130.027886 | 0.8627225 | 0.2573375 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8022094 |  425.836790 | 0.6393900 | 140.299365 | 0.9964758 | 0.0262037 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8080040 |  288.529997 | 0.7741697 | 166.767208 | 0.9421006 | 0.4581417 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0571873 |   23.698129 | 0.0547927 |  15.354311 | 0.0665107 | 0.9967322 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.5995685 |  327.465659 | 0.5744622 | 126.080465 | 0.6794597 | 0.0650607 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0505636 |   15.182239 | 0.0484463 |  12.508148 | 0.0583663 | 0.9971255 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0103432 |    3.531922 | 0.0099101 |   3.261945 | 0.0152611 | 0.9997061 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0036174 |    1.255659 | 0.0034659 |   1.229310 | 0.0052269 | 0.9999518 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0849379 |   29.152565 | 0.0813812 |  31.337654 | 0.0947573 | 0.9961392 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5125382 |  233.382064 | 0.4910762 | 127.370042 | 0.5718756 | 0.9907523 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3063212 |  940.637584 | 1.2516205 | 128.834519 | 1.4833902 | 0.3198787 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3467891 | 1050.736049 | 1.2903939 | 140.919684 | 1.5181169 | 0.0624385 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5809996 |  171.732874 | 0.5566709 | 128.299718 | 0.7210755 | 0.1254477 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.5800361 |  177.157165 | 0.5557478 | 116.509075 | 0.7229454 | 0.0000158 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0702504 |  517.658411 | 0.6968281 | 131.804040 | 1.2995055 | 0.4321001 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0537791 |   30.598673 | 0.0350150 |  17.576931 | 0.0664070 | 0.9981298 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9810049 |  376.643891 | 0.6387213 | 141.865717 | 1.1530732 | 0.0213710 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0497511 |   23.473021 | 0.0323924 |  14.871018 | 0.0612281 | 0.9982676 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0138109 |    2.948561 | 0.0089921 |   2.720458 | 0.0200656 | 0.9997185 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0762282 |   12.728960 | 0.0496314 |  16.696637 | 0.1166436 | 0.9881958 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0657584 |   48.619331 | 0.0428145 |  22.526198 | 0.0841949 | 0.9968604 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.2432994 |  801.266599 | 0.8094983 | 145.567899 | 1.3316460 | 0.8719516 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1528558 |  504.721593 | 0.7506115 | 120.572715 | 1.3689365 | 0.4144630 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4554954 |  653.258594 | 0.9476568 | 150.721725 | 1.6395899 | 0.0530867 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9770401 |  351.379424 | 0.6361399 | 139.502673 | 1.1695054 | 0.0102733 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0274576 |  399.559493 | 0.6689662 | 133.348675 | 1.2530419 | 0.0086336 |
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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.00848  1.36 0.00611  1.38 0.0138  1.00 
    ## 2 healthyR         8 NNAR        Test  0.00585  3.08 0.00709  2.81 0.00999 1.00 
    ## 3 healthy~         7 EARTH       Test  0.0325   3.39 0.0259   3.32 0.0756  0.996
    ## 4 healthy~         8 NNAR        Test  0.00362  1.26 0.00347  1.23 0.00523 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0138   2.95 0.00899  2.72 0.0201  1.00

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
    ## 1 healthyR.data <tibble [402 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [392 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [342 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [317 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [131 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
