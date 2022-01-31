Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
31 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,929
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

The last day in the data set is 2022-01-29 22:13:13, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1469.58
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27929          |
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
| r\_version     |      18552 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18552 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18552 |           0.34 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2350 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-29 | 2021-08-18 |       433 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1529565.29 | 1876243.11 | 357 | 23389 | 271097 | 3247929 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8206.61 |   15697.29 |   1 |   221 |   2782 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-29 22:13:13 | 2021-08-18 22:17:17 |     16371 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 54M 13S |        60 |

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
    ## 1 healthyR.data <tibble [404 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [394 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [344 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [319 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [133 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9950759 |  878.7999920 | 0.6987948 | 122.7145486 | 1.2426365 | 0.3764625 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0972980 |   22.6247126 | 0.0683278 |  23.2966899 | 0.1589051 | 0.9750321 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9098611 |  340.0745004 | 0.6389524 | 123.8422720 | 1.1727566 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0578872 |   11.0310308 | 0.0406515 |  12.1209256 | 0.0725378 | 0.9947722 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0283771 |   16.5339013 | 0.0199279 |   8.0978305 | 0.0380172 | 0.9987468 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0095748 |   15.7371670 | 0.0067240 |   6.0187709 | 0.0133693 | 0.9998156 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0587692 |   10.1607941 | 0.0412709 |  10.9397273 | 0.0828111 | 0.9932676 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5652018 |  341.7911818 | 0.3969146 |  95.7521998 | 0.6798616 | 0.9932461 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1440732 | 1054.8394323 | 0.8034286 | 110.1171282 | 1.4277029 | 0.4155664 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3281793 |  803.0085043 | 0.9327175 | 125.2521137 | 1.6845646 | 0.0663045 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9442902 |  592.7219393 | 0.6631304 | 117.0872554 | 1.2380752 | 0.0724946 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9094955 |  339.8312682 | 0.6386957 | 123.8855458 | 1.1720252 | 0.0908760 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6818744 |  740.5213875 | 0.8245628 | 156.8438784 | 0.8055456 | 0.5456679 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0388341 |   35.4455067 | 0.0469605 |  19.7545078 | 0.0459223 | 0.9989144 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.4700520 |  486.7278978 | 0.5684146 | 121.0340604 | 0.5776793 | 0.1753442 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0363734 |   35.3445585 | 0.0439849 |  25.7851080 | 0.0421620 | 0.9990793 |
| healthyR      |          7 | EARTH                      | Test  | 0.0175629 |   13.7946804 | 0.0212381 |  10.3575036 | 0.0197533 | 0.9995144 |
| healthyR      |          8 | NNAR                       | Test  | 0.0080267 |    6.6768445 | 0.0097063 |   5.3823222 | 0.0151155 | 0.9994651 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0414021 |   40.5165649 | 0.0500659 |  21.3190221 | 0.0506841 | 0.9982135 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4224013 |  576.4293608 | 0.5107926 | 126.8288702 | 0.4690606 | 0.9967466 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2240436 | 1954.6870706 | 1.4801858 | 129.0631433 | 1.4460014 | 0.6099425 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0492563 | 2039.5364325 | 1.2688226 | 124.1197029 | 1.2351027 | 0.3743463 |
| healthyR      |         13 | TBATS                      | Test  | 0.4597636 |  468.2796390 | 0.5559733 | 113.1731734 | 0.5731972 | 0.1948683 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.5176770 |  321.8209813 | 0.6260056 | 138.8623988 | 0.6406647 | 0.0594818 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7305204 |  420.6594500 | 0.5891848 | 139.7560537 | 0.8872284 | 0.2065665 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0690184 |  169.0382968 | 0.0556653 |  29.3056732 | 0.0805606 | 0.9949022 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.6926659 |  503.0936879 | 0.5586541 | 145.3373039 | 0.9134842 | 0.1678955 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0675449 |  143.8359116 | 0.0544768 |  29.2804955 | 0.0792098 | 0.9952951 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0308101 |    3.4201909 | 0.0248492 |   3.3739145 | 0.0709880 | 0.9962575 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0322873 |   18.7011141 | 0.0260406 |  15.4856261 | 0.0712977 | 0.9947610 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0725360 |  174.5175126 | 0.0585023 |  30.0871637 | 0.0904143 | 0.9939198 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4489672 |  774.2359634 | 0.3621044 | 106.5757528 | 0.5085246 | 0.9760150 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2251857 | 1376.8056586 | 0.9881461 | 133.2307556 | 1.4630996 | 0.5238463 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2211994 | 1779.0495834 | 0.9849310 | 131.1680253 | 1.4886696 | 0.1617108 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.7236119 |  524.8755351 | 0.5836130 | 143.9320164 | 0.8809132 | 0.2064491 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.7733190 |  420.0499890 | 0.6237031 | 151.3628540 | 0.9877293 | 0.0807685 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8430812 |  295.6909417 | 0.7631550 | 168.4131411 | 1.0197054 | 0.4066995 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0594169 |   26.9142015 | 0.0537841 |  14.3946552 | 0.0740791 | 0.9923626 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.6508963 |  448.3258722 | 0.5891898 | 124.5097794 | 0.7630578 | 0.1011109 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0522241 |   12.9854289 | 0.0472731 |  11.0346385 | 0.0678479 | 0.9928995 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0141741 |    4.0924673 | 0.0128304 |   3.5641148 | 0.0279496 | 0.9993608 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0082573 |    0.8936141 | 0.0074745 |   0.9054724 | 0.0237222 | 0.9993050 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0879557 |   27.2736386 | 0.0796173 |  29.5185904 | 0.0981271 | 0.9929448 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5185753 |  266.3719245 | 0.4694131 | 116.0151511 | 0.6100220 | 0.9759567 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0331288 |  979.4443871 | 0.9351856 | 115.7210412 | 1.1897911 | 0.4752516 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2713908 | 1449.9104629 | 1.1508599 | 133.5904442 | 1.4890041 | 0.1124006 |
| healthyverse  |         13 | TBATS                      | Test  | 0.6741295 |  237.7416279 | 0.6102204 | 134.2244242 | 0.8347311 | 0.1291355 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.6342151 |  299.1786914 | 0.5740899 | 108.8960363 | 0.8062945 | 0.0525976 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0433081 |  703.4415179 | 0.7235173 | 134.8469132 | 1.2407714 | 0.5028424 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0523518 |   42.7650102 | 0.0363051 |  17.6654919 | 0.0645955 | 0.9980358 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9411491 |  466.9036379 | 0.6526717 | 142.5760570 | 1.1061072 | 0.0428894 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0488913 |   33.4584833 | 0.0339053 |  16.1226352 | 0.0592534 | 0.9981562 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0192864 |    6.0500302 | 0.0133748 |   4.7392510 | 0.0339762 | 0.9991744 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0880315 |   20.1333141 | 0.0610484 |  25.2089173 | 0.1139521 | 0.9901065 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0611232 |   66.0071135 | 0.0423880 |  23.7456724 | 0.0742336 | 0.9970379 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.0514928 |  906.5154915 | 0.7291933 | 142.8333594 | 1.1479309 | 0.9132665 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1185170 |  609.0763214 | 0.7756735 | 122.5211918 | 1.3413597 | 0.4650148 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3340348 |  774.9990748 | 0.9251316 | 140.3463248 | 1.5667724 | 0.0825117 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9408655 |  416.8872840 | 0.6524751 | 142.2651384 | 1.1005795 | 0.0671909 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0038083 |  487.9717561 | 0.6961248 | 138.0995908 | 1.2165231 | 0.0235046 |
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
    ##   package  .model_id .model_desc .type     mae   mape    mase smape   rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl>  <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.00957 15.7   0.00672 6.02  0.0134 1.00 
    ## 2 healthyR         8 NNAR        Test  0.00803  6.68  0.00971 5.38  0.0151 0.999
    ## 3 healthy~         7 EARTH       Test  0.0308   3.42  0.0248  3.37  0.0710 0.996
    ## 4 healthy~         8 NNAR        Test  0.00826  0.894 0.00747 0.905 0.0237 0.999
    ## 5 healthy~         7 EARTH       Test  0.0193   6.05  0.0134  4.74  0.0340 0.999

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
    ## 1 healthyR.data <tibble [404 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [394 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [344 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [319 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [133 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
