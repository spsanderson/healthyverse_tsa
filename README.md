Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
10 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,746
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

The last day in the data set is 2022-02-08 22:38:40, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1710 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28746          |
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
| r\_version     |      19062 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19062 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19062 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2406 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-08 | 2021-08-24 |       443 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1524148.64 | 1873474.33 | 357 | 24902.25 | 271098 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8265.92 |   15746.98 |   1 |   288.00 |   2806 |    8406 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-08 22:38:40 | 2021-08-24 17:47:55 |     16814 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   50.5 |        60 |

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [386|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [376|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [326|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [301|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [115|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1392205 | 1523.617438 | 0.7335292 | 139.067106 | 1.4033967 | 0.1575813 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0552057 |  154.483134 | 0.0355462 |  13.139034 | 0.0708601 | 0.9957923 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0192317 |  642.084138 | 0.6562700 | 156.932119 | 1.2139596 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0561275 |  122.677630 | 0.0361397 |  12.606698 | 0.0781058 | 0.9950158 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0326551 |   72.858615 | 0.0210262 |   9.703161 | 0.0451857 | 0.9983392 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0139716 |   44.957398 | 0.0089961 |   7.518080 | 0.0240187 | 0.9995730 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0649653 |  109.621164 | 0.0418303 |  14.325112 | 0.0950515 | 0.9927349 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6281651 | 1359.950765 | 0.4044673 |  91.863820 | 0.7371228 | 0.9889377 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1294607 | 4944.151404 | 0.7272450 | 102.702351 | 1.4238859 | 0.3974533 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5087366 | 5010.424493 | 0.9714558 | 139.818135 | 1.8393777 | 0.0102967 |
| healthyR.data |         13 | TBATS                      | Test  | 1.0311568 |  811.106242 | 0.6639484 | 135.943156 | 1.2963282 | 0.2444326 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0187871 |  635.525971 | 0.6559837 | 157.132261 | 1.2129850 | 0.0275035 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7961592 | 2586.950240 | 0.8853316 | 157.456737 | 1.0005649 | 0.4687529 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0591242 |  137.713250 | 0.0657463 |  26.686927 | 0.0725306 | 0.9948812 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6573928 | 1812.630035 | 0.7310229 | 128.043132 | 0.8620268 | 0.1767430 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0543801 |  123.157207 | 0.0604708 |  28.332196 | 0.0682824 | 0.9949402 |
| healthyR      |          7 | EARTH                      | Test  | 0.0337155 |   48.607855 | 0.0374917 |  17.980457 | 0.0748021 | 0.9933789 |
| healthyR      |          8 | NNAR                       | Test  | 0.0059517 |    8.547596 | 0.0066183 |   6.084335 | 0.0125966 | 0.9998965 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0631193 |  201.339860 | 0.0701889 |  23.843916 | 0.0774163 | 0.9935034 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5202092 | 2469.711843 | 0.5784743 | 119.850917 | 0.6100726 | 0.9753090 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0910144 | 3867.184261 | 1.2132115 | 102.021993 | 1.3660330 | 0.5725760 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0323783 | 4340.881245 | 1.1480079 | 126.640356 | 1.2836135 | 0.2113256 |
| healthyR      |         13 | TBATS                      | Test  | 0.6572625 | 1177.489933 | 0.7308780 | 129.569211 | 0.8699092 | 0.2710111 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7177591 | 1619.701292 | 0.7981505 | 140.308209 | 0.9302747 | 0.0099243 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2474649 | 4553.158883 | 1.0204817 | 148.293755 | 1.5034753 | 0.5977722 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0763532 |  332.836325 | 0.0624603 |  22.196489 | 0.0947772 | 0.9937581 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.3193235 | 5147.665114 | 1.0792652 | 155.783882 | 1.5402437 | 0.2018076 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0727240 |  290.661554 | 0.0594915 |  22.430790 | 0.0872732 | 0.9938968 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0254724 |    3.110115 | 0.0208376 |   3.107057 | 0.0661345 | 0.9972216 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0429420 |   34.970731 | 0.0351284 |  15.498374 | 0.1302727 | 0.9887492 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0798902 |  326.957325 | 0.0653537 |  22.479501 | 0.0989024 | 0.9926973 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6829359 | 2824.870783 | 0.5586719 | 123.769479 | 0.7977780 | 0.9803371 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1308238 | 2120.076869 | 0.9250641 | 113.829852 | 1.4128602 | 0.6109435 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5535927 | 7857.728092 | 1.2709078 | 144.369302 | 1.8764564 | 0.2374654 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.0080945 | 3120.468244 | 0.8246660 | 144.847183 | 1.2639260 | 0.4390919 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2610642 | 4133.368392 | 1.0316064 | 150.329049 | 1.5447735 | 0.2800613 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.1232906 |  132.199552 | 1.0085596 | 166.038051 | 1.3530581 | 0.3508895 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0692996 |    8.927830 | 0.0622215 |   8.497018 | 0.0913323 | 0.9909532 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8553879 |  148.459859 | 0.7680200 | 121.552001 | 1.0109264 | 0.0446377 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0647523 |    7.828185 | 0.0581386 |   7.414106 | 0.0929813 | 0.9909725 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0207405 |    2.064846 | 0.0186221 |   2.077983 | 0.0435148 | 0.9988675 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0195965 |    1.324735 | 0.0175949 |   1.328406 | 0.0424462 | 0.9984987 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1105991 |   24.896581 | 0.0993027 |  24.379597 | 0.1208114 | 0.9888361 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6314359 |   87.133461 | 0.5669421 | 100.662140 | 0.7553884 | 0.9661821 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1741838 |  263.590832 | 1.0542546 | 107.448495 | 1.3415958 | 0.3869941 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2681711 |  246.744097 | 1.1386422 | 128.654801 | 1.5503397 | 0.0438879 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9413236 |  108.181677 | 0.8451783 | 139.021940 | 1.1683357 | 0.0309431 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8266017 |  132.167884 | 0.7421740 | 104.326997 | 1.0308778 | 0.1918112 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2060771 | 4777.515609 | 0.9669588 | 161.245646 | 1.3793014 | 0.4219490 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0646610 |  249.748077 | 0.0518413 |  24.819846 | 0.0809701 | 0.9949145 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 1.0889445 | 2950.170934 | 0.8730490 | 167.757852 | 1.2306418 | 0.0743070 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0537786 |  115.667424 | 0.0431164 |  19.539940 | 0.0713123 | 0.9952411 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0172773 |   12.512481 | 0.0138519 |   7.961219 | 0.0322816 | 0.9991669 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0466188 |   68.300996 | 0.0373761 |  24.520646 | 0.0727241 | 0.9962007 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0683180 |  293.268511 | 0.0547732 |  26.938046 | 0.0858222 | 0.9936094 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9204978 | 4918.552883 | 0.7379988 | 156.918671 | 1.0283487 | 0.9291778 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1695795 | 1368.994088 | 0.9376972 | 130.787280 | 1.4373034 | 0.4527777 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3790127 | 3842.567273 | 1.1056079 | 139.616917 | 1.7228647 | 0.1128667 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0675133 | 3162.269244 | 0.8558668 | 168.024813 | 1.1962290 | 0.0945473 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0935284 | 2717.779267 | 0.8767241 | 163.261288 | 1.2981279 | 0.0184533 |
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
    ## 1 healthyR~         8 NNAR        Test  0.0140  45.0  0.00900  7.52 0.0240 1.00 
    ## 2 healthyR          8 NNAR        Test  0.00595  8.55 0.00662  6.08 0.0126 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0255   3.11 0.0208   3.11 0.0661 0.997
    ## 4 healthyv~         8 NNAR        Test  0.0196   1.32 0.0176   1.33 0.0424 0.998
    ## 5 healthyR~         7 EARTH       Test  0.0173  12.5  0.0139   7.96 0.0323 0.999

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [386|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [376|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [326|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [301|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [115|28]> <mdl_time_tbl>

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
