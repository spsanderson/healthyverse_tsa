Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
06 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,365
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

The last day in the data set is 2022-02-04 23:16:33, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1614.64
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28365          |
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
| country        |       2389 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-04 | 2021-08-23 |       439 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1527292.04 | 1875001.31 | 357 | 26285 | 271098 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8277.04 |   15802.78 |   1 |   253 |   2806 |    8305 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-04 23:16:33 | 2021-08-23 07:39:01 |     16634 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 54M 47S |        60 |

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
    ## 1 healthyR.data <tibble [410 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [400 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [350 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [325 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [139 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1240951 |  115.715080 | 0.6999102 | 131.009384 | 1.3829030 | 0.6381278 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0529830 |    6.512687 | 0.0329895 |   6.569359 | 0.0643324 | 0.9963401 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0909605 |   99.468034 | 0.6792792 | 130.824789 | 1.3631484 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0536290 |    6.176950 | 0.0333917 |   6.099038 | 0.0686054 | 0.9961678 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0282701 |    3.095709 | 0.0176022 |   3.061069 | 0.0376827 | 0.9988053 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0150596 |    2.315244 | 0.0093768 |   2.248495 | 0.0277226 | 0.9993033 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0612579 |    7.634760 | 0.0381418 |   7.760324 | 0.0828760 | 0.9945053 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6566750 |   71.567108 | 0.4088743 |  94.223015 | 0.7643411 | 0.9938099 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9586715 |  139.950453 | 0.5969103 |  78.216563 | 1.2984545 | 0.5341689 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3525908 |  187.260326 | 0.8421816 | 134.247957 | 1.6926060 | 0.0843786 |
| healthyR.data |         13 | BATS                       | Test  | 1.1337981 |  106.244129 | 0.7059517 | 129.695738 | 1.4206079 | 0.1295734 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0906178 |   99.431403 | 0.6790658 | 130.867130 | 1.3624752 | 0.0336131 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8470242 | 3225.293491 | 0.9436257 | 168.761708 | 1.0369253 | 0.5027738 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0492252 |  145.956303 | 0.0548392 |  32.158477 | 0.0649720 | 0.9940337 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6051973 | 1888.060716 | 0.6742190 | 133.621481 | 0.7840936 | 0.1626289 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0463139 |  142.087258 | 0.0515960 |  34.411235 | 0.0623189 | 0.9940672 |
| healthyR      |          7 | EARTH                      | Test  | 0.0284768 |   54.726272 | 0.0317246 |  21.743533 | 0.0694548 | 0.9933435 |
| healthyR      |          8 | NNAR                       | Test  | 0.0065516 |   50.690432 | 0.0072988 |  15.709806 | 0.0116500 | 0.9998187 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0525500 |  190.375302 | 0.0585433 |  27.594097 | 0.0701377 | 0.9926681 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5102290 | 2669.194136 | 0.5684197 | 127.921621 | 0.6011240 | 0.9652240 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1324163 | 6272.405700 | 1.2615664 | 113.776948 | 1.3848957 | 0.5388906 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0470070 | 7093.093359 | 1.1664163 | 127.245336 | 1.3237986 | 0.2155107 |
| healthyR      |         13 | TBATS                      | Test  | 0.5687864 | 2239.309923 | 0.6336554 | 122.143820 | 0.7501932 | 0.2259658 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6581960 |  562.631726 | 0.7332621 | 172.015397 | 0.8663223 | 0.0718901 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0172675 | 2610.096329 | 0.8007736 | 132.577408 | 1.2617118 | 0.5848533 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0761316 |  464.810579 | 0.0599294 |  22.386518 | 0.0934755 | 0.9940764 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.9324631 | 1132.153638 | 0.7340172 | 142.835349 | 1.1527338 | 0.1166464 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0742101 |  410.216495 | 0.0584168 |  23.184752 | 0.0893039 | 0.9945096 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0278703 |    7.497792 | 0.0219390 |   9.777457 | 0.0634221 | 0.9970289 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0784192 |   53.657921 | 0.0617301 |  22.016614 | 0.2301879 | 0.9737188 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0825283 |  546.764732 | 0.0649647 |  23.693920 | 0.1025345 | 0.9929838 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5521214 | 1892.967992 | 0.4346194 | 109.315340 | 0.6655944 | 0.9859096 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1356038 | 3179.928001 | 0.8939256 | 122.300154 | 1.4161371 | 0.5363039 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2879177 | 4024.976152 | 1.0138243 | 133.478571 | 1.6138185 | 0.1148831 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.8492956 |  996.271414 | 0.6685494 | 133.562457 | 1.0744599 | 0.3318283 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9907337 | 2214.595245 | 0.7798867 | 140.361880 | 1.2368302 | 0.3216330 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1385381 | 1084.890025 | 1.0388944 | 179.435712 | 1.3244587 | 0.5473686 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0630040 |   17.046288 | 0.0574900 |  15.505450 | 0.0818372 | 0.9911954 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8267228 | 1317.373145 | 0.7543689 | 133.281533 | 0.9869577 | 0.0342524 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0581014 |   40.169929 | 0.0530165 |  14.831413 | 0.0805940 | 0.9911779 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0182180 |   10.282595 | 0.0166235 |   5.736923 | 0.0370804 | 0.9990120 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0093506 |   34.098393 | 0.0085323 |   7.718029 | 0.0238527 | 0.9994482 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1114814 |   65.181992 | 0.1017247 |  33.706475 | 0.1224936 | 0.9892137 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6068403 |  705.982193 | 0.5537303 | 108.759362 | 0.7113898 | 0.9686023 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2533290 | 3760.314796 | 1.1436390 | 116.923138 | 1.3915881 | 0.3570046 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3762722 | 4687.422430 | 1.2558222 | 142.743033 | 1.6083609 | 0.0423465 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8669560 |  598.845040 | 0.7910809 | 142.971524 | 1.0732067 | 0.0814316 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8045333 |  667.445281 | 0.7341214 | 120.519015 | 1.0156743 | 0.1936487 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1080154 | 3678.618387 | 0.8011443 | 149.469664 | 1.3126736 | 0.4544995 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0586603 |  180.859812 | 0.0424140 |  23.462366 | 0.0745314 | 0.9954620 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9522079 | 1419.504967 | 0.6884886 | 170.299605 | 1.0891708 | 0.0569941 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0524584 |   82.114982 | 0.0379298 |  16.619211 | 0.0690166 | 0.9956478 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0124556 |    5.498422 | 0.0090059 |   4.728003 | 0.0167332 | 0.9997295 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0555324 |   42.907505 | 0.0401524 |  22.076481 | 0.0887363 | 0.9945718 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0648097 |  263.987960 | 0.0468603 |  26.467918 | 0.0854612 | 0.9938162 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9043813 | 4066.204115 | 0.6539079 | 148.377092 | 1.0285276 | 0.9196415 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2031910 | 1163.224308 | 0.8699606 | 139.594475 | 1.4330593 | 0.3964596 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3039787 | 1946.716894 | 0.9428345 | 142.764973 | 1.6755956 | 0.0533930 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9744261 | 2280.983071 | 0.7045534 | 168.220275 | 1.0911159 | 0.0944017 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9913509 | 1772.087518 | 0.7167908 | 154.771150 | 1.2153363 | 0.1978291 |
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
    ## 1 healthyR~         8 NNAR        Test  0.0151   2.32 0.00938  2.25 0.0277 0.999
    ## 2 healthyR          8 NNAR        Test  0.00655 50.7  0.00730 15.7  0.0117 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0279   7.50 0.0219   9.78 0.0634 0.997
    ## 4 healthyv~         8 NNAR        Test  0.00935 34.1  0.00853  7.72 0.0239 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0125   5.50 0.00901  4.73 0.0167 1.00

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
    ## 1 healthyR.data <tibble [410 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [400 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [350 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [325 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [139 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
