Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
27 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,687
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

The last day in the data set is 2022-01-25 19:30:23, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1370.87
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27687          |
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
| r\_version     |      18445 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18445 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18445 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2324 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-25 | 2021-08-15 |       429 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532272.91 | 1877434.73 | 357 | 21809.5 | 251689 | 3247929 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8124.56 |   15448.26 |   1 |   239.0 |   2782 |    8243 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-25 19:30:23 | 2021-08-15 13:08:33 |     16204 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 53M 34S |        60 |

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
    ## 1 healthyR.data <tibble [400 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [390 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [340 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [315 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [129 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7952941 |  326.057865 | 0.5502891 | 114.469235 | 1.0043367 | 0.3350428 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1026930 |   19.713128 | 0.0710565 |  22.726485 | 0.1619675 | 0.9730909 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8107802 |  181.634224 | 0.5610044 | 120.608506 | 1.0270079 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0687602 |   21.037688 | 0.0475773 |  17.794315 | 0.0848105 | 0.9937697 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0292522 |   10.174656 | 0.0202405 |   6.855270 | 0.0452922 | 0.9980829 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0074172 |    1.323039 | 0.0051322 |   1.334638 | 0.0096942 | 0.9998976 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0710979 |   20.278674 | 0.0491949 |  18.230963 | 0.0991062 | 0.9914675 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4899926 |  200.092402 | 0.3390414 |  85.240953 | 0.5900378 | 0.9958344 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1555461 |  590.103657 | 0.7995588 | 121.663815 | 1.3631080 | 0.4008299 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3448847 |  561.503466 | 0.9305682 | 139.599487 | 1.6182917 | 0.0376805 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8162866 |  209.387439 | 0.5648144 | 111.213382 | 1.0736362 | 0.0702786 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8104321 |  181.410129 | 0.5607635 | 120.673164 | 1.0262864 | 0.1020341 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7561881 |  515.963899 | 0.8813889 | 159.593417 | 0.8650121 | 0.6079611 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0375812 |   21.849956 | 0.0438035 |  18.305112 | 0.0448710 | 0.9985931 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5131169 |  159.694770 | 0.5980727 | 127.258701 | 0.6620154 | 0.1017316 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0350268 |   21.748933 | 0.0408262 |  20.707722 | 0.0419763 | 0.9988256 |
| healthyR      |          7 | EARTH                      | Test  | 0.0180599 |    9.810383 | 0.0210500 |   8.369460 | 0.0199455 | 0.9995052 |
| healthyR      |          8 | NNAR                       | Test  | 0.0077955 |    3.248584 | 0.0090862 |   2.967572 | 0.0162975 | 0.9995721 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0399454 |   23.394955 | 0.0465591 |  18.573346 | 0.0521581 | 0.9977009 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4398386 |  357.027017 | 0.5126619 | 133.014011 | 0.4865869 | 0.9963974 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2367846 | 1161.933890 | 1.4415569 | 126.018188 | 1.4452589 | 0.6046517 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0676009 | 1163.212676 | 1.2443618 | 122.859050 | 1.2517506 | 0.3691148 |
| healthyR      |         13 | TBATS                      | Test  | 0.4921367 |  217.422231 | 0.5736189 | 120.246911 | 0.6409115 | 0.2275118 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.5710263 |  122.866107 | 0.6655701 | 195.185939 | 0.6939508 | 0.0316614 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7239585 |  168.567303 | 0.5548596 | 128.143191 | 0.8811893 | 0.2396260 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0674250 |   62.603199 | 0.0516762 |  22.168190 | 0.0777617 | 0.9955184 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7197104 |  251.882953 | 0.5516037 | 136.599688 | 0.9174915 | 0.1757096 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0685044 |   59.658806 | 0.0525034 |  22.709259 | 0.0814178 | 0.9958214 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0320114 |    3.222920 | 0.0245344 |   3.156645 | 0.0748927 | 0.9959446 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0413449 |   10.544196 | 0.0316878 |  11.388140 | 0.1016653 | 0.9899153 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0734454 |   67.147278 | 0.0562904 |  22.050946 | 0.0955423 | 0.9946037 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4528870 |  336.519842 | 0.3471037 | 103.060135 | 0.5230874 | 0.9816881 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2706224 |  665.556888 | 0.9738362 | 131.450863 | 1.4780352 | 0.5199482 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3103730 |  816.695555 | 1.0043021 | 147.733750 | 1.5004488 | 0.1524323 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6984980 |  214.582090 | 0.5353461 | 124.041020 | 0.8629183 | 0.2692273 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8175040 |  253.148103 | 0.6265552 | 146.471697 | 1.0053743 | 0.0305015 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8946284 |  355.199286 | 0.8629760 | 172.623370 | 1.0207542 | 0.4338672 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0599360 |   24.046222 | 0.0578154 |  17.476688 | 0.0696009 | 0.9967170 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.5893094 |  231.436095 | 0.5684593 | 127.999917 | 0.7000027 | 0.0835066 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0543218 |   17.177057 | 0.0523999 |  14.000692 | 0.0629556 | 0.9969656 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0100415 |    3.179638 | 0.0096862 |   3.022762 | 0.0148394 | 0.9997119 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0037459 |    1.127995 | 0.0036134 |   1.120608 | 0.0048232 | 0.9999643 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0841065 |   30.545994 | 0.0811307 |  32.105020 | 0.0934198 | 0.9957135 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5087083 |  219.408622 | 0.4907099 | 127.085058 | 0.5646684 | 0.9901627 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0297638 |  681.342306 | 0.9933302 | 118.276701 | 1.1616557 | 0.4367102 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2944754 |  956.490509 | 1.2486761 | 136.019250 | 1.4691517 | 0.1016598 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5837797 |  160.732244 | 0.5631252 | 128.060024 | 0.7256972 | 0.1809084 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.5842620 |  155.176362 | 0.5635905 | 121.430500 | 0.7366489 | 0.0118453 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0454509 |  194.954940 | 0.6489430 | 127.064903 | 1.2857887 | 0.4557119 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0524337 |   10.399740 | 0.0325472 |  11.932781 | 0.0631752 | 0.9982568 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9723942 |  157.509581 | 0.6035945 | 133.699183 | 1.1763463 | 0.0426451 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0497245 |    9.484904 | 0.0308655 |  10.143134 | 0.0592955 | 0.9984210 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0133608 |    1.689474 | 0.0082935 |   1.674314 | 0.0171615 | 0.9997735 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0912728 |   15.429826 | 0.0566558 |  19.544783 | 0.1226047 | 0.9859144 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0641629 |   13.750699 | 0.0398279 |  16.845406 | 0.0781799 | 0.9971798 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.2010995 |  269.926399 | 0.7455588 | 139.640958 | 1.3114359 | 0.8365502 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1706163 |  281.302824 | 0.7266369 | 122.349889 | 1.3487315 | 0.4789643 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4531885 |  324.056027 | 0.9020381 | 147.866073 | 1.5952555 | 0.0861325 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9289365 |  141.734932 | 0.5766190 | 134.103398 | 1.1219303 | 0.0923091 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0168161 |  166.863956 | 0.6311685 | 128.872571 | 1.2463194 | 0.0261317 |
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
    ## 1 healthy~         8 NNAR        Test  0.00742  1.32 0.00513  1.33 0.00969 1.00 
    ## 2 healthyR         8 NNAR        Test  0.00780  3.25 0.00909  2.97 0.0163  1.00 
    ## 3 healthy~         7 EARTH       Test  0.0320   3.22 0.0245   3.16 0.0749  0.996
    ## 4 healthy~         8 NNAR        Test  0.00375  1.13 0.00361  1.12 0.00482 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0134   1.69 0.00829  1.67 0.0172  1.00

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
    ## 1 healthyR.data <tibble [400 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [390 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [340 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [315 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [129 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
