Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
10 May, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 36,481
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

The last day in the data set is 2022-05-08 23:08:35, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -3846.5
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 36481         |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     24479 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     24479 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     24479 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      2916 |          0.92 |   2 |   2 |     0 |      107 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-05-08 | 2021-10-18 |      532 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1460366.01 | 1835265.31 | 357 | 16873 | 271097 | 3243235 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8585.79 |   16418.94 |   1 |   192 |   2663 |    8617 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-05-08 23:08:35 | 2021-10-18 15:29:07 |    21209 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 8M 6S |       60 |

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
    ## Variable mutation for lubridate::year(date)
    ## Harmonic numeric variables for yr
    ## Fourier Transformation on value_trans
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

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

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
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_arima_boosted,
  wflw_auto_arima,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [473|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [465|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [414|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [390|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [204|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [80 x 6]> <tibble>     <split [52|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc            | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:-----------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA W XGBOOST ERRORS | Test  | 0.6751307 |  80.636085 | 0.4416341 | 113.890961 | 0.8945918 | 0.6886051 |
| healthyR.data |         2 | REGRESSION             | Test  | 0.0617153 |   8.365473 | 0.0403708 |   8.234748 | 0.0772835 | 0.9935485 |
| healthyR.data |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | LM                     | Test  | 0.0671606 |  10.029226 | 0.0439328 |   9.404291 | 0.0865611 | 0.9927217 |
| healthyR.data |         5 | EARTH                  | Test  | 0.0628722 |  10.514132 | 0.0411276 |   8.740877 | 0.1014775 | 0.9888173 |
| healthyR.data |         6 | NNAR                   | Test  | 0.0261681 |   3.814134 | 0.0171177 |   3.804257 | 0.0349395 | 0.9989564 |
| healthyR      |         1 | ARIMA                  | Test  | 0.7748977 | 206.872443 | 0.6750288 | 134.924993 | 0.9547999 | 0.5907666 |
| healthyR      |         2 | REGRESSION             | Test  | 0.0444383 |  11.699550 | 0.0387111 |  10.868097 | 0.0517834 | 0.9974753 |
| healthyR      |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | LM                     | Test  | 0.0451628 |  11.642057 | 0.0393422 |  10.889981 | 0.0528395 | 0.9974970 |
| healthyR      |         5 | EARTH                  | Test  | 0.0155408 |   2.854713 | 0.0135379 |   2.804179 | 0.0188467 | 0.9998348 |
| healthyR      |         6 | NNAR                   | Test  | 0.0571171 |   9.235591 | 0.0497559 |   8.578140 | 0.1068494 | 0.9901116 |
| healthyR.ts   |         1 | ARIMA                  | Test  | 0.6830969 | 168.615934 | 0.5886023 | 128.351085 | 0.8956119 | 0.8198116 |
| healthyR.ts   |         2 | REGRESSION             | Test  | 0.0516855 |  21.024484 | 0.0445357 |  14.532259 | 0.0623096 | 0.9967205 |
| healthyR.ts   |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | LM                     | Test  | 0.0505738 |  20.804131 | 0.0435778 |  14.268115 | 0.0615291 | 0.9969078 |
| healthyR.ts   |         5 | EARTH                  | Test  | 0.0152164 |   3.098758 | 0.0131115 |   3.027389 | 0.0177000 | 0.9998657 |
| healthyR.ts   |         6 | NNAR                   | Test  | 0.0166867 |   5.382524 | 0.0143784 |   5.849674 | 0.0242312 | 0.9993483 |
| healthyverse  |         1 | ARIMA                  | Test  | 0.5779646 | 111.762432 | 0.5536582 | 132.979716 | 0.7721953 | 0.7841172 |
| healthyverse  |         2 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | LM                     | Test  | 0.0360477 |   9.404544 | 0.0345318 |   8.516694 | 0.0436894 | 0.9980672 |
| healthyverse  |         5 | EARTH                  | Test  | 0.0089526 |   2.659537 | 0.0085761 |   2.697527 | 0.0124522 | 0.9997875 |
| healthyverse  |         6 | NNAR                   | Test  | 0.0069368 |   1.516557 | 0.0066451 |   1.498239 | 0.0138672 | 0.9998278 |
| healthyR.ai   |         1 | ARIMA                  | Test  | 0.6934217 | 145.912086 | 0.5721468 | 124.044970 | 0.9173292 | 0.5681747 |
| healthyR.ai   |         2 | REGRESSION             | Test  | 0.0442021 |   9.238774 | 0.0364714 |   8.727652 | 0.0588505 | 0.9960604 |
| healthyR.ai   |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | LM                     | Test  | 0.0437745 |   9.169199 | 0.0361186 |   8.591318 | 0.0600033 | 0.9960960 |
| healthyR.ai   |         5 | EARTH                  | Test  | 0.0152223 |   2.812269 | 0.0125600 |   2.766911 | 0.0189235 | 0.9996938 |
| healthyR.ai   |         6 | NNAR                   | Test  | 0.0206544 |   1.836246 | 0.0170421 |   1.812286 | 0.0590879 | 0.9980076 |
| TidyDensity   |         1 | ARIMA                  | Test  | 0.7081987 | 124.532597 | 0.4590005 | 106.037829 | 0.8914692 | 0.9633909 |
| TidyDensity   |         2 | REGRESSION             | Test  | 0.1050265 |  29.770612 | 0.0680702 |  26.172658 | 0.1310892 | 0.9910848 |
| TidyDensity   |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         4 | LM                     | Test  | 0.1597069 |  39.192604 | 0.1035098 |  34.793807 | 0.2002952 | 0.9814049 |
| TidyDensity   |         5 | EARTH                  | Test  | 0.0229367 |   3.053654 | 0.0148659 |   3.136430 | 0.0279727 | 0.9993932 |
| TidyDensity   |         6 | NNAR                   | Test  | 0.4489334 |  80.680246 | 0.2909645 |  72.528124 | 0.5767732 | 0.8940798 |

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
    ##   # A tibble: 6 x 10
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         6 NNAR        Test  0.0262   3.81 0.0171   3.80 0.0349 0.999
    ## 2 healthyR          5 EARTH       Test  0.0155   2.85 0.0135   2.80 0.0188 1.00 
    ## 3 healthyR~         5 EARTH       Test  0.0152   3.10 0.0131   3.03 0.0177 1.00 
    ## 4 healthyv~         5 EARTH       Test  0.00895  2.66 0.00858  2.70 0.0125 1.00 
    ## 5 healthyR~         5 EARTH       Test  0.0152   2.81 0.0126   2.77 0.0189 1.00 
    ## 6 TidyDens~         5 EARTH       Test  0.0229   3.05 0.0149   3.14 0.0280 0.999

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(!is.na(.model_id)) %>%
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
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = FALSE
    )
  )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [473|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [465|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [414|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [390|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [204|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [80 x 6]> <tibble>     <split [52|28]>  <mdl_time_tbl>

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
