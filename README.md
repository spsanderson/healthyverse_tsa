Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
03 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 40,627
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2022-07-01 22:37:42, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is 29.34 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 40627         |
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
| r_version     |     27248 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     27248 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     27248 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       23 |          0 |
| country       |      3254 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-01 | 2021-11-04 |      586 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1430528.60 | 1802277.28 | 357 | 16873 | 289680 | 2985480 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8737.44 |   16772.45 |   1 |   170 |   2658 |    8628 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-01 22:37:42 | 2021-11-04 10:32:41 |    23687 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 20M 5S |       60 |

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

``` r
recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

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
  add_recipe(recipe = recipe_date) %>%
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
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [526|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [519|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [467|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [443|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [258|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [106|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|------------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0440757 | 13.1374091 | 0.0403988 |  10.3444237 | 0.0558690 | 0.9969424 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4518200 | 76.4432885 | 0.4141275 | 101.5653235 | 0.5650754 | 0.9992922 |
| healthyR.data |         3 | LM          | Test  | 0.0449530 | 11.0036316 | 0.0412029 |  10.0690853 | 0.0578664 | 0.9974270 |
| healthyR.data |         4 | EARTH       | Test  | 0.0224466 |  6.6043304 | 0.0205741 |   6.3139815 | 0.0286484 | 0.9992922 |
| healthyR.data |         5 | NNAR        | Test  | 0.0071766 |  1.4205342 | 0.0065779 |   1.3992043 | 0.0108385 | 0.9998402 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0478828 |  8.7883735 | 0.0435021 |   8.2813015 | 0.0651175 | 0.9960990 |
| healthyR      |         2 | GLMNET      | Test  | 0.5096875 | 64.9098903 | 0.4630575 |  99.9960410 | 0.6444114 | 0.9960572 |
| healthyR      |         3 | LM          | Test  | 0.0482077 |  8.9033664 | 0.0437973 |   8.3283625 | 0.0658773 | 0.9961012 |
| healthyR      |         4 | EARTH       | Test  | 0.0310127 |  4.2885938 | 0.0281754 |   4.2159150 | 0.0611847 | 0.9960572 |
| healthyR      |         5 | NNAR        | Test  | 0.0099267 |  1.3259034 | 0.0090185 |   1.3523678 | 0.0217327 | 0.9995662 |
| healthyR.ts   |         1 | NULL        | NA    |        NA |         NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5101347 | 68.1705810 | 0.4235342 | 102.8084843 | 0.6003862 | 0.9989417 |
| healthyR.ts   |         3 | LM          | Test  | 0.0572602 | 16.3567937 | 0.0475397 |  15.9796857 | 0.0695512 | 0.9981147 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0141035 |  2.4567204 | 0.0117093 |   2.4395577 | 0.0167570 | 0.9997951 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0141032 |  3.4060825 | 0.0117090 |   3.3035679 | 0.0316581 | 0.9990831 |
| healthyverse  |         1 | NULL        | NA    |        NA |         NA |        NA |          NA |        NA |        NA |
| healthyverse  |         2 | GLMNET      | Test  | 0.4551427 | 95.3257182 | 0.3976254 | 110.1888589 | 0.5822720 | 0.9945173 |
| healthyverse  |         3 | LM          | Test  | 0.0471469 | 13.6929567 | 0.0411889 |  11.5782724 | 0.0597469 | 0.9950614 |
| healthyverse  |         4 | EARTH       | Test  | 0.0098735 |  2.0552052 | 0.0086258 |   2.0326431 | 0.0155401 | 0.9996810 |
| healthyverse  |         5 | NNAR        | Test  | 0.0033776 |  0.9218846 | 0.0029508 |   0.9109297 | 0.0055455 | 0.9999599 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0494906 | 10.3704756 | 0.0526740 |   9.0351125 | 0.0556556 | 0.9983742 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4278885 | 64.1349512 | 0.4554111 |  93.7285040 | 0.4944549 | 0.9984878 |
| healthyR.ai   |         3 | LM          | Test  | 0.0489896 |  9.8878311 | 0.0521407 |   8.7187214 | 0.0557821 | 0.9983718 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0138848 |  2.3354578 | 0.0147779 |   2.3109265 | 0.0158340 | 0.9998551 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0032081 |  0.6882654 | 0.0034144 |   0.6671374 | 0.0088032 | 0.9998812 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0340432 |  8.3970587 | 0.0334222 |   7.8962144 | 0.0388978 | 0.9985756 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4623750 | 63.9904979 | 0.4539406 |  95.3175956 | 0.5871998 | 0.9985264 |
| TidyDensity   |         3 | LM          | Test  | 0.0334373 |  8.5035971 | 0.0328274 |   7.8871785 | 0.0387913 | 0.9985633 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0104786 |  2.0091359 | 0.0102875 |   1.9758365 | 0.0147991 | 0.9998094 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0181364 |  3.8689136 | 0.0178056 |   3.7223107 | 0.0405589 | 0.9984729 |

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
    ##   # A tibble: 6 × 10
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy…         5 NNAR        Test  0.00718 1.42  0.00658 1.40  0.0108   1.00
    ## 2 healthyR         5 NNAR        Test  0.00993 1.33  0.00902 1.35  0.0217   1.00
    ## 3 healthy…         4 EARTH       Test  0.0141  2.46  0.0117  2.44  0.0168   1.00
    ## 4 healthy…         5 NNAR        Test  0.00338 0.922 0.00295 0.911 0.00555  1.00
    ## 5 healthy…         5 NNAR        Test  0.00321 0.688 0.00341 0.667 0.00880  1.00
    ## 6 TidyDen…         4 EARTH       Test  0.0105  2.01  0.0103  1.98  0.0148   1.00

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
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [526|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [519|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [467|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [443|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [258|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [106|28]> <mdl_time_tbl>

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
