Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
29 May, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 38,079
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

The last day in the data set is 2022-05-27 23:45:45, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4303.12
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 38079         |
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
| r_version     |     25565 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     25565 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     25565 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      3040 |          0.92 |   2 |   2 |     0 |      109 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-05-27 | 2021-10-27 |      551 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1448613.31 | 1823415.54 | 357 | 16873 | 271502 | 3241513 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8719.28 |   16714.14 |   1 |   186 |   2660 |    8647 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-05-27 23:45:45 | 2021-10-27 03:17:05 |    22142 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 12M 46S |       60 |

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
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl…
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [492|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [484|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [433|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [409|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [223|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [99 × 6]> <tibble>     <split [71|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0526149 |  8.895629 | 0.0320920 |   9.911145 | 0.0731671 | 0.9952326 |
| healthyR.data |         2 | GLMNET      | Test  | 0.6143247 | 78.418728 | 0.3747015 | 107.608175 | 0.7737074 | 0.9963505 |
| healthyR.data |         3 | LM          | Test  | 0.0641407 |  9.917960 | 0.0391220 |  10.308875 | 0.0842351 | 0.9941329 |
| healthyR.data |         4 | EARTH       | Test  | 0.0407657 |  6.421811 | 0.0248647 |   7.108968 | 0.0756407 | 0.9953442 |
| healthyR.data |         5 | NNAR        | Test  | 0.0727511 |  6.890446 | 0.0443738 |   6.166263 | 0.2322068 | 0.9666754 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0469236 | 14.374927 | 0.0352725 |  14.017566 | 0.0645616 | 0.9967237 |
| healthyR      |         2 | GLMNET      | Test  | 0.5528789 | 71.414928 | 0.4155997 | 109.456253 | 0.7668665 | 0.9965257 |
| healthyR      |         3 | LM          | Test  | 0.0475183 | 14.908416 | 0.0357195 |  14.736825 | 0.0646936 | 0.9967279 |
| healthyR      |         4 | EARTH       | Test  | 0.0270838 |  3.293033 | 0.0203589 |   3.197996 | 0.0697599 | 0.9978905 |
| healthyR      |         5 | NNAR        | Test  | 0.0736054 |  7.356749 | 0.0553293 |   7.226586 | 0.2199577 | 0.9795966 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0431547 |  9.166854 | 0.0302952 |   8.341023 | 0.0519095 | 0.9978571 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5482693 | 62.360687 | 0.3848925 |  91.740935 | 0.7087227 | 0.9976686 |
| healthyR.ts   |         3 | LM          | Test  | 0.0421386 |  9.220457 | 0.0295819 |   8.465224 | 0.0503022 | 0.9979037 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0146163 |  2.698099 | 0.0102608 |   2.639357 | 0.0176936 | 0.9998927 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0140886 |  4.449866 | 0.0098904 |   3.689259 | 0.0271859 | 0.9994456 |
| healthyverse  |         1 | NULL        | NA    |        NA |        NA |        NA |         NA |        NA |        NA |
| healthyverse  |         2 | GLMNET      | Test  | 0.4895001 | 91.543981 | 0.3912923 | 101.833809 | 0.6027166 | 0.9981283 |
| healthyverse  |         3 | LM          | Test  | 0.0395013 |  7.682241 | 0.0315762 |   7.212738 | 0.0472049 | 0.9982314 |
| healthyverse  |         4 | EARTH       | Test  | 0.0119849 |  2.508110 | 0.0095804 |   2.522181 | 0.0163658 | 0.9996671 |
| healthyverse  |         5 | NNAR        | Test  | 0.0077199 |  3.240088 | 0.0061711 |   4.564768 | 0.0189754 | 0.9995501 |
| healthyR.ai   |         1 | NULL        | NA    |        NA |        NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.5140770 | 60.909371 | 0.3592425 |  88.310102 | 0.6431187 | 0.9983786 |
| healthyR.ai   |         3 | LM          | Test  | 0.0391332 |  6.855649 | 0.0273467 |   6.510219 | 0.0460546 | 0.9982834 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0165089 |  2.398190 | 0.0115366 |   2.367421 | 0.0203539 | 0.9997600 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0086990 |  1.348375 | 0.0060790 |   1.402001 | 0.0162228 | 0.9997278 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0456543 | 14.726697 | 0.0379928 |  12.550800 | 0.0562591 | 0.9989669 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4366550 | 60.146101 | 0.3633780 |  86.536805 | 0.5342107 | 0.9991216 |
| TidyDensity   |         3 | LM          | Test  | 0.0791913 | 29.419251 | 0.0659018 |  26.471412 | 0.0913448 | 0.9986924 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0111961 |  2.506226 | 0.0093173 |   2.485365 | 0.0149348 | 0.9998401 |
| TidyDensity   |         5 | NNAR        | Test  | 0.2660355 | 48.425647 | 0.2213909 |  57.973229 | 0.3270636 | 0.9063537 |

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
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR…         1 REGRESSION  Test  0.0526   8.90 0.0321   9.91 0.0732 0.995
    ## 2 healthyR          1 REGRESSION  Test  0.0469  14.4  0.0353  14.0  0.0646 0.997
    ## 3 healthyR…         4 EARTH       Test  0.0146   2.70 0.0103   2.64 0.0177 1.00 
    ## 4 healthyv…         4 EARTH       Test  0.0120   2.51 0.00958  2.52 0.0164 1.00 
    ## 5 healthyR…         5 NNAR        Test  0.00870  1.35 0.00608  1.40 0.0162 1.00 
    ## 6 TidyDens…         4 EARTH       Test  0.0112   2.51 0.00932  2.49 0.0149 1.00

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
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl…
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [492|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [484|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [433|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [409|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [223|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [99 × 6]> <tibble>     <split [71|28]>  <mdl_time_tbl>

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
