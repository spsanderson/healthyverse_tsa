Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
07 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 40,926
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

The last day in the data set is 2022-07-05 23:23:18, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -5238.75
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 40926         |
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
| r_version     |     27421 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     27421 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     27421 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3272 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-05 | 2021-11-05 |      590 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1428986.75 | 1799152.49 | 357 | 16873 | 289681 | 2977943 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8742.65 |   16786.73 |   1 |   174 |   2644 |    8628 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-05 23:23:18 | 2021-11-05 18:07:04 |    23904 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     28 |       60 |

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
    ## 1 healthyR      <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 2 healthyR.data <tibble>     <tibble [28 × 6]> <split [306|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [307|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [262|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [110|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0578680 |  12.3898090 | 0.0491600 |  11.1263892 | 0.0699660 | 0.9959743 |
| healthyR      |         2 | GLMNET      | Test  | 0.5511211 |  66.3848439 | 0.4681883 | 102.7544270 | 0.7303596 | 0.9966271 |
| healthyR      |         3 | LM          | Test  | 0.0570818 |  12.6629417 | 0.0484921 |  11.4248203 | 0.0679345 | 0.9959953 |
| healthyR      |         4 | EARTH       | Test  | 0.0394232 |   4.4225170 | 0.0334908 |   4.3860678 | 0.0789719 | 0.9966271 |
| healthyR      |         5 | NNAR        | Test  | 0.0200577 |   1.6178882 | 0.0170394 |   1.5706549 | 0.0775399 | 0.9961075 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0547813 |  57.3688139 | 0.0485638 |  17.3656307 | 0.0665717 | 0.9961289 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4387760 | 127.5739236 | 0.3889766 |  92.2595963 | 0.5493654 | 0.9988957 |
| healthyR.data |         3 | LM          | Test  | 0.0530331 |  53.4814638 | 0.0470140 |  16.7397956 | 0.0651174 | 0.9961344 |
| healthyR.data |         4 | EARTH       | Test  | 0.0214263 |  19.3721425 | 0.0189945 |   8.9274426 | 0.0284455 | 0.9988957 |
| healthyR.data |         5 | NNAR        | Test  | 0.0043413 |   1.7904033 | 0.0038485 |   1.6706693 | 0.0055464 | 0.9999737 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0569441 | 145.6270424 | 0.0493124 |  30.1633305 | 0.0688727 | 0.9966620 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5101209 | 259.9796781 | 0.4417541 | 115.4458414 | 0.6354202 | 0.9976242 |
| healthyR.ts   |         3 | LM          | Test  | 0.0646135 | 197.8981154 | 0.0559539 |  33.0387074 | 0.0728073 | 0.9965128 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0283587 |  20.3828864 | 0.0245581 |  14.1839901 | 0.0485150 | 0.9976242 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0097943 |  30.2427544 | 0.0084816 |  15.2573995 | 0.0148958 | 0.9997837 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0410823 |  41.5450835 | 0.0378104 |  13.5262045 | 0.0501048 | 0.9966190 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4521674 | 177.2398621 | 0.4161560 | 101.7997695 | 0.5858429 | 0.9966227 |
| healthyverse  |         3 | LM          | Test  | 0.0492393 |  64.1448934 | 0.0453178 |  15.5134260 | 0.0578669 | 0.9965583 |
| healthyverse  |         4 | EARTH       | Test  | 0.0086768 |   4.1593278 | 0.0079857 |   3.3730158 | 0.0125414 | 0.9997756 |
| healthyverse  |         5 | NNAR        | Test  | 0.0041382 |   1.9743338 | 0.0038087 |   1.7673575 | 0.0073250 | 0.9999448 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0454802 |  34.0685293 | 0.0470720 |  14.8238883 | 0.0545118 | 0.9962091 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4486177 | 109.3200710 | 0.4643194 | 108.2541282 | 0.5686501 | 0.9963815 |
| healthyR.ai   |         3 | LM          | Test  | 0.0454478 |  33.9388921 | 0.0470385 |  14.7946656 | 0.0545120 | 0.9962093 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0124728 |   5.1753168 | 0.0129094 |   7.3827637 | 0.0149759 | 0.9997541 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0029562 |   0.7031748 | 0.0030596 |   0.7004725 | 0.0049566 | 0.9999739 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0267780 |  15.3468433 | 0.0262615 |  10.7648572 | 0.0309757 | 0.9988891 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4399240 | 156.3717415 | 0.4314391 | 100.9223675 | 0.5817802 | 0.9988904 |
| TidyDensity   |         3 | LM          | Test  | 0.0270608 |  25.6354197 | 0.0265389 |  13.6087297 | 0.0316325 | 0.9988761 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0078898 |   2.1651265 | 0.0077377 |   2.2480241 | 0.0124887 | 0.9998813 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0058519 |   4.4964255 | 0.0057390 |   6.4783573 | 0.0114922 | 0.9998487 |

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
    ##   package      .model_id .model_desc .type     mae   mape    mase  smape    rmse
    ##   <chr>            <int> <chr>       <chr>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
    ## 1 healthyR             3 LM          Test  0.0571  12.7   0.0485  11.4   0.0679 
    ## 2 healthyR.da…         5 NNAR        Test  0.00434  1.79  0.00385  1.67  0.00555
    ## 3 healthyR.ts          5 NNAR        Test  0.00979 30.2   0.00848 15.3   0.0149 
    ## 4 healthyverse         5 NNAR        Test  0.00414  1.97  0.00381  1.77  0.00733
    ## 5 healthyR.ai          5 NNAR        Test  0.00296  0.703 0.00306  0.700 0.00496
    ## 6 TidyDensity          5 NNAR        Test  0.00585  4.50  0.00574  6.48  0.0115 
    ## # … with 1 more variable: rsq <dbl>

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
    ## 1 healthyR      <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 2 healthyR.data <tibble>     <tibble [28 × 6]> <split [306|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [307|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [262|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [110|28]> <mdl_time_tbl>

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
