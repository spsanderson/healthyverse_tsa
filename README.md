Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
02 June, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 38,328
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

The last day in the data set is 2022-05-31 23:26:44, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4398.8
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 38328         |
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
| r_version     |     25726 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     25726 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     25726 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      3058 |          0.92 |   2 |   2 |     0 |      109 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-05-31 | 2021-10-27 |      555 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1447354.62 | 1821946.87 | 357 | 16873 | 271700.5 | 3241513 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8713.83 |   16718.33 |   1 |   186 |   2660.0 |    8617 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-05-31 23:26:44 | 2021-10-27 08:16:22 |    22301 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |    2.5 |       60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [496|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [488|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [437|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [413|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [227|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [75|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|------------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0470890 | 13.2237588 | 0.0323453 |  13.5212462 | 0.0598303 | 0.9969446 |
| healthyR.data |         2 | GLMNET      | Test  | 0.5697815 | 86.1270658 | 0.3913812 | 110.1006163 | 0.6940409 | 0.9983647 |
| healthyR.data |         3 | LM          | Test  | 0.0551246 | 17.7451505 | 0.0378649 |  12.8419037 | 0.0708155 | 0.9952538 |
| healthyR.data |         4 | EARTH       | Test  | 0.0310874 |  7.8923945 | 0.0213539 |   9.0054559 | 0.0435588 | 0.9979924 |
| healthyR.data |         5 | NNAR        | Test  | 0.0651357 |  8.1657012 | 0.0447415 |   7.2909394 | 0.1753867 | 0.9734095 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0492246 | 13.8323203 | 0.0367891 |  13.9000206 | 0.0661790 | 0.9963294 |
| healthyR      |         2 | GLMNET      | Test  | 0.5855097 | 71.8292283 | 0.4375942 | 110.5803941 | 0.7623697 | 0.9958934 |
| healthyR      |         3 | LM          | Test  | 0.0496035 | 14.1686074 | 0.0370723 |  14.4849839 | 0.0663028 | 0.9963284 |
| healthyR      |         4 | EARTH       | Test  | 0.0290965 |  2.8236889 | 0.0217460 |   2.7486513 | 0.0776842 | 0.9971373 |
| healthyR      |         5 | NNAR        | Test  | 0.0241512 |  3.3442886 | 0.0180500 |   3.3648652 | 0.0531875 | 0.9982156 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0441647 |  8.1416513 | 0.0306824 |   7.4162927 | 0.0530140 | 0.9981256 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5812426 | 62.9688273 | 0.4038058 |  92.7103051 | 0.7162509 | 0.9977580 |
| healthyR.ts   |         3 | LM          | Test  | 0.0429372 |  8.0214964 | 0.0298297 |   7.3978169 | 0.0516474 | 0.9981329 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0155954 |  2.5704843 | 0.0108346 |   2.5199448 | 0.0188607 | 0.9997504 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0090837 |  1.5301869 | 0.0063107 |   1.5443107 | 0.0132066 | 0.9998606 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0409772 |  7.6988032 | 0.0341438 |   7.1932197 | 0.0500888 | 0.9981291 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4902124 | 90.9939460 | 0.4084644 |  98.8063611 | 0.6050581 | 0.9981131 |
| healthyverse  |         3 | LM          | Test  | 0.0393536 |  7.5237697 | 0.0327909 |   7.1568725 | 0.0479833 | 0.9981553 |
| healthyverse  |         4 | EARTH       | Test  | 0.0112960 |  2.4307274 | 0.0094123 |   2.4085589 | 0.0156844 | 0.9996869 |
| healthyverse  |         5 | NNAR        | Test  | 0.0060387 |  0.8096552 | 0.0050317 |   0.8089761 | 0.0094115 | 0.9999088 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0420227 |  9.1707933 | 0.0304583 |   8.9176276 | 0.0497018 | 0.9983374 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.5225170 | 61.6665068 | 0.3787232 |  90.2538172 | 0.6497297 | 0.9986167 |
| healthyR.ai   |         3 | LM          | Test  | 0.0420645 |  9.4955174 | 0.0304886 |   9.3549622 | 0.0496845 | 0.9983387 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0157494 |  2.3385073 | 0.0114152 |   2.3055761 | 0.0191886 | 0.9997899 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0047405 |  0.5492228 | 0.0034360 |   0.5500214 | 0.0088290 | 0.9999274 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0423241 | 14.7239104 | 0.0422120 |  12.7937733 | 0.0536409 | 0.9985575 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.3884514 | 60.1702507 | 0.3874228 |  86.2143887 | 0.4861478 | 0.9988709 |
| TidyDensity   |         3 | LM          | Test  | 0.0455089 | 16.2908506 | 0.0453884 |  14.3438035 | 0.0569948 | 0.9985783 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0085802 |  1.0818029 | 0.0085575 |   1.0812363 | 0.0144983 | 0.9996814 |
| TidyDensity   |         5 | NNAR        | Test  | 0.1038242 | 26.3444075 | 0.1035493 |  28.0427120 | 0.1293049 | 0.9883631 |

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
    ## 1 healthy…         4 EARTH       Test  0.0311  7.89  0.0214  9.01  0.0436  0.998
    ## 2 healthyR         5 NNAR        Test  0.0242  3.34  0.0180  3.36  0.0532  0.998
    ## 3 healthy…         5 NNAR        Test  0.00908 1.53  0.00631 1.54  0.0132  1.00 
    ## 4 healthy…         5 NNAR        Test  0.00604 0.810 0.00503 0.809 0.00941 1.00 
    ## 5 healthy…         5 NNAR        Test  0.00474 0.549 0.00344 0.550 0.00883 1.00 
    ## 6 TidyDen…         4 EARTH       Test  0.00858 1.08  0.00856 1.08  0.0145  1.00

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
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [496|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [488|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [437|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [413|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [227|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [75|28]>  <mdl_time_tbl>

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
