Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
08 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,138
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

The last day in the data set is 2022-07-06 22:44:41, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -90.77 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41138         |
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
| r_version     |     27604 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     27604 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     27604 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3296 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-06 | 2021-11-06 |      591 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |        p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|-----------:|--------:|:------|
| size          |         0 |             1 | 1425290.35 | 1797410.55 | 357 | 16873 | 289680 | 2974676.50 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8733.72 |   16761.32 |   1 |   167 |   2622 |    8670.25 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-06 22:44:41 | 2021-11-06 16:17:02 |    23995 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     15 |       60 |

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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [263|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [111|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0542116 |  11.8759847 | 0.0415291 |  10.6838972 | 0.0671492 | 0.9963026 |
| healthyR      |         2 | GLMNET      | Test  | 0.5613225 |  66.2979825 | 0.4300039 | 102.8255652 | 0.7410421 | 0.9967969 |
| healthyR      |         3 | LM          | Test  | 0.0545496 |  12.4139850 | 0.0417880 |  11.2539254 | 0.0655774 | 0.9963082 |
| healthyR      |         4 | EARTH       | Test  | 0.0395178 |   4.3987094 | 0.0302728 |   4.3647783 | 0.0788505 | 0.9967969 |
| healthyR      |         5 | NNAR        | Test  | 0.0321299 |   2.7738698 | 0.0246133 |   2.7003152 | 0.1115120 | 0.9915258 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0529383 |  38.2979040 | 0.0449196 |  16.7339920 | 0.0627823 | 0.9966968 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4400905 |  98.8246192 | 0.3734294 |  92.4271197 | 0.5493815 | 0.9989006 |
| healthyR.data |         3 | LM          | Test  | 0.0522334 |  37.4685007 | 0.0443216 |  16.4895270 | 0.0620301 | 0.9967016 |
| healthyR.data |         4 | EARTH       | Test  | 0.0214321 |  13.3650253 | 0.0181857 |   8.1398857 | 0.0283769 | 0.9989006 |
| healthyR.data |         5 | NNAR        | Test  | 0.0087573 |   2.4843116 | 0.0074308 |   2.3594281 | 0.0116953 | 0.9999217 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0526915 |  89.4909937 | 0.0415456 |  27.6601493 | 0.0641786 | 0.9969078 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5235881 | 177.2884183 | 0.4128333 | 115.8046947 | 0.6471715 | 0.9977641 |
| healthyR.ts   |         3 | LM          | Test  | 0.0636667 | 124.9052587 | 0.0501993 |  31.7412863 | 0.0715914 | 0.9967464 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0276688 |  14.0036556 | 0.0218160 |  11.1581533 | 0.0481383 | 0.9977641 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0087485 |   7.9395019 | 0.0068979 |   9.9046035 | 0.0164249 | 0.9997699 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0363492 |  27.3444993 | 0.0317906 |  11.9825282 | 0.0447293 | 0.9970897 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4434305 | 131.1240099 | 0.3878192 | 102.9078773 | 0.5772376 | 0.9965497 |
| healthyverse  |         3 | LM          | Test  | 0.0464600 |  42.1128743 | 0.0406334 |  14.7521103 | 0.0548766 | 0.9970982 |
| healthyverse  |         4 | EARTH       | Test  | 0.0084993 |   3.0766369 | 0.0074334 |   2.7373578 | 0.0124413 | 0.9997670 |
| healthyverse  |         5 | NNAR        | Test  | 0.0032056 |   1.8465987 | 0.0028036 |   2.0407777 | 0.0044994 | 0.9999797 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0438883 |  23.8154711 | 0.0429999 |  14.0106951 | 0.0526500 | 0.9965797 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4604617 |  92.1847067 | 0.4511407 | 108.4771574 | 0.5818007 | 0.9966305 |
| healthyR.ai   |         3 | LM          | Test  | 0.0441504 |  24.5249108 | 0.0432566 |  14.2687032 | 0.0526941 | 0.9965783 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0121842 |   3.8040245 | 0.0119376 |   4.3094815 | 0.0146338 | 0.9997414 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0028560 |   0.9597332 | 0.0027982 |   0.9815254 | 0.0048774 | 0.9999699 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0253691 |  10.5170012 | 0.0227397 |   8.7207600 | 0.0295770 | 0.9990010 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4626029 | 120.8660782 | 0.4146576 | 102.2307766 | 0.6099586 | 0.9989808 |
| TidyDensity   |         3 | LM          | Test  | 0.0267635 |  18.4803607 | 0.0239897 |  12.0552497 | 0.0314423 | 0.9989828 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0073488 |   2.2628386 | 0.0065872 |   2.1870277 | 0.0106453 | 0.9999028 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0068969 |   6.5316622 | 0.0061821 |  10.3984844 | 0.0107505 | 0.9998596 |

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
    ## 1 healthyR             3 LM          Test  0.0545  12.4   0.0418  11.3   0.0656 
    ## 2 healthyR.da…         5 NNAR        Test  0.00876  2.48  0.00743  2.36  0.0117 
    ## 3 healthyR.ts          5 NNAR        Test  0.00875  7.94  0.00690  9.90  0.0164 
    ## 4 healthyverse         5 NNAR        Test  0.00321  1.85  0.00280  2.04  0.00450
    ## 5 healthyR.ai          5 NNAR        Test  0.00286  0.960 0.00280  0.982 0.00488
    ## 6 TidyDensity          4 EARTH       Test  0.00735  2.26  0.00659  2.19  0.0106 
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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [263|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [111|28]> <mdl_time_tbl>

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
