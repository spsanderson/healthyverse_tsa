Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
19 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 111,348
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

The last day in the data set is 2024-08-17 23:57:00, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -252.35
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 111348        |
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
| r_version     |     78054 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78054 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78054 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9541 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-17 | 2023-01-26 |     1364 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1180264.80 | 1560449.34 | 355 | 14701 | 280846 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10318.82 |   18033.81 |   1 |   317 |   3091 |   11495 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-17 23:57:00 | 2023-01-26 09:58:05 |    67392 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   26.5 |       60 |

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

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,335 × 2]> <tibble [28 × 2]> <split [1307|28]>
    ## 2 healthyR      <tibble [1,327 × 2]> <tibble [28 × 2]> <split [1299|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,273 × 2]> <tibble [28 × 2]> <split [1245|28]>
    ## 5 healthyverse  <tibble [1,244 × 2]> <tibble [28 × 2]> <split [1216|28]>
    ## 6 healthyR.ai   <tibble [1,070 × 2]> <tibble [28 × 2]> <split [1042|28]>
    ## 7 TidyDensity   <tibble [924 × 2]>   <tibble [28 × 2]> <split [896|28]> 
    ## 8 tidyAML       <tibble [540 × 2]>   <tibble [28 × 2]> <split [512|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

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
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.5918895 |  144.22381 | 0.6530259 | 153.26364 | 0.7772022 | 0.1527443 |
| healthyR.data |         2 | LM          | Test  | 0.8210452 |  368.38061 | 0.9058512 | 161.89629 | 0.9550081 | 0.0497278 |
| healthyR.data |         3 | EARTH       | Test  | 0.5924070 |  181.27029 | 0.6535969 | 116.34152 | 0.8444990 | 0.0497278 |
| healthyR.data |         4 | NNAR        | Test  | 0.5721986 |  121.60692 | 0.6313012 | 129.69242 | 0.7958778 | 0.1111210 |
| healthyR      |         1 | ARIMA       | Test  | 0.6719732 |  108.39169 | 0.8543623 | 139.91509 | 0.8550804 | 0.0672595 |
| healthyR      |         2 | LM          | Test  | 0.7655866 |  122.70647 | 0.9733846 | 170.88245 | 0.9770764 | 0.0672049 |
| healthyR      |         3 | EARTH       | Test  | 0.6727419 |  124.89031 | 0.8553398 | 132.26610 | 0.8507256 | 0.0672049 |
| healthyR      |         4 | NNAR        | Test  | 0.7392942 |  119.25510 | 0.9399558 | 168.51799 | 0.9438864 | 0.0249276 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.3082479 |  443.44955 | 1.0491309 | 156.06671 | 1.5853337 | 0.0000191 |
| healthyR.ts   |         2 | LM          | Test  | 0.8995527 |  236.44210 | 0.7213836 | 130.38359 | 1.1226405 | 0.0912726 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.8994774 |  241.76000 | 0.7213232 | 129.53988 | 1.1223167 | 0.0912726 |
| healthyR.ts   |         4 | NNAR        | Test  | 1.0048949 |  143.52840 | 0.8058613 | 180.36549 | 1.2494347 | 0.0048501 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7317812 |  474.07172 | 1.0116420 | 117.50463 | 0.8414497 | 0.2639261 |
| healthyverse  |         2 | LM          | Test  | 0.7722018 |  584.83405 | 1.0675210 | 111.83319 | 0.9057601 | 0.0000290 |
| healthyverse  |         3 | EARTH       | Test  | 0.7570408 |  415.88076 | 1.0465618 | 122.76732 | 0.8856120 | 0.0000290 |
| healthyverse  |         4 | NNAR        | Test  | 0.7573555 |  283.54250 | 1.0469969 | 136.81805 | 0.9282504 | 0.0001021 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.0229602 |  118.75637 | 0.9444389 | 157.43378 | 1.4122398 | 0.1266338 |
| healthyR.ai   |         2 | LM          | Test  | 0.9992485 |  147.82860 | 0.9225473 | 141.02554 | 1.3953151 | 0.0346688 |
| healthyR.ai   |         3 | EARTH       | Test  | 3.3193844 | 1458.76016 | 3.0645921 | 151.06545 | 3.6538985 | 0.0346688 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.9835539 |  123.81046 | 0.9080574 | 160.01304 | 1.2885112 | 0.0288074 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5933145 |  248.62859 | 0.6997806 |  98.52920 | 0.7055574 | 0.4288941 |
| TidyDensity   |         2 | LM          | Test  | 0.6636498 |  269.76241 | 0.7827371 |  99.42475 | 0.8086149 | 0.0296938 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8386390 |  172.75535 | 0.9891268 | 176.01915 | 1.0206041 | 0.0296938 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6490950 |  104.44816 | 0.7655706 | 139.76851 | 0.7786621 | 0.1756825 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6814722 |   95.38443 | 0.7565816 | 122.34330 | 0.8426106 | 0.3056031 |
| tidyAML       |         2 | LM          | Test  | 0.7775483 |  123.29824 | 0.8632470 | 123.16690 | 0.9444820 | 0.0292647 |
| tidyAML       |         3 | EARTH       | Test  | 2.1742708 |  492.91050 | 2.4139112 | 164.18312 | 2.4545761 | 0.0292647 |
| tidyAML       |         4 | NNAR        | Test  | 0.7167514 |  124.13698 | 0.7957492 | 112.30845 | 0.8935873 | 0.1295938 |

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
    ##   # A tibble: 7 × 10
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         1 ARIMA       Test  0.592 144.  0.653 153.  0.777 0.153 
    ## 2 healthyR              3 EARTH       Test  0.673 125.  0.855 132.  0.851 0.0672
    ## 3 healthyR.ts           3 EARTH       Test  0.899 242.  0.721 130.  1.12  0.0913
    ## 4 healthyverse          1 ARIMA       Test  0.732 474.  1.01  118.  0.841 0.264 
    ## 5 healthyR.ai           4 NNAR        Test  0.984 124.  0.908 160.  1.29  0.0288
    ## 6 TidyDensity           1 ARIMA       Test  0.593 249.  0.700  98.5 0.706 0.429 
    ## 7 tidyAML               1 ARIMA       Test  0.681  95.4 0.757 122.  0.843 0.306

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
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
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1307|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1299|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1245|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1216|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1042|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [896|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [512|28]>  <mdl_tm_t [1 × 5]>

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
