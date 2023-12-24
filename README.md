Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 December, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 85,538
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

The last day in the data set is 2023-12-20 23:21:03, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is
-1.285938^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 85538         |
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
| r_version     |     58655 |          0.31 |   5 |   5 |     0 |       40 |          0 |
| r_arch        |     58655 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     58655 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       49 |          0 |
| country       |      7097 |          0.92 |   2 |   2 |     0 |      149 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-12-20 | 2022-07-25 |     1123 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1252815.03 | 1612094.89 | 357 | 16733 | 313313.5 | 2400823 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10298.09 |   18238.17 |   1 |   188 |   2925.0 |   11441 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-12-20 23:21:03 | 2022-07-25 12:53:00 |    51779 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     12 |       60 |

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

    ## # A tibble: 7 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 TidyDensity   <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 2 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 3 healthyR.ai   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 4 healthyR.data <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 5 healthyR.ts   <tibble [542 × 2]> <tibble [28 × 2]> <split [514|28]>
    ## 6 healthyverse  <tibble [537 × 2]> <tibble [28 × 2]> <split [509|28]>
    ## 7 tidyAML       <tibble [308 × 2]> <tibble [28 × 2]> <split [280|28]>

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
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 TidyDensity   <tibble>     <tibble>     <split [521|28]> <mdl_tm_t [4 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [4 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [4 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [4 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [4 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [4 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [280|28]> <mdl_tm_t [4 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 0.5620269 |  655.17984 | 0.7849682 |  98.50008 | 0.6986306 | 0.0609383 |
| TidyDensity   |         2 | LM          | Test  | 0.5321242 |  560.07444 | 0.7432040 |  98.95543 | 0.6792520 | 0.0656381 |
| TidyDensity   |         3 | EARTH       | Test  | 1.6995256 | 1216.58357 | 2.3736831 | 183.92142 | 1.9922182 | 0.0656381 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5536538 |  265.33190 | 0.7732738 | 135.90238 | 0.7125095 | 0.0261293 |
| healthyR      |         1 | ARIMA       | Test  | 0.5542657 |  161.56856 | 0.5384855 | 116.32073 | 0.7191357 | 0.3802397 |
| healthyR      |         2 | LM          | Test  | 0.6988804 |   98.43424 | 0.6789830 | 185.40124 | 0.8851441 | 0.0857864 |
| healthyR      |         3 | EARTH       | Test  | 0.7123164 |  139.13690 | 0.6920364 | 152.64413 | 0.9076882 | 0.0857864 |
| healthyR      |         4 | NNAR        | Test  | 0.7507800 |  133.06591 | 0.7294050 | 166.12499 | 0.9355015 | 0.0036890 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6425112 |  174.78467 | 0.5950040 | 130.59361 | 0.8936358 | 0.2775769 |
| healthyR.ai   |         2 | LM          | Test  | 0.6856616 |  186.52145 | 0.6349638 | 138.98292 | 0.9363276 | 0.0578307 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.6845351 |  202.60463 | 0.6339207 | 135.14422 | 0.9359282 | 0.0578307 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7025464 |  143.17851 | 0.6506002 | 160.35668 | 0.9383361 | 0.0079312 |
| healthyR.data |         1 | ARIMA       | Test  | 0.5923993 |  237.93493 | 0.7234070 | 130.81527 | 0.7369924 | 0.0000902 |
| healthyR.data |         2 | LM          | Test  | 0.5790645 |   98.18022 | 0.7071233 | 177.07934 | 0.7101394 | 0.0023505 |
| healthyR.data |         3 | EARTH       | Test  | 0.6327802 |  289.52857 | 0.7727180 | 124.90856 | 0.7790043 | 0.0023505 |
| healthyR.data |         4 | NNAR        | Test  | 0.6267438 |  140.86456 | 0.7653468 | 180.48987 | 0.7519702 | 0.0576438 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6680566 |  365.69943 | 0.6473434 | 104.92047 | 0.9539797 | 0.0005949 |
| healthyR.ts   |         2 | LM          | Test  | 0.6619522 |  493.31646 | 0.6414283 |  96.86607 | 0.9253856 | 0.0014743 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6654086 |  511.10240 | 0.6447776 |  96.51720 | 0.9242674 | 0.0014743 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.6868483 |  166.76471 | 0.6655525 | 130.65669 | 1.0001789 | 0.0560990 |
| healthyverse  |         1 | ARIMA       | Test  | 0.4859901 |   95.72629 | 0.6120536 |  86.85730 | 0.6079537 | 0.2615862 |
| healthyverse  |         2 | LM          | Test  | 0.5506481 |  122.08097 | 0.6934836 |  87.70305 | 0.6881115 | 0.0434849 |
| healthyverse  |         3 | EARTH       | Test  | 0.5619198 |  129.68598 | 0.7076791 |  88.06158 | 0.6947539 | 0.0434849 |
| healthyverse  |         4 | NNAR        | Test  | 0.6136151 |  139.71509 | 0.7727839 |  93.85386 | 0.7508402 | 0.0683313 |
| tidyAML       |         1 | ARIMA       | Test  | 1.2073402 |  262.19878 | 1.5523571 | 152.10696 | 1.4264491 | 0.0000028 |
| tidyAML       |         2 | LM          | Test  | 1.4188610 |  375.58648 | 1.8243233 | 150.18333 | 1.6699837 | 0.0096616 |
| tidyAML       |         3 | EARTH       | Test  | 1.1762623 |  244.72729 | 1.5123981 | 150.65718 | 1.4030285 | 0.0096616 |
| tidyAML       |         4 | NNAR        | Test  | 1.0922991 |  184.14570 | 1.4044411 | 152.88296 | 1.3190134 | 0.0216536 |

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
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 TidyDensity          2 LM          Test  0.532 560.  0.743  99.0 0.679 0.0656 
    ## 2 healthyR             1 ARIMA       Test  0.554 162.  0.538 116.  0.719 0.380  
    ## 3 healthyR.ai          1 ARIMA       Test  0.643 175.  0.595 131.  0.894 0.278  
    ## 4 healthyR.da…         2 LM          Test  0.579  98.2 0.707 177.  0.710 0.00235
    ## 5 healthyR.ts          3 EARTH       Test  0.665 511.  0.645  96.5 0.924 0.00147
    ## 6 healthyverse         1 ARIMA       Test  0.486  95.7 0.612  86.9 0.608 0.262  
    ## 7 tidyAML              4 NNAR        Test  1.09  184.  1.40  153.  1.32  0.0217

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
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 TidyDensity   <tibble>     <tibble>     <split [521|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [1 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [280|28]> <mdl_tm_t [1 × 5]>

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
