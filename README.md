Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
22 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 111,596
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

The last day in the data set is 2024-08-20 21:23:47, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -321.8 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 111596        |
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
| r_version     |     78254 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78254 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78254 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9548 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-20 | 2023-01-28 |     1367 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1180573.58 | 1560527.61 | 355 | 14701 | 286925 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10315.12 |   18027.21 |   1 |   317 |   3081 |   11494 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-20 21:23:47 | 2023-01-28 10:38:22 |    67559 |

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

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,338 × 2]> <tibble [28 × 2]> <split [1310|28]>
    ## 2 healthyR      <tibble [1,330 × 2]> <tibble [28 × 2]> <split [1302|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,276 × 2]> <tibble [28 × 2]> <split [1248|28]>
    ## 5 healthyverse  <tibble [1,247 × 2]> <tibble [28 × 2]> <split [1219|28]>
    ## 6 healthyR.ai   <tibble [1,073 × 2]> <tibble [28 × 2]> <split [1045|28]>
    ## 7 TidyDensity   <tibble [927 × 2]>   <tibble [28 × 2]> <split [899|28]> 
    ## 8 tidyAML       <tibble [543 × 2]>   <tibble [28 × 2]> <split [515|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.5126980 |  105.02577 | 0.6390606 | 129.03939 | 0.6962574 | 0.1180358 |
| healthyR.data |         2 | LM          | Test  | 0.7922782 |  308.70700 | 0.9875478 | 161.95652 | 0.9100714 | 0.1085936 |
| healthyR.data |         3 | EARTH       | Test  | 0.5224594 |  145.30242 | 0.6512279 | 114.99734 | 0.7278843 | 0.1085936 |
| healthyR.data |         4 | NNAR        | Test  | 0.5202040 |  106.71880 | 0.6484167 | 126.61684 | 0.7275335 | 0.0185306 |
| healthyR      |         1 | ARIMA       | Test  | 0.6154868 |   87.67839 | 0.8133271 | 128.77041 | 0.8100901 | 0.0789151 |
| healthyR      |         2 | LM          | Test  | 0.7156161 |  106.40204 | 0.9456417 | 167.20905 | 0.9329923 | 0.0513629 |
| healthyR      |         3 | EARTH       | Test  | 0.6463001 |   93.68247 | 0.8540449 | 137.98638 | 0.8405370 | 0.0513629 |
| healthyR      |         4 | NNAR        | Test  | 0.7021325 |  113.05117 | 0.9278239 | 163.68306 | 0.8990338 | 0.0239505 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7809259 |  262.04606 | 0.6660828 | 114.79844 | 1.0199235 | 0.0934845 |
| healthyR.ts   |         2 | LM          | Test  | 0.7966453 |  227.36287 | 0.6794905 | 118.02537 | 1.0517603 | 0.0334983 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7957204 |  232.52394 | 0.6787016 | 117.14900 | 1.0507029 | 0.0334983 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8759382 |  122.70562 | 0.7471226 | 183.69433 | 1.1121580 | 0.0497067 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6525075 |  426.91825 | 0.8466343 | 116.64283 | 0.7539853 | 0.4135984 |
| healthyverse  |         2 | LM          | Test  | 0.7404325 |  603.01289 | 0.9607178 | 111.90831 | 0.8605746 | 0.0026732 |
| healthyverse  |         3 | EARTH       | Test  | 0.7245433 |  417.48974 | 0.9401014 | 123.82455 | 0.8513231 | 0.0026732 |
| healthyverse  |         4 | NNAR        | Test  | 0.7071214 |  282.17342 | 0.9174964 | 137.59480 | 0.8624337 | 0.1250645 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8793399 |  103.96018 | 0.8503679 | 170.83624 | 1.1272255 | 0.1441594 |
| healthyR.ai   |         2 | LM          | Test  | 0.7810153 |  136.69562 | 0.7552829 | 133.76102 | 1.0064461 | 0.0138153 |
| healthyR.ai   |         3 | EARTH       | Test  | 8.2967967 | 3469.64360 | 8.0234389 | 179.53344 | 8.9120319 | 0.0138153 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.9072093 |  144.33425 | 0.8773192 | 155.70723 | 1.1794556 | 0.2487777 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5686730 |  241.90337 | 0.7381339 |  93.01183 | 0.6823255 | 0.5195661 |
| TidyDensity   |         2 | LM          | Test  | 0.6250103 |  259.40711 | 0.8112594 |  93.58662 | 0.7790215 | 0.0081831 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8386431 |  178.47483 | 1.0885534 | 184.05866 | 1.0023391 | 0.0081831 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6356061 |   98.36970 | 0.8250128 | 138.17803 | 0.7843098 | 0.1691444 |
| tidyAML       |         1 | ARIMA       | Test  | 0.7642110 |  114.36600 | 0.9198960 | 139.05383 | 0.9241792 | 0.0760555 |
| tidyAML       |         2 | LM          | Test  | 0.7459534 |  116.23214 | 0.8979190 | 119.59783 | 0.8925089 | 0.0113782 |
| tidyAML       |         3 | EARTH       | Test  | 3.6233899 |  781.40867 | 4.3615465 | 178.37131 | 3.9466664 | 0.0113782 |
| tidyAML       |         4 | NNAR        | Test  | 0.6178815 |   98.89371 | 0.7437563 | 108.63503 | 0.8241888 | 0.1928481 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.513 105.  0.639 129.  0.696 0.118 
    ## 2 healthyR              1 ARIMA       Test  0.615  87.7 0.813 129.  0.810 0.0789
    ## 3 healthyR.ts           1 ARIMA       Test  0.781 262.  0.666 115.  1.02  0.0935
    ## 4 healthyverse          1 ARIMA       Test  0.653 427.  0.847 117.  0.754 0.414 
    ## 5 healthyR.ai           2 LM          Test  0.781 137.  0.755 134.  1.01  0.0138
    ## 6 TidyDensity           1 ARIMA       Test  0.569 242.  0.738  93.0 0.682 0.520 
    ## 7 tidyAML               4 NNAR        Test  0.618  98.9 0.744 109.  0.824 0.193

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1310|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1302|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1248|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1219|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1045|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [899|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [515|28]>  <mdl_tm_t [1 × 5]>

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
