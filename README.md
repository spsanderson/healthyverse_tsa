Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
22 July, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 72,561
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

The last day in the data set is 2023-07-19 20:56:47, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -9160.97
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 72561         |
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
| r_version     |     49602 |          0.32 |   5 |   5 |     0 |       38 |          0 |
| r_arch        |     49602 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     49602 |          0.32 |   7 |  15 |     0 |       15 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       45 |          0 |
| country       |      6108 |          0.92 |   2 |   2 |     0 |      139 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-07-19 | 2022-05-06 |      969 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1292479.02 | 1647276.07 | 357 | 17597 | 322873 | 2670896 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9975.71 |   18014.65 |   1 |   143 |   2793 |   11005 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-07-19 20:56:47 | 2022-05-06 18:37:53 |    43550 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 57M 7S |       60 |

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
    ## 1 healthyR      <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 2 healthyR.ai   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.data <tibble [542 × 2]> <tibble [28 × 2]> <split [514|28]>
    ## 4 healthyR.ts   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 5 healthyverse  <tibble [534 × 2]> <tibble [28 × 2]> <split [506|28]>
    ## 6 TidyDensity   <tibble [545 × 2]> <tibble [28 × 2]> <split [517|28]>
    ## 7 tidyAML       <tibble [154 × 2]> <tibble [28 × 2]> <split [126|28]>

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 healthyR      <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [506|28]> <mdl_tm_t [5 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [126|28]> <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |     mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|---------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.6839026 | 126.3021 | 0.7444089 | 168.43494 | 0.8595239 | 0.0462272 |
| healthyR      |         2 | NULL        | NA    |        NA |       NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.6682459 | 113.4755 | 0.7273670 | 160.46435 | 0.8828391 | 0.0127266 |
| healthyR      |         4 | EARTH       | Test  | 0.7249690 | 252.2272 | 0.7891085 | 123.24946 | 1.0110300 | 0.0127266 |
| healthyR      |         5 | NNAR        | Test  | 0.6706993 | 116.9645 | 0.7300375 | 167.06348 | 0.8716285 | 0.0158551 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6779066 | 193.7767 | 0.7103003 | 131.36339 | 0.8180317 | 0.3228380 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |       NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7577335 | 124.8530 | 0.7939417 | 188.27161 | 0.9364597 | 0.0002780 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.5859890 | 145.2935 | 0.6139905 |  97.40601 | 0.8786031 | 0.0002780 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.6515785 | 132.0286 | 0.6827141 | 139.80032 | 0.8601935 | 0.0607018 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6588278 | 106.9657 | 0.7785761 | 149.71439 | 0.8224607 | 0.0548619 |
| healthyR.data |         2 | NULL        | NA    |        NA |       NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.6754018 | 125.4069 | 0.7981626 | 137.83584 | 0.8491813 | 0.1023046 |
| healthyR.data |         4 | EARTH       | Test  | 0.6658966 | 121.3884 | 0.7869297 | 139.21401 | 0.8359578 | 0.1023046 |
| healthyR.data |         5 | NNAR        | Test  | 0.6847743 | 113.2615 | 0.8092387 | 147.96388 | 0.8495897 | 0.0217929 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9410712 | 118.1863 | 0.7290473 | 132.48341 | 1.2798129 | 0.0002221 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |       NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.9087286 | 132.7193 | 0.7039915 | 121.85991 | 1.2461340 | 0.0303577 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.0945715 | 120.3008 | 0.8479639 | 175.45259 | 1.4656888 | 0.0303577 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.0200164 | 102.3565 | 0.7902061 | 168.79121 | 1.3638067 | 0.0004116 |
| healthyverse  |         1 | ARIMA       | Test  | 0.4595510 | 142.9543 | 0.6490840 |  78.74772 | 0.6129521 |        NA |
| healthyverse  |         2 | NULL        | NA    |        NA |       NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.4593798 | 145.5548 | 0.6488423 |  77.97255 | 0.6173923 | 0.0015066 |
| healthyverse  |         4 | EARTH       | Test  | 0.5915663 | 210.2768 | 0.8355465 |  83.12294 | 0.7643089 | 0.0015066 |
| healthyverse  |         5 | NNAR        | Test  | 0.5228471 | 102.6323 | 0.7384855 | 113.84372 | 0.6240068 | 0.0152299 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6392388 | 166.8311 | 0.7146764 | 127.93031 | 0.8421950 | 0.0509299 |
| TidyDensity   |         2 | NULL        | NA    |        NA |       NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6884600 | 222.2192 | 0.7697063 | 127.35710 | 0.8722299 | 0.0095293 |
| TidyDensity   |         4 | EARTH       | Test  | 0.6353956 | 145.6766 | 0.7103797 | 131.53497 | 0.8472148 | 0.0095293 |
| TidyDensity   |         5 | NNAR        | Test  | 0.6613267 | 150.8697 | 0.7393710 | 157.94049 | 0.8607018 | 0.0100503 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5073362 | 110.7099 | 0.7298736 | 100.26482 | 0.6849226 | 0.0551130 |
| tidyAML       |         2 | NULL        | NA    |        NA |       NA |        NA |        NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.4897839 | 197.5504 | 0.7046223 |  82.09552 | 0.6238800 | 0.0001011 |
| tidyAML       |         4 | EARTH       | Test  | 0.6994307 | 373.0087 | 1.0062283 |  89.34006 | 0.8320755 | 0.0001011 |
| tidyAML       |         5 | NNAR        | Test  | 0.4722293 | 121.5163 | 0.6793675 |  94.69097 | 0.6137290 | 0.1491036 |

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
    ## 1 healthyR             1 ARIMA       Test  0.684  126. 0.744 168.  0.860  0.0462
    ## 2 healthyR.ai          1 ARIMA       Test  0.678  194. 0.710 131.  0.818  0.323 
    ## 3 healthyR.da…         1 ARIMA       Test  0.659  107. 0.779 150.  0.822  0.0549
    ## 4 healthyR.ts          3 LM          Test  0.909  133. 0.704 122.  1.25   0.0304
    ## 5 healthyverse         1 ARIMA       Test  0.460  143. 0.649  78.7 0.613 NA     
    ## 6 TidyDensity          1 ARIMA       Test  0.639  167. 0.715 128.  0.842  0.0509
    ## 7 tidyAML              5 NNAR        Test  0.472  122. 0.679  94.7 0.614  0.149

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
    ## 1 healthyR      <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [506|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [126|28]> <mdl_tm_t [1 × 5]>

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
