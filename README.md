Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
29 June, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 70,497
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

The last day in the data set is 2023-06-27 18:56:59, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -8630.98
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 70497         |
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
| r_version     |     48147 |          0.32 |   5 |   5 |     0 |       38 |          0 |
| r_arch        |     48147 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     48147 |          0.32 |   7 |  15 |     0 |       15 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       45 |          0 |
| country       |      5916 |          0.92 |   2 |   2 |     0 |      139 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-06-27 | 2022-04-27 |      947 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1298915.02 | 1652565.58 | 357 | 24837 | 323133 | 2686662 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9950.25 |   18109.17 |   1 |   135 |   2732 |   10899 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-06-27 18:56:59 | 2022-04-27 14:30:44 |    42376 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 55M 55S |       60 |

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
    ## 1 healthyR      <tibble [545 × 2]> <tibble [28 × 2]> <split [517|28]>
    ## 2 healthyR.ai   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 4 healthyR.ts   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 5 healthyverse  <tibble [535 × 2]> <tibble [28 × 2]> <split [507|28]>
    ## 6 TidyDensity   <tibble [523 × 2]> <tibble [28 × 2]> <split [495|28]>
    ## 7 tidyAML       <tibble [132 × 2]> <tibble [28 × 2]> <split [104|28]>

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
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [5 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [495|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [104|28]> <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |        mae |       mape |       mase |     smape |       rmse |       rsq |
|:--------------|----------:|:------------|:------|-----------:|-----------:|-----------:|----------:|-----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  |  0.6705606 |  102.47951 |  0.8085000 | 155.51512 |  0.7939609 | 0.0983727 |
| healthyR      |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| healthyR      |         3 | LM          | Test  |  0.6864081 |   97.22217 |  0.8276074 | 172.91689 |  0.8096688 | 0.0002384 |
| healthyR      |         4 | EARTH       | Test  |  0.7739296 |  164.45916 |  0.9331328 | 130.23491 |  0.9465571 | 0.0002384 |
| healthyR      |         5 | NNAR        | Test  |  0.6747592 |  103.84406 |  0.8135623 | 151.98455 |  0.8196165 | 0.0091175 |
| healthyR.ai   |         1 | ARIMA       | Test  |  0.7410498 |  121.48641 |  0.8344363 | 148.25463 |  0.9131322 | 0.0637747 |
| healthyR.ai   |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| healthyR.ai   |         3 | LM          | Test  |  0.8240210 |  151.74463 |  0.9278634 | 175.69129 |  0.9641372 | 0.0067614 |
| healthyR.ai   |         4 | EARTH       | Test  |  0.9226319 |  414.80882 |  1.0389012 | 110.02872 |  1.1529671 | 0.0067614 |
| healthyR.ai   |         5 | NNAR        | Test  |  0.7792957 |  160.01516 |  0.8775019 | 142.88368 |  0.9567171 | 0.0004221 |
| healthyR.data |         1 | ARIMA       | Test  |  0.6437373 |  122.02780 |  0.6581316 | 171.62601 |  0.8042915 | 0.0064245 |
| healthyR.data |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| healthyR.data |         3 | LM          | Test  |  0.6332559 |  126.54003 |  0.6474158 | 135.44733 |  0.7878767 | 0.0110195 |
| healthyR.data |         4 | EARTH       | Test  |  1.1108137 |  437.68117 |  1.1356520 | 133.13775 |  1.3103698 | 0.0110195 |
| healthyR.data |         5 | NNAR        | Test  |  0.6619209 |  121.53731 |  0.6767218 | 151.48103 |  0.8202404 | 0.0698134 |
| healthyR.ts   |         1 | ARIMA       | Test  |  0.9097376 |   87.55084 |  0.7152456 | 131.93701 |  1.2473298 | 0.0019264 |
| healthyR.ts   |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| healthyR.ts   |         3 | LM          | Test  |  0.8836292 |   89.54335 |  0.6947189 | 113.37420 |  1.1416710 | 0.0227349 |
| healthyR.ts   |         4 | EARTH       | Test  |  0.9298624 |   93.91757 |  0.7310679 | 151.46031 |  1.1242670 | 0.0227349 |
| healthyR.ts   |         5 | NNAR        | Test  |  0.9335854 |   97.25617 |  0.7339950 | 182.31559 |  1.1342224 | 0.0022767 |
| healthyverse  |         1 | ARIMA       | Test  |  0.5577843 |  153.04546 |  0.7890260 | 100.39782 |  0.7059932 | 0.0002305 |
| healthyverse  |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| healthyverse  |         3 | LM          | Test  |  0.5044668 |  186.49291 |  0.7136045 |  84.15483 |  0.6568474 | 0.0001268 |
| healthyverse  |         4 | EARTH       | Test  |  0.7497861 |  399.69725 |  1.0606263 |  87.63638 |  0.8700324 | 0.0001268 |
| healthyverse  |         5 | NNAR        | Test  |  0.6386424 |  132.27674 |  0.9034056 | 130.51993 |  0.7858928 | 0.0011673 |
| TidyDensity   |         1 | ARIMA       | Test  |  0.4492961 |  111.81084 |  0.7275271 |  97.62881 |  0.5833706 | 0.1916456 |
| TidyDensity   |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| TidyDensity   |         3 | LM          | Test  |  0.4659670 |  128.50216 |  0.7545216 |  95.65383 |  0.6111526 | 0.1114714 |
| TidyDensity   |         4 | EARTH       | Test  |  4.4805268 | 1495.94631 |  7.2551367 | 191.97216 |  5.0130370 | 0.1114714 |
| TidyDensity   |         5 | NNAR        | Test  |  0.5343064 |  103.57294 |  0.8651808 | 136.95593 |  0.6816603 | 0.0592783 |
| tidyAML       |         1 | ARIMA       | Test  |  0.5786851 |  107.13397 |  0.9232700 | 136.80231 |  0.7136113 | 0.0016150 |
| tidyAML       |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| tidyAML       |         3 | LM          | Test  |  0.4947674 |   78.07022 |  0.7893825 |  96.23827 |  0.6611328 | 0.0154544 |
| tidyAML       |         4 | EARTH       | Test  |  0.6364436 |  230.99325 |  1.0154215 |  83.20458 |  0.7017339 | 0.0154544 |
| tidyAML       |         5 | NNAR        | Test  | 10.8606296 | 2251.47766 | 17.3277212 | 167.36223 | 13.9158144 | 0.0137134 |

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
    ## 1 healthyR             1 ARIMA       Test  0.671 102.  0.809 156.  0.794 9.84e-2
    ## 2 healthyR.ai          1 ARIMA       Test  0.741 121.  0.834 148.  0.913 6.38e-2
    ## 3 healthyR.da…         3 LM          Test  0.633 127.  0.647 135.  0.788 1.10e-2
    ## 4 healthyR.ts          4 EARTH       Test  0.930  93.9 0.731 151.  1.12  2.27e-2
    ## 5 healthyverse         3 LM          Test  0.504 186.  0.714  84.2 0.657 1.27e-4
    ## 6 TidyDensity          1 ARIMA       Test  0.449 112.  0.728  97.6 0.583 1.92e-1
    ## 7 tidyAML              3 LM          Test  0.495  78.1 0.789  96.2 0.661 1.55e-2

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
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [495|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [104|28]> <mdl_tm_t [1 × 5]>

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
