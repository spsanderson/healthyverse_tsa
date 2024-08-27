Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
27 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 112,192
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

The last day in the data set is 2024-08-25 22:58:19, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -443.38
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 112192        |
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
| r_version     |     78796 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78796 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78796 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9588 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-25 | 2023-01-31 |     1372 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1179898.04 | 1559864.57 | 355 | 14701 | 275362.5 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10284.79 |   17997.79 |   1 |   317 |   3075.0 |   11456 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-25 22:58:19 | 2023-01-31 16:28:21 |    67854 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     35 |       60 |

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
    ## 1 healthyR.data <tibble [1,343 × 2]> <tibble [28 × 2]> <split [1315|28]>
    ## 2 healthyR      <tibble [1,335 × 2]> <tibble [28 × 2]> <split [1307|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,281 × 2]> <tibble [28 × 2]> <split [1253|28]>
    ## 5 healthyverse  <tibble [1,252 × 2]> <tibble [28 × 2]> <split [1224|28]>
    ## 6 healthyR.ai   <tibble [1,078 × 2]> <tibble [28 × 2]> <split [1050|28]>
    ## 7 TidyDensity   <tibble [932 × 2]>   <tibble [28 × 2]> <split [904|28]> 
    ## 8 tidyAML       <tibble [548 × 2]>   <tibble [28 × 2]> <split [520|28]>

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

| package       | .model_id | .model_desc | .type |        mae |       mape |       mase |     smape |       rmse |       rsq |
|:--------------|----------:|:------------|:------|-----------:|-----------:|-----------:|----------:|-----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  |  0.6471791 |  135.42522 |  0.8114337 | 153.13658 |  0.8571546 | 0.0248347 |
| healthyR.data |         2 | LM          | Test  |  0.9249662 |  323.97774 |  1.1597232 | 175.90981 |  1.0442464 | 0.0145694 |
| healthyR.data |         3 | EARTH       | Test  |  0.5800698 |  122.24509 |  0.7272919 | 117.92568 |  0.8370229 | 0.0145694 |
| healthyR.data |         4 | NNAR        | Test  |  0.5886707 |  102.25730 |  0.7380757 | 124.15414 |  0.8285413 | 0.0573215 |
| healthyR      |         1 | ARIMA       | Test  |  0.6740201 |   84.00611 |  0.8258567 | 135.34792 |  0.9143501 | 0.0020007 |
| healthyR      |         2 | LM          | Test  |  0.7926713 |  114.01301 |  0.9712364 | 179.44217 |  1.0064728 | 0.0021964 |
| healthyR      |         3 | EARTH       | Test  |  0.6445749 |   90.87822 |  0.7897784 | 110.89380 |  0.8710031 | 0.0021964 |
| healthyR      |         4 | NNAR        | Test  |  0.7942343 |  121.58750 |  0.9731515 | 173.97278 |  1.0021667 | 0.0014031 |
| NA            |         1 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| NA            |         2 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| NA            |         3 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| NA            |         4 | NULL        | NA    |         NA |         NA |         NA |        NA |         NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  |  0.8899757 |  256.24361 |  0.8558060 | 149.49748 |  1.1707052 | 0.0178074 |
| healthyR.ts   |         2 | LM          | Test  |  0.7200236 |  224.22269 |  0.6923791 | 107.10181 |  0.9947461 | 0.0360602 |
| healthyR.ts   |         3 | EARTH       | Test  |  0.7174424 |  229.17809 |  0.6898969 | 106.03560 |  0.9923455 | 0.0360602 |
| healthyR.ts   |         4 | NNAR        | Test  |  0.8912630 |  159.80247 |  0.8570439 | 173.66123 |  1.1535073 | 0.0006752 |
| healthyverse  |         1 | ARIMA       | Test  |  0.5974518 |  595.99647 |  0.8514117 | 114.18078 |  0.7080538 | 0.2043666 |
| healthyverse  |         2 | LM          | Test  |  0.6882561 |  764.52577 |  0.9808144 | 113.81761 |  0.7953808 | 0.0039695 |
| healthyverse  |         3 | EARTH       | Test  |  0.6340500 |  533.94614 |  0.9035668 | 121.06800 |  0.7486178 | 0.0039695 |
| healthyverse  |         4 | NNAR        | Test  |  0.5962675 |  336.06147 |  0.8497240 | 131.91177 |  0.7382840 | 0.1220693 |
| healthyR.ai   |         1 | ARIMA       | Test  |  0.7580760 |  115.01330 |  0.8082044 | 163.63026 |  0.9706693 | 0.0048997 |
| healthyR.ai   |         2 | LM          | Test  |  0.7565672 |  153.39776 |  0.8065958 | 138.20829 |  0.9953060 | 0.0028777 |
| healthyR.ai   |         3 | EARTH       | Test  | 10.5723674 | 4912.84890 | 11.2714736 | 184.32756 | 11.7525576 | 0.0028777 |
| healthyR.ai   |         4 | NNAR        | Test  |  0.7420324 |  133.33794 |  0.7910999 | 148.63263 |  0.9475997 | 0.0178509 |
| TidyDensity   |         1 | ARIMA       | Test  |  0.4630430 |  212.01417 |  0.7538057 |  78.33992 |  0.5785824 | 0.1546685 |
| TidyDensity   |         2 | LM          | Test  |  0.5148553 |  247.09903 |  0.8381530 |  80.40528 |  0.6319706 | 0.0014718 |
| TidyDensity   |         3 | EARTH       | Test  |  0.8588959 |  176.21691 |  1.3982301 | 180.68922 |  1.0293487 | 0.0014718 |
| TidyDensity   |         4 | NNAR        | Test  |  0.5612669 |   85.63398 |  0.9137082 | 122.47776 |  0.7185027 | 0.1035964 |
| tidyAML       |         1 | ARIMA       | Test  |  0.6292939 |  161.54255 |  0.7889297 | 123.86933 |  0.7556065 | 0.2002049 |
| tidyAML       |         2 | LM          | Test  |  0.6576431 |  175.22384 |  0.8244704 | 114.90665 |  0.8145731 | 0.0239002 |
| tidyAML       |         3 | EARTH       | Test  |  2.6938690 | 1539.96205 |  3.3772351 | 137.77323 |  3.0688907 | 0.0239002 |
| tidyAML       |         4 | NNAR        | Test  |  0.6499253 |  233.03344 |  0.8147948 | 116.21753 |  0.8088713 | 0.0105353 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.589 102.  0.738 124.  0.829 0.0573 
    ## 2 healthyR             3 EARTH       Test  0.645  90.9 0.790 111.  0.871 0.00220
    ## 3 healthyR.ts          3 EARTH       Test  0.717 229.  0.690 106.  0.992 0.0361 
    ## 4 healthyverse         1 ARIMA       Test  0.597 596.  0.851 114.  0.708 0.204  
    ## 5 healthyR.ai          4 NNAR        Test  0.742 133.  0.791 149.  0.948 0.0179 
    ## 6 TidyDensity          1 ARIMA       Test  0.463 212.  0.754  78.3 0.579 0.155  
    ## 7 tidyAML              1 ARIMA       Test  0.629 162.  0.789 124.  0.756 0.200

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1315|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1307|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1253|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1224|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1050|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [904|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [520|28]>  <mdl_tm_t [1 × 5]>

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
