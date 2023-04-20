Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
20 April, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 63,243
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

The last day in the data set is 2023-04-18 23:29:13, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -6955.52
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 63243         |
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
| r_version     |     43074 |          0.32 |   5 |   5 |     0 |       36 |          0 |
| r_arch        |     43074 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     43074 |          0.32 |   7 |  15 |     0 |       14 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       43 |          0 |
| country       |      5263 |          0.92 |   2 |   2 |     0 |      136 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-04-18 | 2022-03-14 |      877 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1324298.90 | 1674351.63 | 357 | 24837 | 322874 | 2724875 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9631.32 |   17872.25 |   1 |   121 |   2562 |   10538 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-04-18 23:29:13 | 2022-03-14 23:07:31 |    37904 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 50M 15S |       60 |

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
    ## 6 TidyDensity   <tibble [453 × 2]> <tibble [28 × 2]> <split [425|28]>
    ## 7 tidyAML       <tibble [62 × 2]>  <tibble [28 × 2]> <split [34|28]>

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [425|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [34|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.9814285 |  281.96823 | 0.8503024 | 142.88633 | 1.2619466 | 0.0145975 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 1.1127875 |  412.20116 | 0.9641108 | 165.37920 | 1.3282567 | 0.0399245 |
| healthyR      |         4 | EARTH       | Test  | 1.0050197 |  121.96077 | 0.8707416 | 191.17964 | 1.2559914 | 0.0399245 |
| healthyR      |         5 | NNAR        | Test  | 1.0136619 |  240.42287 | 0.8782292 | 165.40003 | 1.2522521 | 0.0175610 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.9675252 |  121.88697 | 0.8713620 | 171.56037 | 1.1716762 | 0.2269509 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.9331823 |  124.14680 | 0.8404324 | 185.10517 | 1.1359982 | 0.0094632 |
| healthyR.ai   |         4 | EARTH       | Test  | 1.7816130 | 1537.23441 | 1.6045368 | 129.95415 | 2.1440348 | 0.0094632 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.9644678 |  291.30911 | 0.8686085 | 160.87989 | 1.1779924 | 0.0050991 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7185088 |  114.72903 | 0.9050681 | 177.82423 | 0.8043125 | 0.0287914 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.6563705 |  137.73359 | 0.8267956 |  93.42022 | 0.9094220 | 0.0000098 |
| healthyR.data |         4 | EARTH       | Test  | 2.0686009 |  453.95336 | 2.6057087 | 164.77064 | 2.3038601 | 0.0000098 |
| healthyR.data |         5 | NNAR        | Test  | 0.5593713 |   73.72570 | 0.7046108 | 103.15968 | 0.7256578 | 0.0853106 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.1288819 |   99.10163 | 0.9157656 | 158.97849 | 1.3110537 | 0.0042724 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 1.1230820 |  100.14226 | 0.9110606 | 176.71546 | 1.2962441 | 0.0593696 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.3895289 |  163.32077 | 1.1272063 | 143.76803 | 1.6174580 | 0.0593696 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.1084110 |   99.27775 | 0.8991593 | 174.34871 | 1.2861971 | 0.0164807 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7693782 |  153.12293 | 1.0245909 | 184.67931 | 0.9335350 | 0.1314801 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7943965 | 1499.94625 | 1.0579082 | 111.16032 | 0.9878655 | 0.1518615 |
| healthyverse  |         4 | EARTH       | Test  | 0.9436809 |  456.77872 | 1.2567122 | 153.87627 | 1.2024672 | 0.1518615 |
| healthyverse  |         5 | NNAR        | Test  | 0.7504366 |  416.05342 | 0.9993662 | 149.33209 | 0.9118638 | 0.0158635 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5396929 |  466.92946 | 0.8013000 | 154.09050 | 0.6445398 | 0.0680729 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.5376627 |  282.28735 | 0.7982858 | 167.57981 | 0.6628307 | 0.0011323 |
| TidyDensity   |         4 | EARTH       | Test  | 0.5491712 |  462.57235 | 0.8153728 | 158.47790 | 0.6553094 |        NA |
| TidyDensity   |         5 | NNAR        | Test  | 0.5312068 |  889.28177 | 0.7887005 | 127.98639 | 0.6734413 | 0.0361509 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5927577 |  100.00000 | 1.2819779 | 200.00000 | 0.7087119 |        NA |
| tidyAML       |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.4024355 |  442.10951 | 0.8703612 |  84.79677 | 0.4587908 | 0.0001306 |
| tidyAML       |         4 | EARTH       | Test  | 0.5174968 |  150.22625 | 1.1192085 | 133.59767 | 0.6382343 |        NA |
| tidyAML       |         5 | NNAR        | Test  | 3.9978250 | 5401.17589 | 8.6462359 | 163.56268 | 4.5259479 | 0.0113177 |

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
    ## 1 healthyR             5 NNAR        Test  1.01  240.  0.878 165.  1.25  1.76e-2
    ## 2 healthyR.ai          3 LM          Test  0.933 124.  0.840 185.  1.14  9.46e-3
    ## 3 healthyR.da…         5 NNAR        Test  0.559  73.7 0.705 103.  0.726 8.53e-2
    ## 4 healthyR.ts          5 NNAR        Test  1.11   99.3 0.899 174.  1.29  1.65e-2
    ## 5 healthyverse         5 NNAR        Test  0.750 416.  0.999 149.  0.912 1.59e-2
    ## 6 TidyDensity          1 ARIMA       Test  0.540 467.  0.801 154.  0.645 6.81e-2
    ## 7 tidyAML              3 LM          Test  0.402 442.  0.870  84.8 0.459 1.31e-4

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [425|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [34|28]>  <mdl_tm_t [1 × 5]>

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
