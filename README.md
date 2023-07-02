Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
02 July, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 70,916
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

The last day in the data set is 2023-06-30 23:47:25, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -8707.82
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 70916         |
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
| r_version     |     48478 |          0.32 |   5 |   5 |     0 |       38 |          0 |
| r_arch        |     48478 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     48478 |          0.32 |   7 |  15 |     0 |       15 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       45 |          0 |
| country       |      5963 |          0.92 |   2 |   2 |     0 |      139 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-06-30 | 2022-04-29 |      950 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1298076.47 | 1651523.35 | 357 | 22118.75 | 323131.5 | 2674498 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9954.53 |   18093.13 |   1 |   137.00 |   2745.0 |   10899 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-06-30 23:47:25 | 2022-04-29 14:06:48 |    42604 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      5 |       60 |

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
    ## 6 TidyDensity   <tibble [526 × 2]> <tibble [28 × 2]> <split [498|28]>
    ## 7 tidyAML       <tibble [135 × 2]> <tibble [28 × 2]> <split [107|28]>

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [498|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [107|28]> <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.6904649 | 103.33244 | 0.7840442 | 149.34384 | 0.8365640 | 0.0875305 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.6866373 |  98.71822 | 0.7796978 | 179.21420 | 0.8268101 | 0.0720373 |
| healthyR      |         4 | EARTH       | Test  | 0.7223985 | 130.03782 | 0.8203058 | 143.69935 | 0.8883287 | 0.0720373 |
| healthyR      |         5 | NNAR        | Test  | 0.6561659 |  99.29861 | 0.7450966 | 169.22526 | 0.7777214 | 0.1432588 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7639748 | 135.94066 | 0.8184582 | 173.60868 | 0.9154329 | 0.1586085 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8670316 | 167.11930 | 0.9288645 | 185.14133 | 0.9960811 | 0.0633106 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.7198842 |  93.90332 | 0.7712231 | 151.80186 | 0.8994891 | 0.0633106 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7519005 | 143.54686 | 0.8055227 | 161.00616 | 0.9059544 | 0.0223376 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6968511 | 128.42773 | 0.7519827 | 163.20487 | 0.8543615 | 0.0027797 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.6568790 | 119.46397 | 0.7088482 | 136.97277 | 0.8090890 | 0.1680974 |
| healthyR.data |         4 | EARTH       | Test  | 0.7201458 | 177.20580 | 0.7771204 | 143.76004 | 0.8739633 | 0.1680974 |
| healthyR.data |         5 | NNAR        | Test  | 0.6655929 | 100.76050 | 0.7182515 | 153.37650 | 0.8146444 | 0.0072518 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9407651 |  86.04843 | 0.6884066 | 126.48025 | 1.2990125 | 0.0270921 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.9590765 |  90.17800 | 0.7018060 | 113.90436 | 1.2943290 | 0.2405082 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.2410239 | 126.11938 | 0.9081215 | 159.56971 | 1.5785685 | 0.2405082 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.0000333 |  97.70679 | 0.7317762 | 177.45905 | 1.2789543 | 0.0559228 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5699422 | 118.75192 | 0.8582806 | 105.83034 | 0.7288332 | 0.0404154 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.4895942 | 155.40794 | 0.7372838 |  79.61570 | 0.6481266 | 0.1679470 |
| healthyverse  |         4 | EARTH       | Test  | 0.5888174 | 273.97910 | 0.8867048 |  76.30896 | 0.7473953 | 0.1679470 |
| healthyverse  |         5 | NNAR        | Test  | 0.6121962 | 121.82119 | 0.9219112 | 118.24298 | 0.7547184 | 0.0350212 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.4324385 | 128.13984 | 0.6665016 |  95.46659 | 0.5791450 | 0.0082021 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.4211560 | 133.72452 | 0.6491122 |  91.01625 | 0.5583710 | 0.0339091 |
| TidyDensity   |         4 | EARTH       | Test  | 0.9727393 | 411.81216 | 1.4992474 | 117.84695 | 1.0677687 | 0.0339091 |
| TidyDensity   |         5 | NNAR        | Test  | 0.4846319 | 104.42432 | 0.7469453 | 131.11226 | 0.6608616 | 0.0189336 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6278400 |  97.58693 | 1.0567760 | 150.90209 | 0.7679726 | 0.0091771 |
| tidyAML       |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.5256498 |  73.53433 | 0.8847701 |  99.57269 | 0.6875111 | 0.0937897 |
| tidyAML       |         4 | EARTH       | Test  | 0.3825584 | 129.35931 | 0.6439196 |  57.94321 | 0.5075193 | 0.0937897 |
| tidyAML       |         5 | NNAR        | Test  | 3.9257868 | 974.06945 | 6.6078576 | 178.94350 | 5.8702665 | 0.0103651 |

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
    ## 1 healthyR              5 NNAR        Test  0.656  99.3 0.745 169.  0.778 0.143 
    ## 2 healthyR.ai           4 EARTH       Test  0.720  93.9 0.771 152.  0.899 0.0633
    ## 3 healthyR.data         3 LM          Test  0.657 119.  0.709 137.  0.809 0.168 
    ## 4 healthyR.ts           5 NNAR        Test  1.00   97.7 0.732 177.  1.28  0.0559
    ## 5 healthyverse          3 LM          Test  0.490 155.  0.737  79.6 0.648 0.168 
    ## 6 TidyDensity           3 LM          Test  0.421 134.  0.649  91.0 0.558 0.0339
    ## 7 tidyAML               4 EARTH       Test  0.383 129.  0.644  57.9 0.508 0.0938

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
    ## 6 TidyDensity   <tibble>     <tibble>     <split [498|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [107|28]> <mdl_tm_t [1 × 5]>

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
