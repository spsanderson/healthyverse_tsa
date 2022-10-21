Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
21 October, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 49,282
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

The last day in the data set is 2022-10-19 19:49:00, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -2607.85
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 49282         |
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
| r_version     |     33627 |          0.32 |   5 |   5 |     0 |       34 |          0 |
| r_arch        |     33627 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     33627 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       31 |          0 |
| country       |      3869 |          0.92 |   2 |   2 |     0 |      119 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-10-19 | 2021-12-09 |      696 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |        p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|-----------:|--------:|:------|
| size          |         0 |             1 | 1374960.35 | 1737589.33 | 357 | 16878 | 301755 | 2909723.00 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8865.28 |   16822.63 |   1 |   154 |   2400 |    9302.25 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-10-19 19:49:00 | 2021-12-09 21:13:05 |    28921 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     23 |       60 |

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

    ## # A tibble: 6 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 healthyR      <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 5 healthyR.ai   <tibble [424 × 2]> <tibble [28 × 2]> <split [396|28]>
    ## 6 TidyDensity   <tibble [272 × 2]> <tibble [28 × 2]> <split [244|28]>

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
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          1

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
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [424 × 2]> <tibble>     <split [396|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [272 × 2]> <tibble>     <split [244|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.5688639 | 113.18248 | 0.5693054 | 142.1323 | 0.7545873 | 0.1903342 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.6255931 | 100.17936 | 0.6260786 | 197.7284 | 0.8069042 | 0.0019111 |
| healthyR      |         4 | EARTH       | Test  | 0.6340176 | 104.00838 | 0.6345097 | 172.9950 | 0.8220905 | 0.0019111 |
| healthyR      |         5 | NNAR        | Test  | 0.7406779 | 177.70727 | 0.7412528 | 152.5483 | 0.8920846 | 0.0017764 |
| healthyR.data |         1 | ARIMA       | Test  | 0.8769731 | 223.72013 | 0.6861940 | 119.1557 | 1.0436952 | 0.0128459 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8455558 | 182.39587 | 0.6616114 | 124.9945 | 0.9870164 | 0.0001481 |
| healthyR.data |         4 | EARTH       | Test  | 0.8523934 | 202.54168 | 0.6669615 | 120.1706 | 1.0042630 | 0.0001481 |
| healthyR.data |         5 | NNAR        | Test  | 0.8944429 | 104.21596 | 0.6998634 | 180.6757 | 1.0813729 | 0.0210674 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7547886 | 131.60372 | 0.6706254 | 160.9614 | 0.9476530 | 0.0175797 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7120892 | 127.95664 | 0.6326873 | 157.9352 | 0.8362674 | 0.0110417 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.9272272 | 223.42207 | 0.8238361 | 135.2703 | 1.1890303 | 0.0110417 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.7145460 | 111.17906 | 0.6348701 | 160.9492 | 0.8835235 | 0.0174673 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7475689 | 279.54983 | 0.6451363 | 114.3929 | 0.9264598 | 0.0163402 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7484612 | 249.43906 | 0.6459063 | 120.3376 | 0.9225204 | 0.0000414 |
| healthyverse  |         4 | EARTH       | Test  | 0.7844088 | 350.46358 | 0.6769284 | 111.2329 | 0.9696706 | 0.0000414 |
| healthyverse  |         5 | NNAR        | Test  | 0.7660170 | 150.11495 | 0.6610566 | 140.2674 | 1.0367522 | 0.0149537 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6571558 |  97.75034 | 0.5619745 | 128.5005 | 0.8454613 | 0.3777926 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7803863 | 109.89091 | 0.6673566 | 153.1126 | 0.9947698 | 0.0727335 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.9875809 | 233.35679 | 0.8445415 | 133.5632 | 1.2415034 | 0.0727335 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7555369 | 121.74452 | 0.6461063 | 153.7503 | 0.9732674 | 0.0179285 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6420124 | 126.88121 | 0.6084638 | 151.0024 | 0.7807609 | 0.2573887 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6924840 | 104.11148 | 0.6562980 | 161.9436 | 0.8673512 | 0.0321624 |
| TidyDensity   |         4 | EARTH       | Test  | 0.6880030 | 110.93899 | 0.6520511 | 155.3753 | 0.8624568 |        NA |
| TidyDensity   |         5 | NNAR        | Test  | 0.7730195 | 194.29865 | 0.7326250 | 145.6314 | 0.9150256 | 0.0119885 |

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
    ##   package       .model_id .model_d…¹ .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>             <int> <chr>      <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR              1 ARIMA      Test  0.569 113.  0.569  142. 0.755 1.90e-1
    ## 2 healthyR.data         3 LM         Test  0.846 182.  0.662  125. 0.987 1.48e-4
    ## 3 healthyR.ts           3 LM         Test  0.712 128.  0.633  158. 0.836 1.10e-2
    ## 4 healthyverse          3 LM         Test  0.748 249.  0.646  120. 0.923 4.14e-5
    ## 5 healthyR.ai           1 ARIMA      Test  0.657  97.8 0.562  129. 0.845 3.78e-1
    ## 6 TidyDensity           1 ARIMA      Test  0.642 127.  0.608  151. 0.781 2.57e-1
    ## # … with abbreviated variable name ¹​.model_desc

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
    ##   # A tibble: 6 × 5
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [424 × 2]> <tibble>     <split [396|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [272 × 2]> <tibble>     <split [244|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

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
