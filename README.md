Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
24 August, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 45,037
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

The last day in the data set is 2022-08-22 22:52:01, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -1218.9
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 45037         |
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
| r_version     |     30538 |          0.32 |   5 |   5 |     0 |       33 |          0 |
| r_arch        |     30538 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     30538 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       28 |          0 |
| country       |      3571 |          0.92 |   2 |   2 |     0 |      117 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-08-22 | 2021-11-14 |      638 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1393097.20 | 1764585.42 | 357 | 16734 | 289858 | 2970069 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8806.25 |   16787.31 |   1 |   168 |   2449 |    9062 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-08-22 22:52:01 | 2021-11-14 05:45:26 |    26364 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 30M 41S |       60 |

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 4 healthyverse  <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 5 healthyR.ai   <tibble [366 × 2]> <tibble [28 × 2]> <split [338|28]>
    ## 6 TidyDensity   <tibble [214 × 2]> <tibble [28 × 2]> <split [186|28]>

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [366 × 2]> <tibble>     <split [338|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [214 × 2]> <tibble>     <split [186|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.8033316 | 182.59338 | 0.7867653 | 135.8832 | 1.0846166 | 0.0214159 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8107930 | 102.10839 | 0.7940729 | 194.2716 | 1.0479938 | 0.0434382 |
| healthyR      |         4 | EARTH       | Test  | 1.2927025 | 912.98542 | 1.2660444 | 130.5750 | 1.5882874 | 0.0434382 |
| healthyR      |         5 | NNAR        | Test  | 0.8554287 | 213.66648 | 0.8377881 | 140.6021 | 1.1446580 | 0.0080884 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9618029 | 121.97241 | 0.7407534 | 136.4221 | 1.0938936 | 0.0087125 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.9364629 | 112.45322 | 0.7212373 | 133.8776 | 1.0883834 | 0.0646425 |
| healthyR.data |         4 | EARTH       | Test  | 0.9313591 | 115.31135 | 0.7173065 | 130.7265 | 1.0777711 | 0.0646425 |
| healthyR.data |         5 | NNAR        | Test  | 1.1244022 | 102.12997 | 0.8659828 | 163.3302 | 1.3791942 | 0.0509427 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7871948 | 217.94044 | 0.6672472 | 146.0164 | 0.9241125 | 0.1465068 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8352048 | 161.47637 | 0.7079417 | 163.7454 | 1.0002936 | 0.0598440 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.8312635 | 276.43714 | 0.7046010 | 135.2639 | 0.9984795 | 0.0598440 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.7983312 | 185.72489 | 0.6766867 | 144.3651 | 0.9312397 | 0.1980234 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8457044 | 423.29030 | 0.7767813 | 140.7671 | 1.0464672 | 0.0475207 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7852814 | 545.02476 | 0.7212827 | 119.7314 | 0.9601654 | 0.0879963 |
| healthyverse  |         4 | EARTH       | Test  | 0.7725070 | 741.43961 | 0.7095494 | 110.3334 | 0.9136380 | 0.0879963 |
| healthyverse  |         5 | NNAR        | Test  | 0.8510120 | 793.31028 | 0.7816564 | 133.6258 | 1.0543428 | 0.0072271 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8038322 | 449.66844 | 0.7229646 | 180.9687 | 1.0062886 | 0.0468794 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7793959 | 110.15510 | 0.7009867 | 196.2822 | 1.0056376 | 0.0965020 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8143432 | 362.50492 | 0.7324182 | 143.8750 | 1.0855884 | 0.0965020 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7198052 | 546.17138 | 0.6473909 | 139.0106 | 0.9245905 | 0.1274882 |
| TidyDensity   |         1 | ARIMA       | Test  | 1.0214500 |  95.39469 | 0.7486471 | 183.0820 | 1.2109862 | 0.0117723 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 1.0326188 |  99.65431 | 0.7568331 | 158.6283 | 1.2195007 | 0.0796033 |
| TidyDensity   |         4 | EARTH       | Test  | 1.0229241 | 102.06026 | 0.7497276 | 126.6385 | 1.2773902 | 0.0796033 |
| TidyDensity   |         5 | NNAR        | Test  | 0.9569192 | 103.86370 | 0.7013508 | 139.0383 | 1.1472501 | 0.1318520 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR              3 LM          Test  0.811  102. 0.794  194. 1.05  0.0434
    ## 2 healthyR.data         4 EARTH       Test  0.931  115. 0.717  131. 1.08  0.0646
    ## 3 healthyR.ts           1 ARIMA       Test  0.787  218. 0.667  146. 0.924 0.147 
    ## 4 healthyverse          4 EARTH       Test  0.773  741. 0.710  110. 0.914 0.0880
    ## 5 healthyR.ai           5 NNAR        Test  0.720  546. 0.647  139. 0.925 0.127 
    ## 6 TidyDensity           5 NNAR        Test  0.957  104. 0.701  139. 1.15  0.132

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [366 × 2]> <tibble>     <split [338|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [214 × 2]> <tibble>     <split [186|28]> <mdl_time_tbl> 
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
