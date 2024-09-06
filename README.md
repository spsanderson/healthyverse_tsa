Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 113,052
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

The last day in the data set is 2024-09-04 22:59:42, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -683.4 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 113052        |
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
| r_version     |     79520 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     79520 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     79520 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9657 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-04 | 2023-02-03 |     1382 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |        p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|-----------:|--------:|:------|
| size          |         0 |             1 | 1177460.20 | 1558254.94 | 355 | 14701 | 274552.5 | 2373525.25 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10264.61 |   17955.99 |   1 |   328 |   3075.0 |   11386.25 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-04 22:59:42 | 2023-02-03 02:03:57 |    68298 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   30.5 |       60 |

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

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -151.47  -34.31   -9.29   25.93  800.03 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.645e+02  8.723e+01
    ## date                                                9.901e-03  4.621e-03
    ## lag(value, 1)                                       1.597e-01  2.640e-02
    ## lag(value, 7)                                       9.824e-02  2.744e-02
    ## lag(value, 14)                                      1.110e-01  2.756e-02
    ## lag(value, 21)                                      3.078e-02  2.770e-02
    ## lag(value, 28)                                      8.697e-02  2.747e-02
    ## lag(value, 35)                                      7.008e-02  2.763e-02
    ## lag(value, 42)                                      4.184e-02  2.768e-02
    ## lag(value, 49)                                      8.989e-02  2.761e-02
    ## month(date, label = TRUE).L                        -1.032e+01  5.696e+00
    ## month(date, label = TRUE).Q                         2.719e+00  5.534e+00
    ## month(date, label = TRUE).C                        -1.114e+01  5.682e+00
    ## month(date, label = TRUE)^4                        -9.197e+00  5.658e+00
    ## month(date, label = TRUE)^5                        -1.623e+01  5.588e+00
    ## month(date, label = TRUE)^6                        -4.161e+00  5.706e+00
    ## month(date, label = TRUE)^7                        -1.003e+01  5.531e+00
    ## month(date, label = TRUE)^8                        -4.563e-01  5.529e+00
    ## month(date, label = TRUE)^9                         6.911e+00  5.512e+00
    ## month(date, label = TRUE)^10                        7.239e+00  5.390e+00
    ## month(date, label = TRUE)^11                       -4.803e+00  5.275e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.153e+01  2.509e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  5.943e+00  2.597e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.886 0.059538 .  
    ## date                                                 2.142 0.032338 *  
    ## lag(value, 1)                                        6.049 1.90e-09 ***
    ## lag(value, 7)                                        3.581 0.000355 ***
    ## lag(value, 14)                                       4.029 5.91e-05 ***
    ## lag(value, 21)                                       1.111 0.266737    
    ## lag(value, 28)                                       3.167 0.001578 ** 
    ## lag(value, 35)                                       2.537 0.011298 *  
    ## lag(value, 42)                                       1.512 0.130829    
    ## lag(value, 49)                                       3.256 0.001160 ** 
    ## month(date, label = TRUE).L                         -1.812 0.070261 .  
    ## month(date, label = TRUE).Q                          0.491 0.623254    
    ## month(date, label = TRUE).C                         -1.960 0.050215 .  
    ## month(date, label = TRUE)^4                         -1.626 0.104291    
    ## month(date, label = TRUE)^5                         -2.904 0.003751 ** 
    ## month(date, label = TRUE)^6                         -0.729 0.465934    
    ## month(date, label = TRUE)^7                         -1.813 0.069994 .  
    ## month(date, label = TRUE)^8                         -0.083 0.934238    
    ## month(date, label = TRUE)^9                          1.254 0.210133    
    ## month(date, label = TRUE)^10                         1.343 0.179462    
    ## month(date, label = TRUE)^11                        -0.910 0.362760    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.598 4.68e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.289 0.022263 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.51 on 1310 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2609, Adjusted R-squared:  0.2485 
    ## F-statistic: 21.02 on 22 and 1310 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

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
    ## 1 healthyR.data <tibble [1,350 × 2]> <tibble [28 × 2]> <split [1322|28]>
    ## 2 healthyR      <tibble [1,342 × 2]> <tibble [28 × 2]> <split [1314|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,288 × 2]> <tibble [28 × 2]> <split [1260|28]>
    ## 5 healthyverse  <tibble [1,259 × 2]> <tibble [28 × 2]> <split [1231|28]>
    ## 6 healthyR.ai   <tibble [1,085 × 2]> <tibble [28 × 2]> <split [1057|28]>
    ## 7 TidyDensity   <tibble [939 × 2]>   <tibble [28 × 2]> <split [911|28]> 
    ## 8 tidyAML       <tibble [555 × 2]>   <tibble [28 × 2]> <split [527|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.6190095 |   94.20378 | 0.7324763 | 151.83627 | 0.8384324 | 0.0413676 |
| healthyR.data |         2 | LM          | Test  | 0.9000308 |  248.83528 | 1.0650100 | 171.00197 | 1.0231374 | 0.0131012 |
| healthyR.data |         3 | EARTH       | Test  | 0.5854024 |  107.81638 | 0.6927089 | 122.22993 | 0.8340511 | 0.0131012 |
| healthyR.data |         4 | NNAR        | Test  | 0.6231943 |  117.47533 | 0.7374282 | 146.06232 | 0.8500176 | 0.0120495 |
| healthyR      |         1 | ARIMA       | Test  | 0.7735929 |   83.65127 | 0.7845892 | 141.13941 | 0.9837654 | 0.0730893 |
| healthyR      |         2 | LM          | Test  | 0.9158716 |  117.15701 | 0.9288904 | 191.24670 | 1.0963106 | 0.0284018 |
| healthyR      |         3 | EARTH       | Test  | 0.7241672 |   71.08046 | 0.7344610 | 106.16428 | 0.9767549 | 0.0284018 |
| healthyR      |         4 | NNAR        | Test  | 0.8867723 |  114.51848 | 0.8993774 | 184.43397 | 1.0688303 | 0.0405091 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9441220 |  205.86017 | 0.9851930 | 164.20211 | 1.1817792 | 0.0210898 |
| healthyR.ts   |         2 | LM          | Test  | 0.6769390 |  136.23206 | 0.7063871 |  94.12056 | 0.9504857 | 0.0031010 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6728769 |  138.29883 | 0.7021483 |  92.90032 | 0.9469948 | 0.0031010 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8332667 |  162.66800 | 0.8695154 | 177.51576 | 1.0447579 | 0.0787235 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6357511 |  551.41284 | 0.8221619 | 114.25904 | 0.7398938 | 0.1582412 |
| healthyverse  |         2 | LM          | Test  | 0.6915213 |  717.34588 | 0.8942847 | 112.59131 | 0.8014188 | 0.0026772 |
| healthyverse  |         3 | EARTH       | Test  | 0.6289863 |  509.06856 | 0.8134137 | 117.52008 | 0.7448159 | 0.0026772 |
| healthyverse  |         4 | NNAR        | Test  | 0.5888764 |  282.42388 | 0.7615430 | 128.22156 | 0.7397372 | 0.0669216 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7120726 |  102.83358 | 0.7368568 | 172.65266 | 0.9172247 | 0.0265120 |
| healthyR.ai   |         2 | LM          | Test  | 0.7263699 |  144.86539 | 0.7516518 | 131.02537 | 0.9794761 | 0.0115396 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.8030458 |  219.25257 | 0.8309965 | 122.29418 | 1.0913681 | 0.0115396 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.6638176 |  117.17882 | 0.6869223 | 140.10685 | 0.8650967 | 0.1493511 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5319288 |  362.00800 | 0.7950204 |  92.54345 | 0.6305442 | 0.0861126 |
| TidyDensity   |         2 | LM          | Test  | 0.5556081 |  408.25717 | 0.8304114 |  91.29202 | 0.6644353 | 0.0000575 |
| TidyDensity   |         3 | EARTH       | Test  | 0.6889675 |  111.85033 | 1.0297302 | 156.99694 | 0.8945582 | 0.0000575 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6108441 |  112.96401 | 0.9129670 | 139.70651 | 0.7883490 | 0.0398894 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5870456 |  179.53125 | 0.8459308 | 123.32347 | 0.6720106 | 0.0703989 |
| tidyAML       |         2 | LM          | Test  | 0.5939129 |  174.47045 | 0.8558265 | 112.13288 | 0.6910608 | 0.0029144 |
| tidyAML       |         3 | EARTH       | Test  | 2.5522323 | 1106.26314 | 3.6777581 | 140.64920 | 2.8130721 | 0.0029144 |
| tidyAML       |         4 | NNAR        | Test  | 0.5807904 |  266.18165 | 0.8369170 |  96.34107 | 0.7097890 | 0.1509123 |

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
    ## 1 healthyR.da…         3 EARTH       Test  0.585 108.  0.693 122.  0.834 0.0131 
    ## 2 healthyR             3 EARTH       Test  0.724  71.1 0.734 106.  0.977 0.0284 
    ## 3 healthyR.ts          3 EARTH       Test  0.673 138.  0.702  92.9 0.947 0.00310
    ## 4 healthyverse         4 NNAR        Test  0.589 282.  0.762 128.  0.740 0.0669 
    ## 5 healthyR.ai          4 NNAR        Test  0.664 117.  0.687 140.  0.865 0.149  
    ## 6 TidyDensity          1 ARIMA       Test  0.532 362.  0.795  92.5 0.631 0.0861 
    ## 7 tidyAML              1 ARIMA       Test  0.587 180.  0.846 123.  0.672 0.0704

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1322|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1314|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1260|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1231|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1057|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [911|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [527|28]>  <mdl_tm_t [1 × 5]>

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
