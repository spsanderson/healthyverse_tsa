Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
13 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 113,972
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

The last day in the data set is 2024-09-11 23:47:41, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -852.2 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 113972        |
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
| r_version     |     80213 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     80213 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     80213 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9710 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-11 | 2023-02-08 |     1389 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1175130.84 | 1556629.69 | 355 | 14701 | 271313.5 | 2373317 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10263.41 |   17954.08 |   1 |   317 |   3075.0 |   11412 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-11 23:47:41 | 2023-02-08 10:37:09 |    68954 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     30 |       60 |

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
    ## -153.31  -34.34   -9.75   25.97  798.13 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.795e+02  8.725e+01
    ## date                                                1.069e-02  4.623e-03
    ## lag(value, 1)                                       1.558e-01  2.653e-02
    ## lag(value, 7)                                       1.041e-01  2.755e-02
    ## lag(value, 14)                                      1.123e-01  2.766e-02
    ## lag(value, 21)                                      2.738e-02  2.782e-02
    ## lag(value, 28)                                      8.246e-02  2.762e-02
    ## lag(value, 35)                                      6.916e-02  2.776e-02
    ## lag(value, 42)                                      3.946e-02  2.780e-02
    ## lag(value, 49)                                      9.838e-02  2.761e-02
    ## month(date, label = TRUE).L                        -9.915e+00  5.726e+00
    ## month(date, label = TRUE).Q                         2.447e+00  5.566e+00
    ## month(date, label = TRUE).C                        -1.196e+01  5.692e+00
    ## month(date, label = TRUE)^4                        -9.691e+00  5.688e+00
    ## month(date, label = TRUE)^5                        -1.562e+01  5.613e+00
    ## month(date, label = TRUE)^6                        -3.318e+00  5.710e+00
    ## month(date, label = TRUE)^7                        -9.563e+00  5.562e+00
    ## month(date, label = TRUE)^8                        -1.203e+00  5.550e+00
    ## month(date, label = TRUE)^9                         5.829e+00  5.498e+00
    ## month(date, label = TRUE)^10                        6.207e+00  5.383e+00
    ## month(date, label = TRUE)^11                       -5.267e+00  5.301e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.166e+01  2.520e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.146e+00  2.609e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.058 0.039800 *  
    ## date                                                 2.312 0.020923 *  
    ## lag(value, 1)                                        5.875 5.36e-09 ***
    ## lag(value, 7)                                        3.780 0.000164 ***
    ## lag(value, 14)                                       4.062 5.15e-05 ***
    ## lag(value, 21)                                       0.984 0.325167    
    ## lag(value, 28)                                       2.985 0.002885 ** 
    ## lag(value, 35)                                       2.492 0.012829 *  
    ## lag(value, 42)                                       1.419 0.156030    
    ## lag(value, 49)                                       3.562 0.000380 ***
    ## month(date, label = TRUE).L                         -1.732 0.083555 .  
    ## month(date, label = TRUE).Q                          0.440 0.660362    
    ## month(date, label = TRUE).C                         -2.101 0.035860 *  
    ## month(date, label = TRUE)^4                         -1.704 0.088654 .  
    ## month(date, label = TRUE)^5                         -2.784 0.005453 ** 
    ## month(date, label = TRUE)^6                         -0.581 0.561297    
    ## month(date, label = TRUE)^7                         -1.719 0.085774 .  
    ## month(date, label = TRUE)^8                         -0.217 0.828493    
    ## month(date, label = TRUE)^9                          1.060 0.289215    
    ## month(date, label = TRUE)^10                         1.153 0.249052    
    ## month(date, label = TRUE)^11                        -0.994 0.320559    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.625 4.11e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.355 0.018657 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.88 on 1317 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2612, Adjusted R-squared:  0.2489 
    ## F-statistic: 21.17 on 22 and 1317 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,357 × 2]> <tibble [28 × 2]> <split [1329|28]>
    ## 2 healthyR      <tibble [1,349 × 2]> <tibble [28 × 2]> <split [1321|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,295 × 2]> <tibble [28 × 2]> <split [1267|28]>
    ## 5 healthyverse  <tibble [1,266 × 2]> <tibble [28 × 2]> <split [1238|28]>
    ## 6 healthyR.ai   <tibble [1,092 × 2]> <tibble [28 × 2]> <split [1064|28]>
    ## 7 TidyDensity   <tibble [946 × 2]>   <tibble [28 × 2]> <split [918|28]> 
    ## 8 tidyAML       <tibble [562 × 2]>   <tibble [28 × 2]> <split [534|28]>

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

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.6288425 |  97.45270 | 0.6742435 | 154.85495 | 0.8873591 | 0.0002370 |
| healthyR.data |         2 | LM          | Test  | 0.9213446 | 470.28195 | 0.9878636 | 176.84945 | 1.0732253 | 0.0115303 |
| healthyR.data |         3 | EARTH       | Test  | 0.6060764 | 190.80856 | 0.6498338 | 131.27324 | 0.8684557 | 0.0115303 |
| healthyR.data |         4 | NNAR        | Test  | 0.6232235 | 170.21721 | 0.6682188 | 149.56827 | 0.8722967 | 0.0029742 |
| healthyR      |         1 | ARIMA       | Test  | 0.7348675 |  77.37804 | 0.6430059 | 113.46529 | 1.0129768 | 0.0493328 |
| healthyR      |         2 | LM          | Test  | 0.9096294 | 117.51055 | 0.7959219 | 192.04539 | 1.1413478 | 0.0039968 |
| healthyR      |         3 | EARTH       | Test  | 0.7114910 |  76.42251 | 0.6225516 |  95.93139 | 1.0161977 | 0.0039968 |
| healthyR      |         4 | NNAR        | Test  | 0.8660911 | 110.01610 | 0.7578261 | 173.32486 | 1.0864683 | 0.0837318 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7631998 | 121.55052 | 0.7134116 |  84.99875 | 0.9764473 | 0.0072246 |
| healthyR.ts   |         2 | LM          | Test  | 0.7091338 |  81.36559 | 0.6628727 |  94.70300 | 1.0069813 | 0.0073474 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7043327 |  81.45621 | 0.6583849 |  93.31597 | 1.0030502 | 0.0073474 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8503548 |  93.87215 | 0.7948810 | 172.16854 | 1.1014781 | 0.1429420 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6994206 | 436.18822 | 0.9192516 | 119.84605 | 0.8365336 | 0.0236631 |
| healthyverse  |         2 | LM          | Test  | 0.7335886 | 526.82651 | 0.9641588 | 115.97471 | 0.8862550 | 0.0353494 |
| healthyverse  |         3 | EARTH       | Test  | 0.6365117 | 372.60079 | 0.8365702 | 116.83438 | 0.7875508 | 0.0353494 |
| healthyverse  |         4 | NNAR        | Test  | 0.6120396 | 254.96119 | 0.8044064 | 129.61593 | 0.7609515 | 0.0034711 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8185381 | 114.99292 | 0.6357072 | 166.44113 | 1.1072684 | 0.0111222 |
| healthyR.ai   |         2 | LM          | Test  | 0.8531396 | 206.73369 | 0.6625800 | 146.71128 | 1.1622849 | 0.0000087 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.3565408 | 758.28507 | 1.0535402 | 140.80319 | 1.6930399 | 0.0000087 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8290075 | 147.00596 | 0.6438381 | 157.09795 | 1.1236514 | 0.0011333 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5464278 | 364.87678 | 0.8167980 |  89.25027 | 0.7097265 | 0.0164304 |
| TidyDensity   |         2 | LM          | Test  | 0.5648340 | 422.86468 | 0.8443115 |  86.82643 | 0.7155075 | 0.0013538 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7855379 | 137.29080 | 1.1742188 | 172.27100 | 1.0398178 | 0.0013538 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5997314 | 119.02354 | 0.8964760 | 124.75874 | 0.8468363 | 0.1063458 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5831479 | 202.94406 | 0.8590880 | 123.74976 | 0.7081970 | 0.0371386 |
| tidyAML       |         2 | LM          | Test  | 0.5998031 | 242.65754 | 0.8836244 | 114.26265 | 0.7143224 | 0.0788959 |
| tidyAML       |         3 | EARTH       | Test  | 1.7913508 | 905.58752 | 2.6390013 | 172.86281 | 1.9862146 | 0.0788959 |
| tidyAML       |         4 | NNAR        | Test  | 0.5977263 | 350.49002 | 0.8805648 | 100.10956 | 0.7706884 | 0.0021304 |

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
    ## 1 healthyR.da…         3 EARTH       Test  0.606 191.  0.650 131.  0.868 0.0115 
    ## 2 healthyR             1 ARIMA       Test  0.735  77.4 0.643 113.  1.01  0.0493 
    ## 3 healthyR.ts          1 ARIMA       Test  0.763 122.  0.713  85.0 0.976 0.00722
    ## 4 healthyverse         4 NNAR        Test  0.612 255.  0.804 130.  0.761 0.00347
    ## 5 healthyR.ai          1 ARIMA       Test  0.819 115.  0.636 166.  1.11  0.0111 
    ## 6 TidyDensity          1 ARIMA       Test  0.546 365.  0.817  89.3 0.710 0.0164 
    ## 7 tidyAML              1 ARIMA       Test  0.583 203.  0.859 124.  0.708 0.0371

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1329|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1321|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1267|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1238|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1064|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [918|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [534|28]>  <mdl_tm_t [1 × 5]>

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
