Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
20 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 124,346
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

The last day in the data set is 2024-12-18 23:59:17, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3204.39
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 124346        |
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
| r_version     |     88128 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     88128 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     88128 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10663 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-18 | 2023-04-07 | 1487 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1150187.32 | 1537206.18 | 355 | 14701 | 260378 | 2368012.00 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10356.58 | 17999.91 | 1 | 334 | 3100 | 11862.75 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-18 23:59:17 | 2023-04-07 05:19:12 | 75356 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     51 |       60 |

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
    ## -156.07  -34.66   -9.68   26.77  803.36 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.761e+02  8.054e+01
    ## date                                                1.060e-02  4.268e-03
    ## lag(value, 1)                                       1.350e-01  2.559e-02
    ## lag(value, 7)                                       9.220e-02  2.651e-02
    ## lag(value, 14)                                      1.051e-01  2.656e-02
    ## lag(value, 21)                                      4.491e-02  2.668e-02
    ## lag(value, 28)                                      7.160e-02  2.649e-02
    ## lag(value, 35)                                      6.755e-02  2.669e-02
    ## lag(value, 42)                                      4.402e-02  2.680e-02
    ## lag(value, 49)                                      1.062e-01  2.664e-02
    ## month(date, label = TRUE).L                        -1.114e+01  5.508e+00
    ## month(date, label = TRUE).Q                         1.802e+00  5.361e+00
    ## month(date, label = TRUE).C                        -1.140e+01  5.428e+00
    ## month(date, label = TRUE)^4                        -7.831e+00  5.390e+00
    ## month(date, label = TRUE)^5                        -1.280e+01  5.335e+00
    ## month(date, label = TRUE)^6                        -1.111e+00  5.391e+00
    ## month(date, label = TRUE)^7                        -8.989e+00  5.278e+00
    ## month(date, label = TRUE)^8                        -2.351e+00  5.271e+00
    ## month(date, label = TRUE)^9                         4.231e+00  5.261e+00
    ## month(date, label = TRUE)^10                        5.150e+00  5.260e+00
    ## month(date, label = TRUE)^11                       -6.187e+00  5.274e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.164e+01  2.442e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.769e+00  2.555e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.187 0.028925 *  
    ## date                                                 2.483 0.013125 *  
    ## lag(value, 1)                                        5.276 1.53e-07 ***
    ## lag(value, 7)                                        3.478 0.000521 ***
    ## lag(value, 14)                                       3.958 7.92e-05 ***
    ## lag(value, 21)                                       1.684 0.092479 .  
    ## lag(value, 28)                                       2.703 0.006956 ** 
    ## lag(value, 35)                                       2.531 0.011491 *  
    ## lag(value, 42)                                       1.642 0.100739    
    ## lag(value, 49)                                       3.987 7.04e-05 ***
    ## month(date, label = TRUE).L                         -2.022 0.043334 *  
    ## month(date, label = TRUE).Q                          0.336 0.736841    
    ## month(date, label = TRUE).C                         -2.101 0.035800 *  
    ## month(date, label = TRUE)^4                         -1.453 0.146512    
    ## month(date, label = TRUE)^5                         -2.399 0.016557 *  
    ## month(date, label = TRUE)^6                         -0.206 0.836722    
    ## month(date, label = TRUE)^7                         -1.703 0.088766 .  
    ## month(date, label = TRUE)^8                         -0.446 0.655604    
    ## month(date, label = TRUE)^9                          0.804 0.421324    
    ## month(date, label = TRUE)^10                         0.979 0.327691    
    ## month(date, label = TRUE)^11                        -1.173 0.240952    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.766 2.08e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.650 0.008144 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.86 on 1415 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.259,  Adjusted R-squared:  0.2475 
    ## F-statistic: 22.48 on 22 and 1415 DF,  p-value: < 2.2e-16

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

    ## # A tibble: 9 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,453 × 2]> <tibble [28 × 2]> <split [1425|28]>
    ## 2 healthyR      <tibble [1,446 × 2]> <tibble [28 × 2]> <split [1418|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,392 × 2]> <tibble [28 × 2]> <split [1364|28]>
    ## 5 healthyverse  <tibble [1,363 × 2]> <tibble [28 × 2]> <split [1335|28]>
    ## 6 healthyR.ai   <tibble [1,189 × 2]> <tibble [28 × 2]> <split [1161|28]>
    ## 7 TidyDensity   <tibble [1,043 × 2]> <tibble [28 × 2]> <split [1015|28]>
    ## 8 tidyAML       <tibble [659 × 2]>   <tibble [28 × 2]> <split [631|28]> 
    ## 9 RandomWalker  <tibble [93 × 2]>    <tibble [28 × 2]> <split [65|28]>

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
  filter(!is.na(package)) %>%
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.7481926 | 207.64624 | 0.6061820 | 147.62558 | 0.9224688 | 0.1019541 |
| healthyR.data | 2 | LM | Test | 0.7918726 | 291.99706 | 0.6415713 | 142.55651 | 0.9301736 | 0.0053187 |
| healthyR.data | 3 | EARTH | Test | 0.7976216 | 343.49468 | 0.6462292 | 138.60839 | 0.9175083 | 0.0053187 |
| healthyR.data | 4 | NNAR | Test | 0.8583383 | 140.43528 | 0.6954215 | 166.58826 | 1.1166047 | 0.0015234 |
| healthyR | 1 | ARIMA | Test | 0.6089340 | 145.21107 | 0.6863917 | 148.33993 | 0.7412867 | 0.1078974 |
| healthyR | 2 | LM | Test | 0.6309953 | 109.48230 | 0.7112593 | 190.75662 | 0.7422779 | 0.0003193 |
| healthyR | 3 | EARTH | Test | 0.6666601 | 194.97924 | 0.7514607 | 142.82896 | 0.8047909 | 0.0003193 |
| healthyR | 4 | NNAR | Test | 0.5815509 | 133.23593 | 0.6555254 | 154.80899 | 0.6937374 | 0.1364049 |
| healthyR.ts | 1 | ARIMA | Test | 0.8494067 | 108.56343 | 0.8360797 | 112.61720 | 1.0337949 | 0.0159736 |
| healthyR.ts | 2 | LM | Test | 0.8277913 | 97.86843 | 0.8148033 | 115.53197 | 1.0004728 | 0.0159736 |
| healthyR.ts | 3 | EARTH | Test | 0.8288074 | 98.87077 | 0.8158035 | 114.84301 | 1.0039428 | 0.0159736 |
| healthyR.ts | 4 | NNAR | Test | 0.8541213 | 97.48670 | 0.8407202 | 180.15464 | 1.0162979 | 0.0000929 |
| healthyverse | 1 | ARIMA | Test | 0.4837788 | 200.71207 | 0.8727021 | 88.73917 | 0.6035336 | 0.3300899 |
| healthyverse | 2 | LM | Test | 0.5147887 | 324.74019 | 0.9286416 | 82.18156 | 0.6248618 | 0.0000009 |
| healthyverse | 3 | EARTH | Test | 0.5824224 | 191.34608 | 1.0506480 | 107.41483 | 0.7061732 | 0.0000009 |
| healthyverse | 4 | NNAR | Test | 0.5813760 | 188.54794 | 1.0487603 | 106.74382 | 0.7074592 | 0.0000005 |
| healthyR.ai | 1 | ARIMA | Test | 0.6766842 | 93.68848 | 0.9526533 | 156.96239 | 0.7795278 | 0.2114937 |
| healthyR.ai | 2 | LM | Test | 0.6997853 | 120.08259 | 0.9851755 | 162.14807 | 0.7747868 | 0.0003229 |
| healthyR.ai | 3 | EARTH | Test | 0.7804829 | 142.35956 | 1.0987838 | 150.91412 | 0.9128951 | 0.0003229 |
| healthyR.ai | 4 | NNAR | Test | 0.6486707 | 112.86499 | 0.9132152 | 149.78744 | 0.7270289 | 0.1721191 |
| TidyDensity | 1 | ARIMA | Test | 0.6059135 | 201.11389 | 0.7401352 | 109.09264 | 0.7517507 | 0.1854336 |
| TidyDensity | 2 | LM | Test | 0.7084631 | 272.24295 | 0.8654015 | 108.53752 | 0.8498194 | 0.0636905 |
| TidyDensity | 3 | EARTH | Test | 0.6628655 | 206.97116 | 0.8097031 | 112.91984 | 0.8059690 | 0.0636905 |
| TidyDensity | 4 | NNAR | Test | 0.6709074 | 120.71500 | 0.8195264 | 149.52105 | 0.8153243 | 0.0552811 |
| tidyAML | 1 | ARIMA | Test | 0.6940946 | 89.03570 | 0.9537791 | 93.84902 | 0.8575510 | 0.1378735 |
| tidyAML | 2 | LM | Test | 0.7001138 | 89.88925 | 0.9620502 | 88.19056 | 0.8736568 | 0.0038194 |
| tidyAML | 3 | EARTH | Test | 0.6678795 | 116.36165 | 0.9177560 | 80.54350 | 0.7960838 | 0.0038194 |
| tidyAML | 4 | NNAR | Test | 0.6683304 | 109.49502 | 0.9183756 | 83.46534 | 0.7924357 | 0.0681595 |
| RandomWalker | 1 | ARIMA | Test | 1.3064852 | 96.33610 | 0.5845275 | 185.72103 | 1.4196551 | 0.1780135 |
| RandomWalker | 2 | LM | Test | 1.4129692 | 106.01446 | 0.6321690 | 181.06027 | 1.5119950 | 0.0040821 |
| RandomWalker | 3 | EARTH | Test | 1.6221560 | 142.92106 | 0.7257601 | 155.02116 | 1.7995235 | 0.0040821 |
| RandomWalker | 4 | NNAR | Test | 4.3833251 | 403.11760 | 1.9611200 | 152.48027 | 5.1422273 | 0.0245620 |

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
    ## 

    ## # A tibble: 8 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         3 EARTH       Test  0.798 343.  0.646 139.  0.918 0.00532
    ## 2 healthyR             4 NNAR        Test  0.582 133.  0.656 155.  0.694 0.136  
    ## 3 healthyR.ts          2 LM          Test  0.828  97.9 0.815 116.  1.00  0.0160 
    ## 4 healthyverse         1 ARIMA       Test  0.484 201.  0.873  88.7 0.604 0.330  
    ## 5 healthyR.ai          4 NNAR        Test  0.649 113.  0.913 150.  0.727 0.172  
    ## 6 TidyDensity          1 ARIMA       Test  0.606 201.  0.740 109.  0.752 0.185  
    ## 7 tidyAML              4 NNAR        Test  0.668 109.  0.918  83.5 0.792 0.0682 
    ## 8 RandomWalker         1 ARIMA       Test  1.31   96.3 0.585 186.  1.42  0.178

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
    ## 

    ## # A tibble: 8 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1425|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1418|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1364|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1335|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1161|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1015|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [631|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [65|28]>   <mdl_tm_t [1 × 5]>

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
