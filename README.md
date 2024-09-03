Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
03 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 112,694
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

The last day in the data set is 2024-09-01 23:54:46, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -612.32
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 112694        |
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
| r_version     |     79230 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     79230 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     79230 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9630 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-01 | 2023-02-02 |     1379 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1178497.27 | 1558802.20 | 355 | 14701 | 274859 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10275.06 |   17969.95 |   1 |   319 |   3082 |   11424 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-01 23:54:46 | 2023-02-02 00:58:28 |    68132 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     12 |       60 |

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
    ## -150.73  -34.22   -9.26   25.83  800.38 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.622e+02  8.755e+01
    ## date                                                9.785e-03  4.638e-03
    ## lag(value, 1)                                       1.603e-01  2.642e-02
    ## lag(value, 7)                                       9.711e-02  2.747e-02
    ## lag(value, 14)                                      1.114e-01  2.757e-02
    ## lag(value, 21)                                      3.198e-02  2.773e-02
    ## lag(value, 28)                                      8.704e-02  2.749e-02
    ## lag(value, 35)                                      7.077e-02  2.766e-02
    ## lag(value, 42)                                      4.028e-02  2.776e-02
    ## lag(value, 49)                                      8.804e-02  2.767e-02
    ## month(date, label = TRUE).L                        -1.039e+01  5.701e+00
    ## month(date, label = TRUE).Q                         2.780e+00  5.538e+00
    ## month(date, label = TRUE).C                        -1.091e+01  5.698e+00
    ## month(date, label = TRUE)^4                        -9.050e+00  5.666e+00
    ## month(date, label = TRUE)^5                        -1.636e+01  5.596e+00
    ## month(date, label = TRUE)^6                        -4.306e+00  5.720e+00
    ## month(date, label = TRUE)^7                        -1.014e+01  5.537e+00
    ## month(date, label = TRUE)^8                        -2.655e-01  5.541e+00
    ## month(date, label = TRUE)^9                         7.139e+00  5.538e+00
    ## month(date, label = TRUE)^10                        7.443e+00  5.410e+00
    ## month(date, label = TRUE)^11                       -4.668e+00  5.284e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.153e+01  2.512e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  5.902e+00  2.599e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.852 0.064189 .  
    ## date                                                 2.110 0.035065 *  
    ## lag(value, 1)                                        6.065 1.72e-09 ***
    ## lag(value, 7)                                        3.535 0.000422 ***
    ## lag(value, 14)                                       4.041 5.63e-05 ***
    ## lag(value, 21)                                       1.153 0.249100    
    ## lag(value, 28)                                       3.166 0.001582 ** 
    ## lag(value, 35)                                       2.559 0.010623 *  
    ## lag(value, 42)                                       1.451 0.147069    
    ## lag(value, 49)                                       3.182 0.001498 ** 
    ## month(date, label = TRUE).L                         -1.822 0.068726 .  
    ## month(date, label = TRUE).Q                          0.502 0.615757    
    ## month(date, label = TRUE).C                         -1.915 0.055769 .  
    ## month(date, label = TRUE)^4                         -1.597 0.110440    
    ## month(date, label = TRUE)^5                         -2.923 0.003531 ** 
    ## month(date, label = TRUE)^6                         -0.753 0.451732    
    ## month(date, label = TRUE)^7                         -1.832 0.067201 .  
    ## month(date, label = TRUE)^8                         -0.048 0.961783    
    ## month(date, label = TRUE)^9                          1.289 0.197638    
    ## month(date, label = TRUE)^10                         1.376 0.169152    
    ## month(date, label = TRUE)^11                        -0.883 0.377199    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.591 4.84e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.271 0.023301 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.54 on 1307 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2598, Adjusted R-squared:  0.2474 
    ## F-statistic: 20.85 on 22 and 1307 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,347 × 2]> <tibble [28 × 2]> <split [1319|28]>
    ## 2 healthyR      <tibble [1,339 × 2]> <tibble [28 × 2]> <split [1311|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,285 × 2]> <tibble [28 × 2]> <split [1257|28]>
    ## 5 healthyverse  <tibble [1,256 × 2]> <tibble [28 × 2]> <split [1228|28]>
    ## 6 healthyR.ai   <tibble [1,082 × 2]> <tibble [28 × 2]> <split [1054|28]>
    ## 7 TidyDensity   <tibble [936 × 2]>   <tibble [28 × 2]> <split [908|28]> 
    ## 8 tidyAML       <tibble [552 × 2]>   <tibble [28 × 2]> <split [524|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.6086675 |  106.27598 | 0.7048551 | 137.48144 | 0.8692983 | 0.1083790 |
| healthyR.data |         2 | LM          | Test  | 0.8806103 |  300.27267 | 1.0197730 | 166.54262 | 1.0177017 | 0.0000430 |
| healthyR.data |         3 | EARTH       | Test  | 0.6179884 |  144.47196 | 0.7156490 | 114.68212 | 0.9115992 | 0.0000430 |
| healthyR.data |         4 | NNAR        | Test  | 0.6158987 |  105.53029 | 0.7132291 | 126.36895 | 0.8913024 | 0.0199900 |
| healthyR      |         1 | ARIMA       | Test  | 0.7174876 |   80.12365 | 0.7837968 | 133.19425 | 0.9407335 | 0.1568197 |
| healthyR      |         2 | LM          | Test  | 0.8492254 |  111.20928 | 0.9277096 | 182.35015 | 1.0472558 | 0.0151489 |
| healthyR      |         3 | EARTH       | Test  | 0.7199527 |   88.76010 | 0.7864897 | 112.60252 | 0.9597434 | 0.0151489 |
| healthyR      |         4 | NNAR        | Test  | 0.8193791 |  111.41023 | 0.8951048 | 175.65839 | 1.0027777 | 0.0996188 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6814418 |  162.68425 | 0.6828543 |  97.92034 | 0.9050986 | 0.1632205 |
| healthyR.ts   |         2 | LM          | Test  | 0.7187328 |  201.48418 | 0.7202225 | 105.96965 | 0.9893845 | 0.0447079 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7160943 |  205.56397 | 0.7175786 | 104.77227 | 0.9880960 | 0.0447079 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8295853 |  148.30057 | 0.8313049 | 174.91579 | 1.0331948 | 0.0571067 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6102849 |  452.42534 | 0.8030192 | 110.39581 | 0.7194258 | 0.2218699 |
| healthyverse  |         2 | LM          | Test  | 0.6738190 |  682.76418 | 0.8866180 | 107.37485 | 0.7831703 | 0.0475146 |
| healthyverse  |         3 | EARTH       | Test  | 0.6567514 |  583.91864 | 0.8641604 | 111.05015 | 0.7605685 |        NA |
| healthyverse  |         4 | NNAR        | Test  | 0.6010956 |  269.20466 | 0.7909278 | 123.80080 | 0.7455195 | 0.1957489 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7399041 |  101.06316 | 0.7833920 | 175.65946 | 0.9245654 | 0.1123243 |
| healthyR.ai   |         2 | LM          | Test  | 0.7533350 |  136.19789 | 0.7976123 | 132.71399 | 0.9859797 | 0.0064865 |
| healthyR.ai   |         3 | EARTH       | Test  | 3.7731459 | 1426.05855 | 3.9949127 | 160.28927 | 4.2561431 | 0.0064865 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7044597 |  120.67128 | 0.7458644 | 138.54415 | 0.8953070 | 0.1506232 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5155299 |  359.58674 | 0.7913068 |  88.54776 | 0.6261139 | 0.1578563 |
| TidyDensity   |         2 | LM          | Test  | 0.5501818 |  382.94165 | 0.8444953 |  91.05729 | 0.6649070 | 0.0164092 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7216186 |  164.76410 | 1.1076403 | 177.71700 | 0.9062251 | 0.0164092 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5635768 |  116.87130 | 0.8650558 | 131.98813 | 0.7193667 | 0.0868960 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5902179 |  145.96939 | 0.8213426 | 118.43193 | 0.6914866 | 0.2420973 |
| tidyAML       |         2 | LM          | Test  | 0.6342844 |  163.42625 | 0.8826652 | 112.30399 | 0.7536459 | 0.0062154 |
| tidyAML       |         3 | EARTH       | Test  | 3.0533363 | 1414.95002 | 4.2489989 | 140.82459 | 3.4075985 | 0.0062154 |
| tidyAML       |         4 | NNAR        | Test  | 0.5858022 |  233.80101 | 0.8151978 |  96.53454 | 0.7146686 | 0.0728253 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse   rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR.data         1 ARIMA       Test  0.609 106.  0.705 137.  0.869 0.108
    ## 2 healthyR              1 ARIMA       Test  0.717  80.1 0.784 133.  0.941 0.157
    ## 3 healthyR.ts           1 ARIMA       Test  0.681 163.  0.683  97.9 0.905 0.163
    ## 4 healthyverse          1 ARIMA       Test  0.610 452.  0.803 110.  0.719 0.222
    ## 5 healthyR.ai           4 NNAR        Test  0.704 121.  0.746 139.  0.895 0.151
    ## 6 TidyDensity           1 ARIMA       Test  0.516 360.  0.791  88.5 0.626 0.158
    ## 7 tidyAML               1 ARIMA       Test  0.590 146.  0.821 118.  0.691 0.242

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1319|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1311|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1257|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1228|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1054|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [908|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [524|28]>  <mdl_tm_t [1 × 5]>

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
