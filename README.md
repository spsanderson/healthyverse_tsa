Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
21 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 127,783
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

The last day in the data set is 2025-01-19 22:28:50, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3970.88
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 127783        |
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
| r_version     |     90966 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     90966 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     90966 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10861 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-19 | 2023-04-20 | 1519 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1144519.27 | 1533592.11 | 355 | 14701 | 260378 | 2367923 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10364.63 | 18054.03 | 1 | 328 | 3098 | 11923 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-19 22:28:50 | 2023-04-20 23:11:56 | 77391 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 22S |       60 |

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
    ## -154.89  -35.11   -9.62   26.98  805.72 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.888e+02  7.818e+01
    ## date                                                1.131e-02  4.143e-03
    ## lag(value, 1)                                       1.232e-01  2.531e-02
    ## lag(value, 7)                                       9.293e-02  2.625e-02
    ## lag(value, 14)                                      1.088e-01  2.626e-02
    ## lag(value, 21)                                      4.944e-02  2.648e-02
    ## lag(value, 28)                                      6.409e-02  2.631e-02
    ## lag(value, 35)                                      7.271e-02  2.644e-02
    ## lag(value, 42)                                      4.968e-02  2.651e-02
    ## lag(value, 49)                                      9.387e-02  2.636e-02
    ## month(date, label = TRUE).L                        -1.127e+01  5.340e+00
    ## month(date, label = TRUE).Q                         2.190e+00  5.202e+00
    ## month(date, label = TRUE).C                        -1.136e+01  5.278e+00
    ## month(date, label = TRUE)^4                        -7.490e+00  5.300e+00
    ## month(date, label = TRUE)^5                        -1.295e+01  5.293e+00
    ## month(date, label = TRUE)^6                        -8.582e-01  5.375e+00
    ## month(date, label = TRUE)^7                        -9.092e+00  5.277e+00
    ## month(date, label = TRUE)^8                        -2.188e+00  5.274e+00
    ## month(date, label = TRUE)^9                         4.160e+00  5.266e+00
    ## month(date, label = TRUE)^10                        5.138e+00  5.265e+00
    ## month(date, label = TRUE)^11                       -6.209e+00  5.279e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.167e+01  2.420e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.265e+00  2.538e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.414 0.015889 *  
    ## date                                                 2.729 0.006430 ** 
    ## lag(value, 1)                                        4.869 1.25e-06 ***
    ## lag(value, 7)                                        3.541 0.000412 ***
    ## lag(value, 14)                                       4.143 3.62e-05 ***
    ## lag(value, 21)                                       1.867 0.062079 .  
    ## lag(value, 28)                                       2.436 0.014987 *  
    ## lag(value, 35)                                       2.750 0.006036 ** 
    ## lag(value, 42)                                       1.874 0.061143 .  
    ## lag(value, 49)                                       3.561 0.000381 ***
    ## month(date, label = TRUE).L                         -2.110 0.035001 *  
    ## month(date, label = TRUE).Q                          0.421 0.673870    
    ## month(date, label = TRUE).C                         -2.153 0.031502 *  
    ## month(date, label = TRUE)^4                         -1.413 0.157825    
    ## month(date, label = TRUE)^5                         -2.446 0.014547 *  
    ## month(date, label = TRUE)^6                         -0.160 0.873170    
    ## month(date, label = TRUE)^7                         -1.723 0.085127 .  
    ## month(date, label = TRUE)^8                         -0.415 0.678302    
    ## month(date, label = TRUE)^9                          0.790 0.429651    
    ## month(date, label = TRUE)^10                         0.976 0.329317    
    ## month(date, label = TRUE)^11                        -1.176 0.239685    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.822 1.57e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.862 0.004264 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.92 on 1447 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2594, Adjusted R-squared:  0.2481 
    ## F-statistic: 23.04 on 22 and 1447 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,484 × 2]> <tibble [28 × 2]> <split [1456|28]>
    ## 2 healthyR      <tibble [1,477 × 2]> <tibble [28 × 2]> <split [1449|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,423 × 2]> <tibble [28 × 2]> <split [1395|28]>
    ## 5 healthyverse  <tibble [1,394 × 2]> <tibble [28 × 2]> <split [1366|28]>
    ## 6 healthyR.ai   <tibble [1,220 × 2]> <tibble [28 × 2]> <split [1192|28]>
    ## 7 TidyDensity   <tibble [1,074 × 2]> <tibble [28 × 2]> <split [1046|28]>
    ## 8 tidyAML       <tibble [690 × 2]>   <tibble [28 × 2]> <split [662|28]> 
    ## 9 RandomWalker  <tibble [124 × 2]>   <tibble [28 × 2]> <split [96|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.9660606 | 251.7826 | 0.7287414 | 166.5174 | 1.1427142 | 0.0154401 |
| healthyR.data | 2 | LM | Test | 1.0127851 | 298.4606 | 0.7639877 | 165.8210 | 1.1680434 | 0.0987496 |
| healthyR.data | 3 | EARTH | Test | 1.1500578 | 434.9121 | 0.8675384 | 161.9814 | 1.2907721 | 0.0987496 |
| healthyR.data | 4 | NNAR | Test | 0.8386507 | 109.4191 | 0.6326306 | 155.6359 | 1.0838285 | 0.0030900 |
| healthyR | 1 | ARIMA | Test | 0.7462293 | 125.3709 | 0.6475086 | 165.9818 | 0.9164523 | 0.0002247 |
| healthyR | 2 | LM | Test | 0.7201895 | 100.4234 | 0.6249137 | 183.5706 | 0.9078927 | 0.0006906 |
| healthyR | 3 | EARTH | Test | 0.7206886 | 104.1237 | 0.6253468 | 171.1062 | 0.9110183 | 0.0006906 |
| healthyR | 4 | NNAR | Test | 0.7458654 | 132.3747 | 0.6471929 | 159.2359 | 0.9355789 | 0.0024783 |
| healthyR.ts | 1 | ARIMA | Test | 0.8385886 | 265.4061 | 0.6230547 | 130.3256 | 1.0775067 | 0.0006811 |
| healthyR.ts | 2 | LM | Test | 0.8532588 | 281.9015 | 0.6339543 | 131.0542 | 1.0828739 | 0.0014699 |
| healthyR.ts | 3 | EARTH | Test | 0.8599330 | 289.3160 | 0.6389131 | 131.3419 | 1.0856031 | 0.0014699 |
| healthyR.ts | 4 | NNAR | Test | 0.8259774 | 108.1344 | 0.6136847 | 169.9118 | 1.1140883 | 0.0003085 |
| healthyverse | 1 | ARIMA | Test | 0.7158770 | 238.7358 | 0.6746307 | 114.0354 | 0.8375049 | 0.0238043 |
| healthyverse | 2 | LM | Test | 0.7440508 | 295.0817 | 0.7011812 | 107.5096 | 0.8771499 | 0.0021292 |
| healthyverse | 3 | EARTH | Test | 0.7973910 | 368.0586 | 0.7514481 | 105.3821 | 0.9463171 | 0.0021292 |
| healthyverse | 4 | NNAR | Test | 0.7062237 | 170.2594 | 0.6655336 | 126.2305 | 0.8458487 | 0.0409557 |
| healthyR.ai | 1 | ARIMA | Test | 0.6947312 | 102.1644 | 0.6902056 | 183.4309 | 0.8248161 | 0.0004603 |
| healthyR.ai | 2 | LM | Test | 0.6673449 | 131.0949 | 0.6629978 | 150.1916 | 0.8266005 | 0.0237516 |
| healthyR.ai | 3 | EARTH | Test | 0.6701633 | 188.5499 | 0.6657978 | 121.3960 | 0.8766347 | 0.0237516 |
| healthyR.ai | 4 | NNAR | Test | 0.6964455 | 132.5580 | 0.6919088 | 164.3381 | 0.8447034 | 0.0000266 |
| TidyDensity | 1 | ARIMA | Test | 0.7407566 | 149.1976 | 0.6619540 | 123.0846 | 0.8895559 | 0.0330781 |
| TidyDensity | 2 | LM | Test | 0.7976940 | 198.8795 | 0.7128344 | 114.8154 | 0.9672846 | 0.0050526 |
| TidyDensity | 3 | EARTH | Test | 0.7503616 | 152.8522 | 0.6705373 | 122.0759 | 0.9002432 | 0.0050526 |
| TidyDensity | 4 | NNAR | Test | 0.7592488 | 121.3200 | 0.6784790 | 153.1233 | 0.9004254 | 0.0074265 |
| tidyAML | 1 | ARIMA | Test | 0.9140123 | 270.7334 | 0.7348860 | 104.2895 | 1.0392362 | 0.0255064 |
| tidyAML | 2 | LM | Test | 0.9712648 | 206.1443 | 0.7809182 | 119.4539 | 1.1051215 | 0.0120817 |
| tidyAML | 3 | EARTH | Test | 0.9331123 | 294.9832 | 0.7502428 | 102.6497 | 1.0541753 | 0.0120817 |
| tidyAML | 4 | NNAR | Test | 0.9381549 | 236.1620 | 0.7542972 | 111.6861 | 1.0746623 | 0.0003888 |
| RandomWalker | 1 | ARIMA | Test | 1.1070160 | 215.4375 | 0.5458678 | 125.8308 | 1.3650028 | 0.1320745 |
| RandomWalker | 2 | LM | Test | 1.1604326 | 105.8613 | 0.5722075 | 153.4235 | 1.3768264 | 0.0003837 |
| RandomWalker | 3 | EARTH | Test | 1.1568563 | 100.4138 | 0.5704440 | 156.4170 | 1.3712000 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3485227 | 137.2397 | 0.6649544 | 158.9886 | 1.6401062 | 0.0487051 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.839  109. 0.633  156. 1.08  3.09e-3
    ## 2 healthyR             2 LM          Test  0.720  100. 0.625  184. 0.908 6.91e-4
    ## 3 healthyR.ts          1 ARIMA       Test  0.839  265. 0.623  130. 1.08  6.81e-4
    ## 4 healthyverse         1 ARIMA       Test  0.716  239. 0.675  114. 0.838 2.38e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.695  102. 0.690  183. 0.825 4.60e-4
    ## 6 TidyDensity          1 ARIMA       Test  0.741  149. 0.662  123. 0.890 3.31e-2
    ## 7 tidyAML              1 ARIMA       Test  0.914  271. 0.735  104. 1.04  2.55e-2
    ## 8 RandomWalker         1 ARIMA       Test  1.11   215. 0.546  126. 1.37  1.32e-1

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1456|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1449|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1395|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1366|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1192|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1046|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [662|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [96|28]>   <mdl_tm_t [1 × 5]>

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
