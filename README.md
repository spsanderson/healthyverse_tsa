Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
10 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 113,414
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

The last day in the data set is 2024-09-08 23:22:22, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -779.78
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 113414        |
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
| r_version     |     79802 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     79802 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     79802 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9682 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-08 | 2023-02-05 |     1386 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1176317.94 | 1557472.56 | 355 | 14701 | 271768 | 2373433 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10245.18 |   17946.07 |   1 |   317 |   3064 |   11354 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-08 23:22:22 | 2023-02-05 08:48:34 |    68598 |

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
    ## -151.39  -34.48   -9.34   26.10  800.44 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.679e+02  8.687e+01
    ## date                                                1.010e-02  4.603e-03
    ## lag(value, 1)                                       1.580e-01  2.635e-02
    ## lag(value, 7)                                       9.940e-02  2.738e-02
    ## lag(value, 14)                                      1.090e-01  2.748e-02
    ## lag(value, 21)                                      2.918e-02  2.763e-02
    ## lag(value, 28)                                      8.644e-02  2.745e-02
    ## lag(value, 35)                                      6.904e-02  2.757e-02
    ## lag(value, 42)                                      4.266e-02  2.763e-02
    ## lag(value, 49)                                      9.112e-02  2.749e-02
    ## month(date, label = TRUE).L                        -1.029e+01  5.689e+00
    ## month(date, label = TRUE).Q                         2.695e+00  5.529e+00
    ## month(date, label = TRUE).C                        -1.114e+01  5.662e+00
    ## month(date, label = TRUE)^4                        -9.309e+00  5.651e+00
    ## month(date, label = TRUE)^5                        -1.620e+01  5.579e+00
    ## month(date, label = TRUE)^6                        -4.188e+00  5.682e+00
    ## month(date, label = TRUE)^7                        -1.000e+01  5.526e+00
    ## month(date, label = TRUE)^8                        -5.828e-01  5.518e+00
    ## month(date, label = TRUE)^9                         6.817e+00  5.479e+00
    ## month(date, label = TRUE)^10                        7.211e+00  5.362e+00
    ## month(date, label = TRUE)^11                       -4.866e+00  5.268e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.154e+01  2.505e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  5.949e+00  2.593e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.933 0.053439 .  
    ## date                                                 2.193 0.028447 *  
    ## lag(value, 1)                                        5.994 2.65e-09 ***
    ## lag(value, 7)                                        3.631 0.000293 ***
    ## lag(value, 14)                                       3.965 7.73e-05 ***
    ## lag(value, 21)                                       1.056 0.291088    
    ## lag(value, 28)                                       3.149 0.001673 ** 
    ## lag(value, 35)                                       2.504 0.012404 *  
    ## lag(value, 42)                                       1.544 0.122778    
    ## lag(value, 49)                                       3.314 0.000944 ***
    ## month(date, label = TRUE).L                         -1.809 0.070722 .  
    ## month(date, label = TRUE).Q                          0.487 0.626053    
    ## month(date, label = TRUE).C                         -1.968 0.049274 *  
    ## month(date, label = TRUE)^4                         -1.647 0.099775 .  
    ## month(date, label = TRUE)^5                         -2.903 0.003757 ** 
    ## month(date, label = TRUE)^6                         -0.737 0.461151    
    ## month(date, label = TRUE)^7                         -1.810 0.070532 .  
    ## month(date, label = TRUE)^8                         -0.106 0.915900    
    ## month(date, label = TRUE)^9                          1.244 0.213705    
    ## month(date, label = TRUE)^10                         1.345 0.178918    
    ## month(date, label = TRUE)^11                        -0.924 0.355809    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.605 4.52e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.294 0.021937 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.48 on 1314 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2599, Adjusted R-squared:  0.2475 
    ## F-statistic: 20.98 on 22 and 1314 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,354 × 2]> <tibble [28 × 2]> <split [1326|28]>
    ## 2 healthyR      <tibble [1,346 × 2]> <tibble [28 × 2]> <split [1318|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,292 × 2]> <tibble [28 × 2]> <split [1264|28]>
    ## 5 healthyverse  <tibble [1,263 × 2]> <tibble [28 × 2]> <split [1235|28]>
    ## 6 healthyR.ai   <tibble [1,089 × 2]> <tibble [28 × 2]> <split [1061|28]>
    ## 7 TidyDensity   <tibble [943 × 2]>   <tibble [28 × 2]> <split [915|28]> 
    ## 8 tidyAML       <tibble [559 × 2]>   <tibble [28 × 2]> <split [531|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.6068485 |  105.35973 | 0.6966218 | 164.78229 | 0.8253772 | 0.0096525 |
| healthyR.data |         2 | LM          | Test  | 0.8595304 |  314.89359 | 0.9866839 | 170.18059 | 0.9907346 | 0.0392989 |
| healthyR.data |         3 | EARTH       | Test  | 0.5598704 |  128.68916 | 0.6426941 | 124.00285 | 0.8102285 | 0.0392989 |
| healthyR.data |         4 | NNAR        | Test  | 0.5997097 |  142.82327 | 0.6884269 | 152.20212 | 0.8164809 | 0.0018301 |
| healthyR      |         1 | ARIMA       | Test  | 0.7485639 |   90.20151 | 0.6854344 | 144.06117 | 0.9496709 | 0.0741052 |
| healthyR      |         2 | LM          | Test  | 0.8735807 |  116.68388 | 0.7999080 | 191.67160 | 1.0608953 | 0.0528793 |
| healthyR      |         3 | EARTH       | Test  | 0.6686191 |   71.52000 | 0.6122317 |  95.10753 | 0.9439037 | 0.0528793 |
| healthyR      |         4 | NNAR        | Test  | 0.8315110 |  111.76053 | 0.7613862 | 177.53615 | 0.9877602 | 0.2379064 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6635642 |  137.98042 | 0.7086232 | 111.71268 | 0.9102397 | 0.0352958 |
| healthyR.ts   |         2 | LM          | Test  | 0.6128140 |  130.89567 | 0.6544269 |  88.97856 | 0.8797624 | 0.0081310 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6078851 |  132.60324 | 0.6491633 |  87.53159 | 0.8762352 | 0.0081310 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.7649299 |  154.53389 | 0.8168722 | 180.80162 | 0.9424403 | 0.2009232 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5949689 |  262.64196 | 0.7495111 | 101.93536 | 0.7170734 | 0.0575864 |
| healthyverse  |         2 | LM          | Test  | 0.6399035 |  341.61877 | 0.8061174 |  99.21948 | 0.7690272 | 0.0163108 |
| healthyverse  |         3 | EARTH       | Test  | 0.5861924 |  238.44249 | 0.7384548 | 102.93884 | 0.7225982 | 0.0163108 |
| healthyverse  |         4 | NNAR        | Test  | 0.5859266 |  175.96541 | 0.7381200 | 114.22552 | 0.7378341 | 0.0267942 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6938410 |  118.96566 | 0.6174112 | 148.39827 | 0.9212928 | 0.0848185 |
| healthyR.ai   |         2 | LM          | Test  | 0.7284809 |  183.92052 | 0.6482354 | 135.17084 | 0.9851818 | 0.0016455 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.9664242 | 1182.59120 | 1.7498135 | 156.58020 | 2.3133580 | 0.0016455 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7142045 |  140.23006 | 0.6355315 | 144.06873 | 0.9526324 | 0.0190135 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.4893858 |  372.07422 | 0.8083136 |  87.40612 | 0.5962042 | 0.0756922 |
| TidyDensity   |         2 | LM          | Test  | 0.5263964 |  413.24300 | 0.8694435 |  88.80384 | 0.6305613 | 0.0007343 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7268041 |  150.80074 | 1.2004548 | 176.62642 | 0.9176199 | 0.0007343 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5366492 |  119.48180 | 0.8863780 | 123.92624 | 0.6998810 | 0.0743057 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5544727 |  169.94122 | 0.8470793 | 132.46275 | 0.6406978 | 0.0756254 |
| tidyAML       |         2 | LM          | Test  | 0.5626459 |  233.18497 | 0.8595657 | 114.44402 | 0.6456504 | 0.0362459 |
| tidyAML       |         3 | EARTH       | Test  | 3.3787858 | 1993.80721 | 5.1618402 | 182.69717 | 3.7622900 | 0.0362459 |
| tidyAML       |         4 | NNAR        | Test  | 0.5477489 |  304.66881 | 0.8368072 | 104.65199 | 0.6619389 | 0.0293298 |

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
    ## 1 healthyR.da…         3 EARTH       Test  0.560 129.  0.643 124.  0.810 0.0393 
    ## 2 healthyR             3 EARTH       Test  0.669  71.5 0.612  95.1 0.944 0.0529 
    ## 3 healthyR.ts          3 EARTH       Test  0.608 133.  0.649  87.5 0.876 0.00813
    ## 4 healthyverse         1 ARIMA       Test  0.595 263.  0.750 102.  0.717 0.0576 
    ## 5 healthyR.ai          1 ARIMA       Test  0.694 119.  0.617 148.  0.921 0.0848 
    ## 6 TidyDensity          1 ARIMA       Test  0.489 372.  0.808  87.4 0.596 0.0757 
    ## 7 tidyAML              1 ARIMA       Test  0.554 170.  0.847 132.  0.641 0.0756

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1326|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1318|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1264|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1235|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1061|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [915|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [531|28]>  <mdl_tm_t [1 × 5]>

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
