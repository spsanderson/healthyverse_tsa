Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
28 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 128,695
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

The last day in the data set is 2025-01-26 23:48:59, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4140.22
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 128695        |
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
| r_version     |     91763 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     91763 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     91763 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10897 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-26 | 2023-04-24 | 1526 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1142776.05 | 1532046.5 | 355 | 14701.0 | 260378 | 2367894 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10462.97 | 18482.5 | 1 | 327.5 | 3091 | 11941 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-26 23:48:59 | 2023-04-24 18:42:43 | 77971 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 30S |       60 |

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
    ## -155.88  -35.20   -9.75   27.21  804.61 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.899e+02  7.787e+01
    ## date                                                1.136e-02  4.128e-03
    ## lag(value, 1)                                       1.195e-01  2.523e-02
    ## lag(value, 7)                                       9.659e-02  2.623e-02
    ## lag(value, 14)                                      1.120e-01  2.626e-02
    ## lag(value, 21)                                      5.128e-02  2.636e-02
    ## lag(value, 28)                                      6.650e-02  2.630e-02
    ## lag(value, 35)                                      6.840e-02  2.641e-02
    ## lag(value, 42)                                      4.967e-02  2.650e-02
    ## lag(value, 49)                                      9.280e-02  2.635e-02
    ## month(date, label = TRUE).L                        -1.145e+01  5.309e+00
    ## month(date, label = TRUE).Q                         2.366e+00  5.181e+00
    ## month(date, label = TRUE).C                        -1.154e+01  5.261e+00
    ## month(date, label = TRUE)^4                        -7.240e+00  5.290e+00
    ## month(date, label = TRUE)^5                        -1.303e+01  5.293e+00
    ## month(date, label = TRUE)^6                        -6.421e-01  5.378e+00
    ## month(date, label = TRUE)^7                        -9.128e+00  5.282e+00
    ## month(date, label = TRUE)^8                        -2.110e+00  5.280e+00
    ## month(date, label = TRUE)^9                         4.195e+00  5.272e+00
    ## month(date, label = TRUE)^10                        5.049e+00  5.271e+00
    ## month(date, label = TRUE)^11                       -6.109e+00  5.285e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.175e+01  2.417e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.255e+00  2.538e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.438 0.014888 *  
    ## date                                                 2.751 0.006014 ** 
    ## lag(value, 1)                                        4.738 2.37e-06 ***
    ## lag(value, 7)                                        3.682 0.000240 ***
    ## lag(value, 14)                                       4.267 2.11e-05 ***
    ## lag(value, 21)                                       1.945 0.051938 .  
    ## lag(value, 28)                                       2.528 0.011582 *  
    ## lag(value, 35)                                       2.590 0.009698 ** 
    ## lag(value, 42)                                       1.874 0.061107 .  
    ## lag(value, 49)                                       3.522 0.000441 ***
    ## month(date, label = TRUE).L                         -2.157 0.031150 *  
    ## month(date, label = TRUE).Q                          0.457 0.647917    
    ## month(date, label = TRUE).C                         -2.194 0.028372 *  
    ## month(date, label = TRUE)^4                         -1.368 0.171375    
    ## month(date, label = TRUE)^5                         -2.461 0.013972 *  
    ## month(date, label = TRUE)^6                         -0.119 0.904966    
    ## month(date, label = TRUE)^7                         -1.728 0.084210 .  
    ## month(date, label = TRUE)^8                         -0.400 0.689547    
    ## month(date, label = TRUE)^9                          0.796 0.426345    
    ## month(date, label = TRUE)^10                         0.958 0.338254    
    ## month(date, label = TRUE)^11                        -1.156 0.247843    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.861 1.30e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.858 0.004320 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.98 on 1454 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2618, Adjusted R-squared:  0.2506 
    ## F-statistic: 23.44 on 22 and 1454 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,491 × 2]> <tibble [28 × 2]> <split [1463|28]>
    ## 2 healthyR      <tibble [1,484 × 2]> <tibble [28 × 2]> <split [1456|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,430 × 2]> <tibble [28 × 2]> <split [1402|28]>
    ## 5 healthyverse  <tibble [1,401 × 2]> <tibble [28 × 2]> <split [1373|28]>
    ## 6 healthyR.ai   <tibble [1,227 × 2]> <tibble [28 × 2]> <split [1199|28]>
    ## 7 TidyDensity   <tibble [1,081 × 2]> <tibble [28 × 2]> <split [1053|28]>
    ## 8 tidyAML       <tibble [697 × 2]>   <tibble [28 × 2]> <split [669|28]> 
    ## 9 RandomWalker  <tibble [131 × 2]>   <tibble [28 × 2]> <split [103|28]>

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
| healthyR.data | 1 | ARIMA | Test | 1.0425123 | 245.79294 | 0.7749017 | 158.34711 | 1.2162429 | 0.0003559 |
| healthyR.data | 2 | LM | Test | 1.0478816 | 238.15463 | 0.7788927 | 160.18152 | 1.2100185 | 0.0067081 |
| healthyR.data | 3 | EARTH | Test | 1.2132034 | 349.54292 | 0.9017768 | 160.25374 | 1.3597735 | 0.0067081 |
| healthyR.data | 4 | NNAR | Test | 0.8611488 | 98.30509 | 0.6400938 | 163.71588 | 1.0563842 | 0.0395808 |
| healthyR | 1 | ARIMA | Test | 0.7534164 | 106.03117 | 0.6819625 | 167.88709 | 0.9485477 | 0.0011619 |
| healthyR | 2 | LM | Test | 0.7669643 | 102.77420 | 0.6942255 | 187.14023 | 0.9505720 | 0.0004889 |
| healthyR | 3 | EARTH | Test | 0.7731174 | 110.75542 | 0.6997950 | 164.10740 | 0.9681108 | 0.0004889 |
| healthyR | 4 | NNAR | Test | 0.7623846 | 125.96966 | 0.6900802 | 164.20981 | 0.9633875 | 0.0063141 |
| healthyR.ts | 1 | ARIMA | Test | 0.7177663 | 229.08841 | 0.5657708 | 125.01788 | 0.9915409 | 0.0011649 |
| healthyR.ts | 2 | LM | Test | 0.7561897 | 301.59458 | 0.5960576 | 125.06856 | 0.9940414 | 0.0006507 |
| healthyR.ts | 3 | EARTH | Test | 0.7606550 | 309.14833 | 0.5995773 | 125.10922 | 0.9951344 | 0.0006507 |
| healthyR.ts | 4 | NNAR | Test | 0.7653755 | 131.50407 | 0.6032981 | 176.08093 | 1.0774872 | 0.0003505 |
| healthyverse | 1 | ARIMA | Test | 0.7081975 | 239.39775 | 0.6591321 | 108.72929 | 0.8548929 | 0.0122643 |
| healthyverse | 2 | LM | Test | 0.7374038 | 291.79935 | 0.6863150 | 104.14161 | 0.8897909 | 0.0092701 |
| healthyverse | 3 | EARTH | Test | 0.8227325 | 368.96572 | 0.7657319 | 104.84538 | 0.9789317 | 0.0092701 |
| healthyverse | 4 | NNAR | Test | 0.7051563 | 183.82123 | 0.6563016 | 119.95671 | 0.8579168 | 0.0098013 |
| healthyR.ai | 1 | ARIMA | Test | 0.7140918 | 113.35442 | 0.6630683 | 180.41209 | 0.9084216 | 0.0031445 |
| healthyR.ai | 2 | LM | Test | 0.7291291 | 144.36115 | 0.6770312 | 160.84308 | 0.9208661 | 0.0001140 |
| healthyR.ai | 3 | EARTH | Test | 0.7522259 | 203.79002 | 0.6984776 | 136.10863 | 0.9850709 | 0.0001140 |
| healthyR.ai | 4 | NNAR | Test | 0.7414502 | 137.05796 | 0.6884719 | 162.91349 | 0.9521048 | 0.0079488 |
| TidyDensity | 1 | ARIMA | Test | 0.6692048 | 166.19501 | 0.6582160 | 113.06816 | 0.8470851 | 0.0166270 |
| TidyDensity | 2 | LM | Test | 0.7286335 | 238.39202 | 0.7166689 | 104.80656 | 0.9067901 | 0.0002535 |
| TidyDensity | 3 | EARTH | Test | 0.6662707 | 166.32295 | 0.6553301 | 106.71991 | 0.8551767 | 0.0002535 |
| TidyDensity | 4 | NNAR | Test | 0.7551788 | 118.67300 | 0.7427783 | 147.92866 | 0.9359817 | 0.0531162 |
| tidyAML | 1 | ARIMA | Test | 0.8636827 | 294.83619 | 0.7749563 | 97.73077 | 1.0640717 | 0.0752924 |
| tidyAML | 2 | LM | Test | 0.8902880 | 211.31904 | 0.7988284 | 111.41260 | 1.0759184 | 0.0382487 |
| tidyAML | 3 | EARTH | Test | 1.1485946 | 431.88132 | 1.0305990 | 106.31588 | 1.3308169 | 0.0382487 |
| tidyAML | 4 | NNAR | Test | 0.8949871 | 226.00842 | 0.8030448 | 105.91715 | 1.0885092 | 0.0034809 |
| RandomWalker | 1 | ARIMA | Test | 1.4411979 | 168.44564 | 0.7071123 | 139.54124 | 1.7582100 | 0.0081832 |
| RandomWalker | 2 | LM | Test | 1.2580202 | 106.17521 | 0.6172376 | 160.61677 | 1.4610142 | 0.0011569 |
| RandomWalker | 3 | EARTH | Test | 1.2527962 | 102.50391 | 0.6146745 | 163.93356 | 1.4521169 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4978376 | 137.35672 | 0.7349021 | 156.45025 | 1.7746185 | 0.1115836 |

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
    ##   package     .model_id .model_desc .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>           <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR.d…         4 NNAR        Test  0.861  98.3 0.640 164.  1.06   0.0396 
    ## 2 healthyR            1 ARIMA       Test  0.753 106.  0.682 168.  0.949  0.00116
    ## 3 healthyR.ts         1 ARIMA       Test  0.718 229.  0.566 125.  0.992  0.00116
    ## 4 healthyver…         1 ARIMA       Test  0.708 239.  0.659 109.  0.855  0.0123 
    ## 5 healthyR.ai         1 ARIMA       Test  0.714 113.  0.663 180.  0.908  0.00314
    ## 6 TidyDensity         1 ARIMA       Test  0.669 166.  0.658 113.  0.847  0.0166 
    ## 7 tidyAML             1 ARIMA       Test  0.864 295.  0.775  97.7 1.06   0.0753 
    ## 8 RandomWalk…         3 EARTH       Test  1.25  103.  0.615 164.  1.45  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1463|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1456|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1402|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1373|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1199|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1053|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [669|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [103|28]>  <mdl_tm_t [1 × 5]>

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
