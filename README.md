Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 133,261
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

The last day in the data set is 2025-03-04 22:32:54, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5026.95
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 133261        |
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
| r_version     |     95644 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     95644 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     95644 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11256 |          0.92 |   2 |   2 |     0 |      162 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-04 | 2023-05-13 | 1563 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1137697.97 | 1527136.30 | 355 | 14701 | 261421 | 2367806 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10393.99 | 18386.55 | 1 | 302 | 3089 | 11878 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-04 22:32:54 | 2023-05-13 23:42:21 | 80851 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 24S |       60 |

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
    ## -153.53  -34.73  -10.16   26.98  810.09 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.165e+02  7.580e+01
    ## date                                                1.282e-02  4.021e-03
    ## lag(value, 1)                                       1.119e-01  2.505e-02
    ## lag(value, 7)                                       8.974e-02  2.593e-02
    ## lag(value, 14)                                      9.912e-02  2.607e-02
    ## lag(value, 21)                                      6.782e-02  2.615e-02
    ## lag(value, 28)                                      6.242e-02  2.611e-02
    ## lag(value, 35)                                      7.070e-02  2.630e-02
    ## lag(value, 42)                                      5.177e-02  2.642e-02
    ## lag(value, 49)                                      8.471e-02  2.628e-02
    ## month(date, label = TRUE).L                        -1.236e+01  5.199e+00
    ## month(date, label = TRUE).Q                         2.357e+00  5.193e+00
    ## month(date, label = TRUE).C                        -1.076e+01  5.279e+00
    ## month(date, label = TRUE)^4                        -8.714e+00  5.253e+00
    ## month(date, label = TRUE)^5                        -1.172e+01  5.210e+00
    ## month(date, label = TRUE)^6                        -2.079e+00  5.294e+00
    ## month(date, label = TRUE)^7                        -8.355e+00  5.235e+00
    ## month(date, label = TRUE)^8                        -2.616e+00  5.275e+00
    ## month(date, label = TRUE)^9                         4.291e+00  5.299e+00
    ## month(date, label = TRUE)^10                        5.125e+00  5.315e+00
    ## month(date, label = TRUE)^11                       -6.224e+00  5.331e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.183e+01  2.408e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.958e+00  2.538e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.857 0.004338 ** 
    ## date                                                 3.190 0.001455 ** 
    ## lag(value, 1)                                        4.466 8.57e-06 ***
    ## lag(value, 7)                                        3.460 0.000555 ***
    ## lag(value, 14)                                       3.801 0.000150 ***
    ## lag(value, 21)                                       2.594 0.009587 ** 
    ## lag(value, 28)                                       2.391 0.016940 *  
    ## lag(value, 35)                                       2.688 0.007262 ** 
    ## lag(value, 42)                                       1.960 0.050232 .  
    ## lag(value, 49)                                       3.224 0.001293 ** 
    ## month(date, label = TRUE).L                         -2.377 0.017557 *  
    ## month(date, label = TRUE).Q                          0.454 0.650031    
    ## month(date, label = TRUE).C                         -2.039 0.041624 *  
    ## month(date, label = TRUE)^4                         -1.659 0.097343 .  
    ## month(date, label = TRUE)^5                         -2.249 0.024630 *  
    ## month(date, label = TRUE)^6                         -0.393 0.694598    
    ## month(date, label = TRUE)^7                         -1.596 0.110687    
    ## month(date, label = TRUE)^8                         -0.496 0.619978    
    ## month(date, label = TRUE)^9                          0.810 0.418232    
    ## month(date, label = TRUE)^10                         0.964 0.335030    
    ## month(date, label = TRUE)^11                        -1.167 0.243201    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.912 9.98e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.136 0.001746 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.51 on 1491 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2594, Adjusted R-squared:  0.2484 
    ## F-statistic: 23.73 on 22 and 1491 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,556 × 2]> <tibble [28 × 2]> <split [1528|28]>
    ## 2 healthyR      <tibble [1,549 × 2]> <tibble [28 × 2]> <split [1521|28]>
    ## 3 healthyR.ts   <tibble [1,493 × 2]> <tibble [28 × 2]> <split [1465|28]>
    ## 4 healthyverse  <tibble [1,464 × 2]> <tibble [28 × 2]> <split [1436|28]>
    ## 5 healthyR.ai   <tibble [1,288 × 2]> <tibble [28 × 2]> <split [1260|28]>
    ## 6 TidyDensity   <tibble [1,139 × 2]> <tibble [28 × 2]> <split [1111|28]>
    ## 7 tidyAML       <tibble [747 × 2]>   <tibble [28 × 2]> <split [719|28]> 
    ## 8 RandomWalker  <tibble [169 × 2]>   <tibble [28 × 2]> <split [141|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6892974 | 170.56750 | 0.6396662 | 152.99580 | 0.8016718 | 0.2452797 |
| healthyR.data | 2 | LM | Test | 0.8319348 | 222.68131 | 0.7720332 | 153.48050 | 0.9689177 | 0.0363635 |
| healthyR.data | 3 | EARTH | Test | 1.9894992 | 989.35865 | 1.8462499 | 159.66907 | 2.1943799 | 0.0363635 |
| healthyR.data | 4 | NNAR | Test | 0.7018474 | 109.72185 | 0.6513125 | 175.67400 | 0.8425885 | 0.0010789 |
| healthyR | 1 | ARIMA | Test | 0.9488409 | 137.51183 | 0.7206989 | 186.07659 | 1.1305676 | 0.0299489 |
| healthyR | 2 | LM | Test | 0.9226088 | 121.83293 | 0.7007742 | 189.35974 | 1.0947895 | 0.0369246 |
| healthyR | 3 | EARTH | Test | 0.9660557 | 179.32857 | 0.7337746 | 177.52083 | 1.1309503 | 0.0369246 |
| healthyR | 4 | NNAR | Test | 0.9563021 | 172.50178 | 0.7263662 | 176.80747 | 1.1234290 | 0.0009442 |
| healthyR.ts | 1 | ARIMA | Test | 1.0191117 | 99.59896 | 0.6445996 | 147.42703 | 1.2293002 | 0.0281646 |
| healthyR.ts | 2 | LM | Test | 0.9430330 | 105.84552 | 0.5964789 | 119.86423 | 1.1407073 | 0.0281646 |
| healthyR.ts | 3 | EARTH | Test | 0.9446660 | 105.79692 | 0.5975118 | 120.38792 | 1.1419349 | NA |
| healthyR.ts | 4 | NNAR | Test | 1.1188036 | 141.02238 | 0.7076558 | 185.74427 | 1.3301093 | 0.0011132 |
| healthyverse | 1 | ARIMA | Test | 0.6839766 | 181.33222 | 0.8621400 | 118.31396 | 0.8505586 | 0.0484974 |
| healthyverse | 2 | LM | Test | 0.7356006 | 216.18537 | 0.9272111 | 117.06273 | 0.9109677 | 0.0699334 |
| healthyverse | 3 | EARTH | Test | 0.6298446 | 158.88050 | 0.7939077 | 116.64861 | 0.7875416 | 0.0699334 |
| healthyverse | 4 | NNAR | Test | 0.5849477 | 125.12048 | 0.7373159 | 121.31669 | 0.7308892 | 0.0117740 |
| healthyR.ai | 1 | ARIMA | Test | 0.8507371 | 104.61142 | 0.6872132 | 168.67038 | 0.9972864 | 0.0039283 |
| healthyR.ai | 2 | LM | Test | 0.8566383 | 105.04111 | 0.6919802 | 157.66090 | 1.0264470 | 0.0711111 |
| healthyR.ai | 3 | EARTH | Test | 0.8707686 | 109.39559 | 0.7033944 | 144.94775 | 1.0651174 | 0.0711111 |
| healthyR.ai | 4 | NNAR | Test | 0.8659820 | 121.69133 | 0.6995278 | 165.40891 | 1.0022392 | 0.0015533 |
| TidyDensity | 1 | ARIMA | Test | 0.6943427 | 99.21482 | 0.6909348 | 112.23967 | 0.8509575 | 0.0345375 |
| TidyDensity | 2 | LM | Test | 0.6518493 | 139.22591 | 0.6486500 | 93.64496 | 0.7810824 | 0.0163806 |
| TidyDensity | 3 | EARTH | Test | 0.7060870 | 101.71475 | 0.7026215 | 114.20244 | 0.8712918 | 0.0163806 |
| TidyDensity | 4 | NNAR | Test | 0.7644125 | 92.96955 | 0.7606607 | 150.79206 | 0.9500352 | 0.0833193 |
| tidyAML | 1 | ARIMA | Test | 0.6675286 | 193.40200 | 0.7288579 | 100.22877 | 0.7795936 | 0.0375584 |
| tidyAML | 2 | LM | Test | 0.6158850 | 166.09386 | 0.6724696 | 96.63925 | 0.7608317 | 0.0578428 |
| tidyAML | 3 | EARTH | Test | 0.7237776 | 213.75246 | 0.7902749 | 102.90287 | 0.8373883 | 0.0578428 |
| tidyAML | 4 | NNAR | Test | 0.6298558 | 175.85147 | 0.6877239 | 97.18090 | 0.7838749 | 0.0173825 |
| RandomWalker | 1 | ARIMA | Test | 0.7883585 | 158.02954 | 0.3526719 | 79.31513 | 0.9308817 | 0.6388430 |
| RandomWalker | 2 | LM | Test | 1.3226599 | 101.96409 | 0.5916914 | 198.39625 | 1.5187173 | 0.0115578 |
| RandomWalker | 3 | EARTH | Test | 1.2843636 | 84.90399 | 0.5745596 | 150.77604 | 1.5312412 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4490626 | 196.42516 | 0.6482376 | 150.85467 | 1.6857221 | 0.0024778 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.689  171. 0.640 153.  0.802 0.245  
    ## 2 healthyR             2 LM          Test  0.923  122. 0.701 189.  1.09  0.0369 
    ## 3 healthyR.ts          2 LM          Test  0.943  106. 0.596 120.  1.14  0.0282 
    ## 4 healthyverse         4 NNAR        Test  0.585  125. 0.737 121.  0.731 0.0118 
    ## 5 healthyR.ai          1 ARIMA       Test  0.851  105. 0.687 169.  0.997 0.00393
    ## 6 TidyDensity          2 LM          Test  0.652  139. 0.649  93.6 0.781 0.0164 
    ## 7 tidyAML              2 LM          Test  0.616  166. 0.672  96.6 0.761 0.0578 
    ## 8 RandomWalker         1 ARIMA       Test  0.788  158. 0.353  79.3 0.931 0.639

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1528|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1521|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1465|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1436|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1260|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1111|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [719|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [141|28]>  <mdl_tm_t [1 × 5]>

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
