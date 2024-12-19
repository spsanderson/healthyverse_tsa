Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
19 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 124,276
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

The last day in the data set is 2024-12-17 20:58:09, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3177.37
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 124276        |
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
| r_version     |     88082 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     88082 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     88082 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10646 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-17 | 2023-04-06 | 1486 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1150336 | 1537375.07 | 355 | 14701 | 260378 | 2368012.0 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10353 | 17995.59 | 1 | 336 | 3100 | 11850.5 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-17 20:58:09 | 2023-04-06 19:57:46 | 75295 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     46 |       60 |

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
    ## -156.94  -34.80   -9.78   26.94  802.57 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.789e+02  8.053e+01
    ## date                                                1.074e-02  4.267e-03
    ## lag(value, 1)                                       1.351e-01  2.558e-02
    ## lag(value, 7)                                       9.351e-02  2.652e-02
    ## lag(value, 14)                                      1.052e-01  2.655e-02
    ## lag(value, 21)                                      4.425e-02  2.667e-02
    ## lag(value, 28)                                      7.280e-02  2.649e-02
    ## lag(value, 35)                                      6.517e-02  2.673e-02
    ## lag(value, 42)                                      4.399e-02  2.679e-02
    ## lag(value, 49)                                      1.088e-01  2.669e-02
    ## month(date, label = TRUE).L                        -1.086e+01  5.509e+00
    ## month(date, label = TRUE).Q                         2.197e+00  5.366e+00
    ## month(date, label = TRUE).C                        -1.107e+01  5.431e+00
    ## month(date, label = TRUE)^4                        -7.551e+00  5.392e+00
    ## month(date, label = TRUE)^5                        -1.257e+01  5.335e+00
    ## month(date, label = TRUE)^6                        -9.930e-01  5.389e+00
    ## month(date, label = TRUE)^7                        -8.888e+00  5.277e+00
    ## month(date, label = TRUE)^8                        -2.354e+00  5.269e+00
    ## month(date, label = TRUE)^9                         4.283e+00  5.259e+00
    ## month(date, label = TRUE)^10                        5.149e+00  5.258e+00
    ## month(date, label = TRUE)^11                       -6.172e+00  5.272e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.168e+01  2.441e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.793e+00  2.554e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.222 0.026436 *  
    ## date                                                 2.517 0.011957 *  
    ## lag(value, 1)                                        5.281 1.49e-07 ***
    ## lag(value, 7)                                        3.526 0.000435 ***
    ## lag(value, 14)                                       3.961 7.82e-05 ***
    ## lag(value, 21)                                       1.659 0.097301 .  
    ## lag(value, 28)                                       2.748 0.006078 ** 
    ## lag(value, 35)                                       2.438 0.014908 *  
    ## lag(value, 42)                                       1.642 0.100885    
    ## lag(value, 49)                                       4.076 4.83e-05 ***
    ## month(date, label = TRUE).L                         -1.972 0.048806 *  
    ## month(date, label = TRUE).Q                          0.409 0.682302    
    ## month(date, label = TRUE).C                         -2.038 0.041759 *  
    ## month(date, label = TRUE)^4                         -1.400 0.161588    
    ## month(date, label = TRUE)^5                         -2.356 0.018623 *  
    ## month(date, label = TRUE)^6                         -0.184 0.853833    
    ## month(date, label = TRUE)^7                         -1.684 0.092334 .  
    ## month(date, label = TRUE)^8                         -0.447 0.655135    
    ## month(date, label = TRUE)^9                          0.815 0.415493    
    ## month(date, label = TRUE)^10                         0.979 0.327663    
    ## month(date, label = TRUE)^11                        -1.171 0.241896    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.786 1.88e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.660 0.007903 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.83 on 1414 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:   0.26,  Adjusted R-squared:  0.2485 
    ## F-statistic: 22.59 on 22 and 1414 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,452 × 2]> <tibble [28 × 2]> <split [1424|28]>
    ## 2 healthyR      <tibble [1,445 × 2]> <tibble [28 × 2]> <split [1417|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,391 × 2]> <tibble [28 × 2]> <split [1363|28]>
    ## 5 healthyverse  <tibble [1,362 × 2]> <tibble [28 × 2]> <split [1334|28]>
    ## 6 healthyR.ai   <tibble [1,188 × 2]> <tibble [28 × 2]> <split [1160|28]>
    ## 7 TidyDensity   <tibble [1,042 × 2]> <tibble [28 × 2]> <split [1014|28]>
    ## 8 tidyAML       <tibble [658 × 2]>   <tibble [28 × 2]> <split [630|28]> 
    ## 9 RandomWalker  <tibble [92 × 2]>    <tibble [28 × 2]> <split [64|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7446308 | 226.71718 | 0.6125041 | 145.78672 | 0.9127697 | 0.1424074 |
| healthyR.data | 2 | LM | Test | 0.8066751 | 299.02525 | 0.6635393 | 145.95642 | 0.9442676 | 0.0133093 |
| healthyR.data | 3 | EARTH | Test | 0.8224375 | 370.88393 | 0.6765048 | 141.28147 | 0.9356338 | 0.0133093 |
| healthyR.data | 4 | NNAR | Test | 0.8419468 | 134.65892 | 0.6925524 | 166.65252 | 1.1010282 | 0.0033829 |
| healthyR | 1 | ARIMA | Test | 0.6386840 | 141.75668 | 0.7414331 | 152.35439 | 0.7649530 | 0.0567903 |
| healthyR | 2 | LM | Test | 0.6629288 | 111.54113 | 0.7695783 | 190.52755 | 0.7743904 | 0.0077999 |
| healthyR | 3 | EARTH | Test | 0.6927181 | 219.68269 | 0.8041600 | 138.05123 | 0.8333291 | 0.0077999 |
| healthyR | 4 | NNAR | Test | 0.6396928 | 140.61034 | 0.7426042 | 164.03575 | 0.7537525 | 0.0597948 |
| healthyR.ts | 1 | ARIMA | Test | 0.8488803 | 99.00237 | 0.8575781 | 110.45519 | 1.0297577 | 0.0052423 |
| healthyR.ts | 2 | LM | Test | 0.8422013 | 93.36243 | 0.8508307 | 112.90224 | 1.0169015 | 0.0052423 |
| healthyR.ts | 3 | EARTH | Test | 0.8422087 | 94.22153 | 0.8508382 | 112.15879 | 1.0190376 | 0.0052423 |
| healthyR.ts | 4 | NNAR | Test | 0.8785854 | 96.03664 | 0.8875876 | 176.09866 | 1.0520643 | 0.0050383 |
| healthyverse | 1 | ARIMA | Test | 0.4872722 | 210.17098 | 0.9607515 | 87.19809 | 0.6044576 | 0.4103024 |
| healthyverse | 2 | LM | Test | 0.5211268 | 323.95387 | 1.0275024 | 82.12911 | 0.6347444 | 0.0375545 |
| healthyverse | 3 | EARTH | Test | 0.5285992 | 360.39415 | 1.0422356 | 81.87207 | 0.6329843 | 0.0375545 |
| healthyverse | 4 | NNAR | Test | 0.5698080 | 200.83086 | 1.1234868 | 102.68589 | 0.6922214 | 0.0341398 |
| healthyR.ai | 1 | ARIMA | Test | 0.7084891 | 93.31630 | 1.0170938 | 162.82636 | 0.8050213 | 0.1169457 |
| healthyR.ai | 2 | LM | Test | 0.7319270 | 122.74293 | 1.0507408 | 164.49811 | 0.8079728 | 0.0092927 |
| healthyR.ai | 3 | EARTH | Test | 1.9473896 | 754.12113 | 2.7956364 | 137.34211 | 2.2094922 | 0.0092927 |
| healthyR.ai | 4 | NNAR | Test | 0.6789684 | 104.05949 | 0.9747144 | 159.21140 | 0.7726903 | 0.1208256 |
| TidyDensity | 1 | ARIMA | Test | 0.6634642 | 198.20530 | 0.7708399 | 109.69514 | 0.8098503 | 0.1159842 |
| TidyDensity | 2 | LM | Test | 0.7479536 | 273.56707 | 0.8690032 | 111.51229 | 0.8794406 | 0.0137222 |
| TidyDensity | 3 | EARTH | Test | 0.7076995 | 199.06007 | 0.8222343 | 118.17085 | 0.8495220 | 0.0137222 |
| TidyDensity | 4 | NNAR | Test | 0.7709127 | 122.82688 | 0.8956780 | 155.36730 | 0.9465172 | 0.0124827 |
| tidyAML | 1 | ARIMA | Test | 0.6920708 | 88.80831 | 0.9022304 | 93.11650 | 0.8572739 | 0.1150062 |
| tidyAML | 2 | LM | Test | 0.7003435 | 89.89696 | 0.9130152 | 88.24613 | 0.8734694 | 0.0146365 |
| tidyAML | 3 | EARTH | Test | 0.6634881 | 117.40221 | 0.8649680 | 79.76152 | 0.7922454 | 0.0146365 |
| tidyAML | 4 | NNAR | Test | 0.6688676 | 110.81012 | 0.8719812 | 82.80078 | 0.7943643 | 0.0459098 |
| RandomWalker | 1 | ARIMA | Test | 1.4018449 | 100.00000 | 0.6306489 | 200.00000 | 1.5019178 | NA |
| RandomWalker | 2 | LM | Test | 1.4088226 | 100.48012 | 0.6337879 | 194.38469 | 1.5067527 | 0.0000165 |
| RandomWalker | 3 | EARTH | Test | 1.3054324 | 92.75007 | 0.5872757 | 119.31953 | 1.5791395 | 0.0000165 |
| RandomWalker | 4 | NNAR | Test | 1.6763394 | 160.54983 | 0.7541359 | 151.91255 | 1.8850291 | 0.0049064 |

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
    ## 1 healthyR.d…         1 ARIMA       Test  0.745 227.  0.613 146.  0.913  0.142  
    ## 2 healthyR            4 NNAR        Test  0.640 141.  0.743 164.  0.754  0.0598 
    ## 3 healthyR.ts         2 LM          Test  0.842  93.4 0.851 113.  1.02   0.00524
    ## 4 healthyver…         1 ARIMA       Test  0.487 210.  0.961  87.2 0.604  0.410  
    ## 5 healthyR.ai         4 NNAR        Test  0.679 104.  0.975 159.  0.773  0.121  
    ## 6 TidyDensity         1 ARIMA       Test  0.663 198.  0.771 110.  0.810  0.116  
    ## 7 tidyAML             3 EARTH       Test  0.663 117.  0.865  79.8 0.792  0.0146 
    ## 8 RandomWalk…         1 ARIMA       Test  1.40  100   0.631 200   1.50  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1424|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1417|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1363|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1334|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1160|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1014|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [630|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [64|28]>   <mdl_tm_t [1 × 5]>

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
