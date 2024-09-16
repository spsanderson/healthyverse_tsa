Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
16 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 114,206
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

The last day in the data set is 2024-09-14 23:52:13, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -924.27
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 114206        |
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
| r_version     |     80373 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     80373 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     80373 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9726 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-14 | 2023-02-09 |     1392 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1174680.62 | 1556166.40 | 355 | 14701 | 271098.0 | 2373201 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10273.16 |   17974.08 |   1 |   317 |   3071.5 |   11424 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-14 23:52:13 | 2023-02-09 07:21:29 |    69135 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     20 |       60 |

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
    ## -153.21  -34.13   -9.59   26.14  798.67 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.812e+02  8.706e+01
    ## date                                                1.081e-02  4.613e-03
    ## lag(value, 1)                                       1.495e-01  2.629e-02
    ## lag(value, 7)                                       1.056e-01  2.753e-02
    ## lag(value, 14)                                      1.145e-01  2.762e-02
    ## lag(value, 21)                                      2.430e-02  2.776e-02
    ## lag(value, 28)                                      8.030e-02  2.759e-02
    ## lag(value, 35)                                      6.854e-02  2.776e-02
    ## lag(value, 42)                                      3.718e-02  2.777e-02
    ## lag(value, 49)                                      9.979e-02  2.759e-02
    ## month(date, label = TRUE).L                        -1.002e+01  5.725e+00
    ## month(date, label = TRUE).Q                         2.537e+00  5.566e+00
    ## month(date, label = TRUE).C                        -1.161e+01  5.681e+00
    ## month(date, label = TRUE)^4                        -9.628e+00  5.687e+00
    ## month(date, label = TRUE)^5                        -1.582e+01  5.610e+00
    ## month(date, label = TRUE)^6                        -3.607e+00  5.697e+00
    ## month(date, label = TRUE)^7                        -9.709e+00  5.561e+00
    ## month(date, label = TRUE)^8                        -1.102e+00  5.547e+00
    ## month(date, label = TRUE)^9                         6.133e+00  5.478e+00
    ## month(date, label = TRUE)^10                        6.493e+00  5.369e+00
    ## month(date, label = TRUE)^11                       -5.123e+00  5.298e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.165e+01  2.519e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.241e+00  2.608e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.081 0.037599 *  
    ## date                                                 2.344 0.019233 *  
    ## lag(value, 1)                                        5.688 1.58e-08 ***
    ## lag(value, 7)                                        3.836 0.000131 ***
    ## lag(value, 14)                                       4.146 3.60e-05 ***
    ## lag(value, 21)                                       0.876 0.381403    
    ## lag(value, 28)                                       2.911 0.003662 ** 
    ## lag(value, 35)                                       2.469 0.013660 *  
    ## lag(value, 42)                                       1.339 0.180910    
    ## lag(value, 49)                                       3.616 0.000310 ***
    ## month(date, label = TRUE).L                         -1.751 0.080243 .  
    ## month(date, label = TRUE).Q                          0.456 0.648640    
    ## month(date, label = TRUE).C                         -2.043 0.041234 *  
    ## month(date, label = TRUE)^4                         -1.693 0.090706 .  
    ## month(date, label = TRUE)^5                         -2.821 0.004866 ** 
    ## month(date, label = TRUE)^6                         -0.633 0.526822    
    ## month(date, label = TRUE)^7                         -1.746 0.081088 .  
    ## month(date, label = TRUE)^8                         -0.199 0.842581    
    ## month(date, label = TRUE)^9                          1.119 0.263142    
    ## month(date, label = TRUE)^10                         1.209 0.226763    
    ## month(date, label = TRUE)^11                        -0.967 0.333768    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.627 4.08e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.393 0.016859 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.9 on 1320 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2592, Adjusted R-squared:  0.2469 
    ## F-statistic:    21 on 22 and 1320 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,360 × 2]> <tibble [28 × 2]> <split [1332|28]>
    ## 2 healthyR      <tibble [1,352 × 2]> <tibble [28 × 2]> <split [1324|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,298 × 2]> <tibble [28 × 2]> <split [1270|28]>
    ## 5 healthyverse  <tibble [1,269 × 2]> <tibble [28 × 2]> <split [1241|28]>
    ## 6 healthyR.ai   <tibble [1,095 × 2]> <tibble [28 × 2]> <split [1067|28]>
    ## 7 TidyDensity   <tibble [949 × 2]>   <tibble [28 × 2]> <split [921|28]> 
    ## 8 tidyAML       <tibble [565 × 2]>   <tibble [28 × 2]> <split [537|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.6500857 | 111.79358 | 0.6500091 | 158.43464 | 0.8948844 | 0.0001120 |
| healthyR.data |         2 | LM          | Test  | 0.8749922 | 504.98179 | 0.8748891 | 164.82529 | 1.0528908 | 0.0400189 |
| healthyR.data |         3 | EARTH       | Test  | 0.6505077 | 228.40522 | 0.6504310 | 145.14433 | 0.8897593 | 0.0400189 |
| healthyR.data |         4 | NNAR        | Test  | 0.6521776 | 190.46881 | 0.6521007 | 153.13667 | 0.8950366 | 0.0225322 |
| healthyR      |         1 | ARIMA       | Test  | 0.7076801 |  90.07214 | 0.6180798 | 105.91722 | 0.9828651 | 0.0388860 |
| healthyR      |         2 | LM          | Test  | 0.8486795 | 111.59764 | 0.7412271 | 186.94961 | 1.0884188 | 0.0026814 |
| healthyR      |         3 | EARTH       | Test  | 0.6989911 |  86.17923 | 0.6104909 |  99.71371 | 0.9984968 | 0.0026814 |
| healthyR      |         4 | NNAR        | Test  | 0.8301770 | 114.43476 | 0.7250672 | 163.37162 | 1.0640290 | 0.0138713 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7492187 | 103.76232 | 0.6448971 |  97.02730 | 1.0044625 | 0.0076226 |
| healthyR.ts   |         2 | LM          | Test  | 0.7416109 |  91.92342 | 0.6383487 | 104.11663 | 1.0265267 | 0.0600288 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7391965 |  92.54372 | 0.6362705 | 103.09651 | 1.0236624 | 0.0600288 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8340499 |  93.16208 | 0.7179164 | 174.63317 | 1.1057130 | 0.1602159 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7323393 | 374.30356 | 0.9049596 | 118.01558 | 0.8873669 | 0.0043256 |
| healthyverse  |         2 | LM          | Test  | 0.7523112 | 494.76434 | 0.9296390 | 112.86004 | 0.9184283 | 0.0117364 |
| healthyverse  |         3 | EARTH       | Test  | 0.6774601 | 356.26557 | 0.8371447 | 115.59098 | 0.8474487 | 0.0117364 |
| healthyverse  |         4 | NNAR        | Test  | 0.6489635 | 232.04330 | 0.8019311 | 127.65112 | 0.8284092 | 0.0585258 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.9055341 | 117.30561 | 0.6929551 | 168.65032 | 1.1591391 | 0.0545698 |
| healthyR.ai   |         2 | LM          | Test  | 0.9749479 | 168.99436 | 0.7460736 | 159.35931 | 1.2400035 | 0.0055123 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.8972833 |  97.79520 | 0.6866412 | 191.12275 | 1.1572906 | 0.0055123 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8888544 | 133.57042 | 0.6801910 | 161.22949 | 1.1332632 | 0.1062936 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5752943 | 383.11969 | 0.8035770 |  94.68508 | 0.7235775 | 0.0040893 |
| TidyDensity   |         2 | LM          | Test  | 0.6031824 | 439.32461 | 0.8425314 |  95.17695 | 0.7312499 | 0.0155791 |
| TidyDensity   |         3 | EARTH       | Test  | 0.5328520 | 271.01596 | 0.7442933 |  92.76704 | 0.7471666 | 0.0155791 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5712055 | 123.05274 | 0.7978658 | 119.33894 | 0.8436427 | 0.0991386 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5864522 | 295.39300 | 0.7868098 | 116.69593 | 0.7211795 | 0.0289528 |
| tidyAML       |         2 | LM          | Test  | 0.6162904 | 318.00587 | 0.8268421 | 120.28544 | 0.7225283 | 0.0111864 |
| tidyAML       |         3 | EARTH       | Test  | 0.6745812 | 133.71682 | 0.9050475 | 168.63597 | 0.7674652 | 0.0111864 |
| tidyAML       |         4 | NNAR        | Test  | 0.6094850 | 472.92551 | 0.8177117 | 103.67547 | 0.7754208 | 0.0043468 |

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
    ## 1 healthyR.da…         3 EARTH       Test  0.651 228.  0.650 145.  0.890 0.0400 
    ## 2 healthyR             1 ARIMA       Test  0.708  90.1 0.618 106.  0.983 0.0389 
    ## 3 healthyR.ts          1 ARIMA       Test  0.749 104.  0.645  97.0 1.00  0.00762
    ## 4 healthyverse         4 NNAR        Test  0.649 232.  0.802 128.  0.828 0.0585 
    ## 5 healthyR.ai          4 NNAR        Test  0.889 134.  0.680 161.  1.13  0.106  
    ## 6 TidyDensity          1 ARIMA       Test  0.575 383.  0.804  94.7 0.724 0.00409
    ## 7 tidyAML              1 ARIMA       Test  0.586 295.  0.787 117.  0.721 0.0290

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1332|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1324|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1270|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1241|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1067|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [921|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [537|28]>  <mdl_tm_t [1 × 5]>

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
