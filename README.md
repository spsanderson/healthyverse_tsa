Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
29 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 112,318
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

The last day in the data set is 2024-08-27 23:43:28, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -492.13
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 112318        |
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
| r_version     |     78897 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78897 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78897 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9604 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-27 | 2023-02-01 |     1374 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |       p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|----------:|--------:|:------|
| size          |         0 |             1 | 1180041.69 | 1559690.59 | 355 | 14701 | 279942.5 | 2373526.0 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10282.82 |   17995.41 |   1 |   317 |   3064.0 |   11455.5 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-27 23:43:28 | 2023-02-01 03:16:42 |    67943 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     35 |       60 |

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
    ## -150.79  -34.39   -9.64   25.81  799.00 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.679e+02  8.786e+01
    ## date                                                1.007e-02  4.654e-03
    ## lag(value, 1)                                       1.616e-01  2.645e-02
    ## lag(value, 7)                                       1.004e-01  2.751e-02
    ## lag(value, 14)                                      1.156e-01  2.763e-02
    ## lag(value, 21)                                      3.337e-02  2.773e-02
    ## lag(value, 28)                                      8.919e-02  2.753e-02
    ## lag(value, 35)                                      6.600e-02  2.774e-02
    ## lag(value, 42)                                      3.760e-02  2.790e-02
    ## lag(value, 49)                                      8.688e-02  2.768e-02
    ## month(date, label = TRUE).L                        -1.021e+01  5.701e+00
    ## month(date, label = TRUE).Q                         2.557e+00  5.541e+00
    ## month(date, label = TRUE).C                        -1.122e+01  5.708e+00
    ## month(date, label = TRUE)^4                        -8.753e+00  5.668e+00
    ## month(date, label = TRUE)^5                        -1.591e+01  5.605e+00
    ## month(date, label = TRUE)^6                        -3.874e+00  5.723e+00
    ## month(date, label = TRUE)^7                        -1.033e+01  5.543e+00
    ## month(date, label = TRUE)^8                        -4.621e-01  5.549e+00
    ## month(date, label = TRUE)^9                         7.164e+00  5.547e+00
    ## month(date, label = TRUE)^10                        7.619e+00  5.432e+00
    ## month(date, label = TRUE)^11                       -4.201e+00  5.298e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.134e+01  2.514e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  5.895e+00  2.602e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.911 0.056277 .  
    ## date                                                 2.163 0.030706 *  
    ## lag(value, 1)                                        6.111 1.30e-09 ***
    ## lag(value, 7)                                        3.648 0.000274 ***
    ## lag(value, 14)                                       4.183 3.07e-05 ***
    ## lag(value, 21)                                       1.203 0.229044    
    ## lag(value, 28)                                       3.240 0.001226 ** 
    ## lag(value, 35)                                       2.379 0.017493 *  
    ## lag(value, 42)                                       1.348 0.177943    
    ## lag(value, 49)                                       3.139 0.001734 ** 
    ## month(date, label = TRUE).L                         -1.791 0.073570 .  
    ## month(date, label = TRUE).Q                          0.461 0.644526    
    ## month(date, label = TRUE).C                         -1.966 0.049558 *  
    ## month(date, label = TRUE)^4                         -1.544 0.122784    
    ## month(date, label = TRUE)^5                         -2.838 0.004610 ** 
    ## month(date, label = TRUE)^6                         -0.677 0.498545    
    ## month(date, label = TRUE)^7                         -1.864 0.062523 .  
    ## month(date, label = TRUE)^8                         -0.083 0.933648    
    ## month(date, label = TRUE)^9                          1.292 0.196726    
    ## month(date, label = TRUE)^10                         1.403 0.160979    
    ## month(date, label = TRUE)^11                        -0.793 0.427934    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.509 7.09e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.266 0.023641 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.52 on 1302 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2608, Adjusted R-squared:  0.2483 
    ## F-statistic: 20.88 on 22 and 1302 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,345 × 2]> <tibble [28 × 2]> <split [1317|28]>
    ## 2 healthyR      <tibble [1,337 × 2]> <tibble [28 × 2]> <split [1309|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,283 × 2]> <tibble [28 × 2]> <split [1255|28]>
    ## 5 healthyverse  <tibble [1,254 × 2]> <tibble [28 × 2]> <split [1226|28]>
    ## 6 healthyR.ai   <tibble [1,080 × 2]> <tibble [28 × 2]> <split [1052|28]>
    ## 7 TidyDensity   <tibble [934 × 2]>   <tibble [28 × 2]> <split [906|28]> 
    ## 8 tidyAML       <tibble [550 × 2]>   <tibble [28 × 2]> <split [522|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.5889235 |  102.98549 | 0.6929962 | 150.95219 | 0.8122604 | 0.1668398 |
| healthyR.data |         2 | LM          | Test  | 0.8872433 |  314.63274 | 1.0440343 | 169.30622 | 1.0234672 | 0.0084193 |
| healthyR.data |         3 | EARTH       | Test  | 0.5797453 |  142.25975 | 0.6821962 | 116.36789 | 0.8400875 | 0.0084193 |
| healthyR.data |         4 | NNAR        | Test  | 0.6068439 |  105.35924 | 0.7140835 | 137.45367 | 0.8488337 | 0.0008777 |
| healthyR      |         1 | ARIMA       | Test  | 0.6965002 |   80.84187 | 0.7994799 | 137.66907 | 0.9261233 | 0.0146321 |
| healthyR      |         2 | LM          | Test  | 0.8165672 |  113.30030 | 0.9372992 | 182.02137 | 1.0149592 | 0.0107902 |
| healthyR      |         3 | EARTH       | Test  | 0.6691406 |   81.09282 | 0.7680752 | 114.73257 | 0.8971608 | 0.0107902 |
| healthyR      |         4 | NNAR        | Test  | 0.8060460 |  111.38288 | 0.9252224 | 174.04667 | 1.0118469 | 0.0003474 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7854418 |  156.09142 | 0.7778504 | 141.51027 | 1.0839674 | 0.0198646 |
| healthyR.ts   |         2 | LM          | Test  | 0.6927468 |  208.87272 | 0.6860513 | 103.36288 | 0.9780897 | 0.0223210 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6901642 |  213.34328 | 0.6834937 | 102.26356 | 0.9762307 | 0.0223210 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8681706 |  138.03848 | 0.8597797 | 174.68991 | 1.1468376 | 0.0116422 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5951814 |  544.37650 | 0.8258613 | 113.83438 | 0.7066952 | 0.1999234 |
| healthyverse  |         2 | LM          | Test  | 0.6801144 |  747.03571 | 0.9437125 | 112.30398 | 0.7871541 | 0.0127376 |
| healthyverse  |         3 | EARTH       | Test  | 0.6339186 |  522.96572 | 0.8796122 | 119.86142 | 0.7453803 | 0.0127376 |
| healthyverse  |         4 | NNAR        | Test  | 0.5940420 |  342.04621 | 0.8242802 | 127.51595 | 0.7321187 | 0.1402311 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7168177 |  104.52634 | 0.7636494 | 157.63611 | 0.9318688 | 0.0591144 |
| healthyR.ai   |         2 | LM          | Test  | 0.7424175 |  148.25946 | 0.7909217 | 134.44040 | 0.9864242 | 0.0001144 |
| healthyR.ai   |         3 | EARTH       | Test  | 6.4241637 | 2790.67104 | 6.8438723 | 174.47057 | 7.1317678 | 0.0001144 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7507653 |  151.50123 | 0.7998149 | 145.81323 | 0.9531495 | 0.0176181 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.4810483 |  320.88092 | 0.8118239 |  83.85481 | 0.5965116 | 0.1076694 |
| TidyDensity   |         2 | LM          | Test  | 0.5293848 |  363.01143 | 0.8933972 |  85.37075 | 0.6435934 | 0.0006540 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8292140 |  232.05008 | 1.3993933 | 185.78680 | 0.9918900 | 0.0006540 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5471615 |  105.41258 | 0.9233975 | 122.66443 | 0.7151529 | 0.0870950 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5904735 |  179.76666 | 0.8241973 | 114.89229 | 0.6699227 | 0.2348355 |
| tidyAML       |         2 | LM          | Test  | 0.6313453 |  179.26531 | 0.8812472 | 113.92056 | 0.7498805 | 0.0098213 |
| tidyAML       |         3 | EARTH       | Test  | 5.6590913 | 2802.88914 | 7.8990983 | 161.34486 | 6.2257780 | 0.0098213 |
| tidyAML       |         4 | NNAR        | Test  | 0.5673603 |  240.91333 | 0.7919354 | 103.25689 | 0.6839154 | 0.1538324 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         1 ARIMA       Test  0.589 103.  0.693 151.  0.812 0.167 
    ## 2 healthyR              3 EARTH       Test  0.669  81.1 0.768 115.  0.897 0.0108
    ## 3 healthyR.ts           3 EARTH       Test  0.690 213.  0.683 102.  0.976 0.0223
    ## 4 healthyverse          1 ARIMA       Test  0.595 544.  0.826 114.  0.707 0.200 
    ## 5 healthyR.ai           1 ARIMA       Test  0.717 105.  0.764 158.  0.932 0.0591
    ## 6 TidyDensity           1 ARIMA       Test  0.481 321.  0.812  83.9 0.597 0.108 
    ## 7 tidyAML               1 ARIMA       Test  0.590 180.  0.824 115.  0.670 0.235

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1317|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1309|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1255|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1226|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1052|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [906|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [522|28]>  <mdl_tm_t [1 × 5]>

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
