Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
16 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 127,151
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

The last day in the data set is 2025-01-14 21:02:35, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3849.45
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 127151        |
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
| r_version     |     90453 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     90453 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     90453 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10831 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-14 | 2023-04-19 | 1514 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1145128.76 | 1533884.40 | 355 | 14701 | 260378 | 2367940.5 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10346.78 | 18030.87 | 1 | 317 | 3091 | 11836.5 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-14 21:02:35 | 2023-04-19 21:36:07 | 77000 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 5M 40S |       60 |

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
    ## -154.97  -35.15   -9.73   26.97  806.04 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -187.75756   78.50460
    ## date                                                  0.01125    0.00416
    ## lag(value, 1)                                         0.12551    0.02536
    ## lag(value, 7)                                         0.09053    0.02630
    ## lag(value, 14)                                        0.10668    0.02641
    ## lag(value, 21)                                        0.04904    0.02651
    ## lag(value, 28)                                        0.06657    0.02637
    ## lag(value, 35)                                        0.07176    0.02648
    ## lag(value, 42)                                        0.05172    0.02655
    ## lag(value, 49)                                        0.09355    0.02639
    ## month(date, label = TRUE).L                         -11.23134    5.37265
    ## month(date, label = TRUE).Q                           2.14704    5.22484
    ## month(date, label = TRUE).C                         -11.34747    5.29884
    ## month(date, label = TRUE)^4                          -7.56924    5.31564
    ## month(date, label = TRUE)^5                         -12.93343    5.30074
    ## month(date, label = TRUE)^6                          -0.93640    5.38048
    ## month(date, label = TRUE)^7                          -9.08921    5.27984
    ## month(date, label = TRUE)^8                          -2.21977    5.27662
    ## month(date, label = TRUE)^9                           4.15911    5.26756
    ## month(date, label = TRUE)^10                          5.18725    5.26729
    ## month(date, label = TRUE)^11                         -6.26663    5.28076
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.72016    2.42309
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    7.29394    2.54196
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.392 0.016899 *  
    ## date                                                 2.705 0.006908 ** 
    ## lag(value, 1)                                        4.949 8.33e-07 ***
    ## lag(value, 7)                                        3.442 0.000594 ***
    ## lag(value, 14)                                       4.039 5.64e-05 ***
    ## lag(value, 21)                                       1.850 0.064521 .  
    ## lag(value, 28)                                       2.524 0.011700 *  
    ## lag(value, 35)                                       2.710 0.006809 ** 
    ## lag(value, 42)                                       1.948 0.051662 .  
    ## lag(value, 49)                                       3.544 0.000406 ***
    ## month(date, label = TRUE).L                         -2.090 0.036751 *  
    ## month(date, label = TRUE).Q                          0.411 0.681185    
    ## month(date, label = TRUE).C                         -2.142 0.032401 *  
    ## month(date, label = TRUE)^4                         -1.424 0.154676    
    ## month(date, label = TRUE)^5                         -2.440 0.014810 *  
    ## month(date, label = TRUE)^6                         -0.174 0.861861    
    ## month(date, label = TRUE)^7                         -1.721 0.085376 .  
    ## month(date, label = TRUE)^8                         -0.421 0.674051    
    ## month(date, label = TRUE)^9                          0.790 0.429908    
    ## month(date, label = TRUE)^10                         0.985 0.324886    
    ## month(date, label = TRUE)^11                        -1.187 0.235545    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.837 1.46e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.869 0.004172 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.94 on 1442 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2584, Adjusted R-squared:  0.2471 
    ## F-statistic: 22.84 on 22 and 1442 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,479 × 2]> <tibble [28 × 2]> <split [1451|28]>
    ## 2 healthyR      <tibble [1,472 × 2]> <tibble [28 × 2]> <split [1444|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,418 × 2]> <tibble [28 × 2]> <split [1390|28]>
    ## 5 healthyverse  <tibble [1,389 × 2]> <tibble [28 × 2]> <split [1361|28]>
    ## 6 healthyR.ai   <tibble [1,215 × 2]> <tibble [28 × 2]> <split [1187|28]>
    ## 7 TidyDensity   <tibble [1,069 × 2]> <tibble [28 × 2]> <split [1041|28]>
    ## 8 tidyAML       <tibble [685 × 2]>   <tibble [28 × 2]> <split [657|28]> 
    ## 9 RandomWalker  <tibble [119 × 2]>   <tibble [28 × 2]> <split [91|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.9505634 | 296.44441 | 0.7641665 | 164.1568 | 1.0853363 | 0.0000215 |
| healthyR.data | 2 | LM | Test | 0.9273290 | 294.42288 | 0.7454881 | 162.0113 | 1.0648837 | 0.0220982 |
| healthyR.data | 3 | EARTH | Test | 1.0909958 | 473.73140 | 0.8770614 | 154.4388 | 1.2160930 | 0.0220982 |
| healthyR.data | 4 | NNAR | Test | 0.8165750 | 128.00640 | 0.6564520 | 163.1356 | 1.0550742 | 0.0261184 |
| healthyR | 1 | ARIMA | Test | 0.7170400 | 115.03724 | 0.6546397 | 164.4519 | 0.8906775 | 0.0190719 |
| healthyR | 2 | LM | Test | 0.7024222 | 99.07159 | 0.6412940 | 180.4487 | 0.8983997 | 0.0109789 |
| healthyR | 3 | EARTH | Test | 0.6971997 | 102.81947 | 0.6365261 | 154.5354 | 0.9018441 | 0.0109789 |
| healthyR | 4 | NNAR | Test | 0.7140946 | 112.35232 | 0.6519507 | 152.5039 | 0.9061415 | 0.0036073 |
| healthyR.ts | 1 | ARIMA | Test | 0.8563541 | 201.61543 | 0.6258415 | 129.6562 | 1.0857972 | 0.0085404 |
| healthyR.ts | 2 | LM | Test | 0.8739606 | 220.21342 | 0.6387087 | 129.5066 | 1.0935499 | 0.0062668 |
| healthyR.ts | 3 | EARTH | Test | 0.8786187 | 225.13525 | 0.6421130 | 129.4310 | 1.0959569 | 0.0062668 |
| healthyR.ts | 4 | NNAR | Test | 0.8681279 | 112.56278 | 0.6344460 | 171.5813 | 1.0967085 | 0.0031789 |
| healthyverse | 1 | ARIMA | Test | 0.6833509 | 234.48592 | 0.7440722 | 112.3660 | 0.8071589 | 0.0115417 |
| healthyverse | 2 | LM | Test | 0.7031547 | 296.47912 | 0.7656357 | 105.0306 | 0.8361981 | 0.0268294 |
| healthyverse | 3 | EARTH | Test | 0.7537575 | 395.12537 | 0.8207349 | 100.8425 | 0.9173676 | 0.0268294 |
| healthyverse | 4 | NNAR | Test | 0.6747939 | 179.27956 | 0.7347547 | 121.9473 | 0.8106909 | 0.0655704 |
| healthyR.ai | 1 | ARIMA | Test | 0.6902049 | 102.06477 | 0.7203194 | 184.2778 | 0.8099597 | 0.0186075 |
| healthyR.ai | 2 | LM | Test | 0.6509080 | 108.12465 | 0.6793080 | 141.4558 | 0.8126979 | 0.0039882 |
| healthyR.ai | 3 | EARTH | Test | 0.6289580 | 131.29343 | 0.6564002 | 114.0339 | 0.8357718 | 0.0039882 |
| healthyR.ai | 4 | NNAR | Test | 0.6809088 | 107.88660 | 0.7106178 | 157.4555 | 0.8191828 | 0.0002872 |
| TidyDensity | 1 | ARIMA | Test | 0.7669925 | 187.11129 | 0.7331266 | 120.3530 | 0.9324481 | 0.0023540 |
| TidyDensity | 2 | LM | Test | 0.7947690 | 215.96940 | 0.7596766 | 115.8751 | 0.9581631 | 0.0198052 |
| TidyDensity | 3 | EARTH | Test | 0.7250557 | 155.67126 | 0.6930415 | 120.2691 | 0.8865397 | 0.0198052 |
| TidyDensity | 4 | NNAR | Test | 0.7700472 | 147.06209 | 0.7360464 | 146.6095 | 0.9056274 | 0.0038639 |
| tidyAML | 1 | ARIMA | Test | 0.9372263 | 261.47900 | 0.8126580 | 108.5427 | 1.0455927 | 0.0405904 |
| tidyAML | 2 | LM | Test | 0.9831425 | 212.07941 | 0.8524715 | 118.9153 | 1.1303763 | 0.0078307 |
| tidyAML | 3 | EARTH | Test | 0.9416255 | 284.08799 | 0.8164725 | 106.1738 | 1.0484107 | 0.0078307 |
| tidyAML | 4 | NNAR | Test | 0.9643946 | 252.84854 | 0.8362154 | 111.6753 | 1.0887098 | 0.0063446 |
| RandomWalker | 1 | ARIMA | Test | 1.0170357 | 166.32247 | 0.4805444 | 115.4064 | 1.2483094 | 0.2723496 |
| RandomWalker | 2 | LM | Test | 1.2726136 | 106.13274 | 0.6013037 | 159.0587 | 1.4623653 | 0.0001226 |
| RandomWalker | 3 | EARTH | Test | 1.2690645 | 100.65248 | 0.5996268 | 161.4669 | 1.4577607 | NA |
| RandomWalker | 4 | NNAR | Test | 1.6396221 | 174.11423 | 0.7747134 | 163.0108 | 1.9536114 | 0.1688960 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.817  128. 0.656  163. 1.06  0.0261 
    ## 2 healthyR             1 ARIMA       Test  0.717  115. 0.655  164. 0.891 0.0191 
    ## 3 healthyR.ts          1 ARIMA       Test  0.856  202. 0.626  130. 1.09  0.00854
    ## 4 healthyverse         1 ARIMA       Test  0.683  234. 0.744  112. 0.807 0.0115 
    ## 5 healthyR.ai          1 ARIMA       Test  0.690  102. 0.720  184. 0.810 0.0186 
    ## 6 TidyDensity          3 EARTH       Test  0.725  156. 0.693  120. 0.887 0.0198 
    ## 7 tidyAML              1 ARIMA       Test  0.937  261. 0.813  109. 1.05  0.0406 
    ## 8 RandomWalker         1 ARIMA       Test  1.02   166. 0.481  115. 1.25  0.272

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1451|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1444|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1390|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1361|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1187|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1041|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [657|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [91|28]>   <mdl_tm_t [1 × 5]>

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
