Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 132,173
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

The last day in the data set is 2025-02-22 23:34:55, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4787.99
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 132173        |
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
| r_version     |     94747 |          0.28 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     94747 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     94747 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11161 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-22 | 2023-05-09 | 1553 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1138993 | 1528478.98 | 355 | 14701 | 260584 | 2367818 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10407 | 18401.48 | 1 | 288 | 3077 | 11949 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-22 23:34:55 | 2023-05-09 23:16:43 | 80145 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 58S |       60 |

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
    ## -154.46  -34.92  -10.42   26.99  809.53 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.169e+02  7.659e+01
    ## date                                                1.282e-02  4.062e-03
    ## lag(value, 1)                                       1.132e-01  2.510e-02
    ## lag(value, 7)                                       8.858e-02  2.613e-02
    ## lag(value, 14)                                      1.020e-01  2.613e-02
    ## lag(value, 21)                                      6.344e-02  2.632e-02
    ## lag(value, 28)                                      5.957e-02  2.628e-02
    ## lag(value, 35)                                      7.509e-02  2.642e-02
    ## lag(value, 42)                                      5.418e-02  2.648e-02
    ## lag(value, 49)                                      8.682e-02  2.633e-02
    ## month(date, label = TRUE).L                        -1.264e+01  5.233e+00
    ## month(date, label = TRUE).Q                         2.506e+00  5.198e+00
    ## month(date, label = TRUE).C                        -1.088e+01  5.291e+00
    ## month(date, label = TRUE)^4                        -8.935e+00  5.276e+00
    ## month(date, label = TRUE)^5                        -1.141e+01  5.235e+00
    ## month(date, label = TRUE)^6                        -2.574e+00  5.319e+00
    ## month(date, label = TRUE)^7                        -7.932e+00  5.265e+00
    ## month(date, label = TRUE)^8                        -2.978e+00  5.300e+00
    ## month(date, label = TRUE)^9                         4.513e+00  5.313e+00
    ## month(date, label = TRUE)^10                        5.060e+00  5.319e+00
    ## month(date, label = TRUE)^11                       -6.286e+00  5.333e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.186e+01  2.416e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.829e+00  2.546e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.832 0.004693 ** 
    ## date                                                 3.157 0.001627 ** 
    ## lag(value, 1)                                        4.509 7.02e-06 ***
    ## lag(value, 7)                                        3.390 0.000719 ***
    ## lag(value, 14)                                       3.905 9.86e-05 ***
    ## lag(value, 21)                                       2.411 0.016049 *  
    ## lag(value, 28)                                       2.267 0.023559 *  
    ## lag(value, 35)                                       2.842 0.004543 ** 
    ## lag(value, 42)                                       2.046 0.040954 *  
    ## lag(value, 49)                                       3.297 0.001000 ***
    ## month(date, label = TRUE).L                         -2.416 0.015819 *  
    ## month(date, label = TRUE).Q                          0.482 0.629786    
    ## month(date, label = TRUE).C                         -2.056 0.040005 *  
    ## month(date, label = TRUE)^4                         -1.694 0.090554 .  
    ## month(date, label = TRUE)^5                         -2.180 0.029438 *  
    ## month(date, label = TRUE)^6                         -0.484 0.628522    
    ## month(date, label = TRUE)^7                         -1.506 0.132185    
    ## month(date, label = TRUE)^8                         -0.562 0.574279    
    ## month(date, label = TRUE)^9                          0.849 0.395804    
    ## month(date, label = TRUE)^10                         0.951 0.341589    
    ## month(date, label = TRUE)^11                        -1.179 0.238712    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.908 1.02e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.075 0.002141 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.53 on 1481 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2603, Adjusted R-squared:  0.2494 
    ## F-statistic: 23.69 on 22 and 1481 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,518 × 2]> <tibble [28 × 2]> <split [1490|28]>
    ## 2 healthyR      <tibble [1,511 × 2]> <tibble [28 × 2]> <split [1483|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,457 × 2]> <tibble [28 × 2]> <split [1429|28]>
    ## 5 healthyverse  <tibble [1,428 × 2]> <tibble [28 × 2]> <split [1400|28]>
    ## 6 healthyR.ai   <tibble [1,254 × 2]> <tibble [28 × 2]> <split [1226|28]>
    ## 7 TidyDensity   <tibble [1,108 × 2]> <tibble [28 × 2]> <split [1080|28]>
    ## 8 tidyAML       <tibble [723 × 2]>   <tibble [28 × 2]> <split [695|28]> 
    ## 9 RandomWalker  <tibble [157 × 2]>   <tibble [28 × 2]> <split [129|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7744516 | 110.73714 | 0.6580216 | 174.07104 | 0.9239111 | 0.1012826 |
| healthyR.data | 2 | LM | Test | 0.8079355 | 131.10802 | 0.6864716 | 142.19700 | 0.9441567 | 0.0587475 |
| healthyR.data | 3 | EARTH | Test | 1.3485937 | 358.06900 | 1.1458480 | 134.87560 | 1.5773448 | 0.0587475 |
| healthyR.data | 4 | NNAR | Test | 0.7638615 | 107.62862 | 0.6490236 | 178.17681 | 0.8964631 | 0.1216723 |
| healthyR | 1 | ARIMA | Test | 0.9960148 | 111.68497 | 0.7347096 | 167.17975 | 1.1394376 | 0.0272403 |
| healthyR | 2 | LM | Test | 0.9808029 | 104.22517 | 0.7234885 | 195.50916 | 1.1194500 | 0.1276272 |
| healthyR | 3 | EARTH | Test | 0.9866338 | 105.94265 | 0.7277897 | 197.01224 | 1.1240251 | 0.1276272 |
| healthyR | 4 | NNAR | Test | 1.0252155 | 122.43501 | 0.7562494 | 175.38393 | 1.1664780 | 0.0273189 |
| healthyR.ts | 1 | ARIMA | Test | 0.9948065 | 115.87869 | 0.6230421 | 134.81906 | 1.2046248 | 0.0794495 |
| healthyR.ts | 2 | LM | Test | 0.9914507 | 125.37946 | 0.6209404 | 129.09771 | 1.1910057 | 0.0794495 |
| healthyR.ts | 3 | EARTH | Test | 0.9913860 | 126.78070 | 0.6208999 | 128.43151 | 1.1896421 | 0.0794495 |
| healthyR.ts | 4 | NNAR | Test | 1.0469537 | 131.54279 | 0.6557016 | 174.99859 | 1.2780709 | 0.0206854 |
| healthyverse | 1 | ARIMA | Test | 0.6283681 | 200.35285 | 0.7457609 | 118.50406 | 0.7717906 | 0.0264102 |
| healthyverse | 2 | LM | Test | 0.7059573 | 303.11587 | 0.8378455 | 115.02439 | 0.8728417 | 0.1924173 |
| healthyverse | 3 | EARTH | Test | 0.6192986 | 180.81619 | 0.7349971 | 126.84797 | 0.7425184 | 0.1924173 |
| healthyverse | 4 | NNAR | Test | 0.6040434 | 128.72744 | 0.7168919 | 121.69719 | 0.7359758 | 0.0281816 |
| healthyR.ai | 1 | ARIMA | Test | 0.8471458 | 100.62381 | 0.6851909 | 186.32970 | 0.9846842 | 0.0002641 |
| healthyR.ai | 2 | LM | Test | 0.8591605 | 97.41236 | 0.6949086 | 157.10207 | 1.0248418 | 0.1408105 |
| healthyR.ai | 3 | EARTH | Test | 0.8649608 | 97.60866 | 0.6996000 | 154.77836 | 1.0366046 | 0.1408105 |
| healthyR.ai | 4 | NNAR | Test | 0.8334172 | 95.56928 | 0.6740869 | 161.01651 | 0.9914189 | 0.0252664 |
| TidyDensity | 1 | ARIMA | Test | 0.7138722 | 177.99243 | 0.6733067 | 117.73734 | 0.8360317 | 0.0830742 |
| TidyDensity | 2 | LM | Test | 0.7328298 | 240.87940 | 0.6911870 | 108.13251 | 0.8552416 | 0.0969764 |
| TidyDensity | 3 | EARTH | Test | 0.7289389 | 173.01838 | 0.6875172 | 120.11733 | 0.8626965 | 0.0969764 |
| TidyDensity | 4 | NNAR | Test | 0.7198052 | 128.53846 | 0.6789026 | 139.48614 | 0.8920493 | 0.0770698 |
| tidyAML | 1 | ARIMA | Test | 0.6275723 | 129.17292 | 0.7121127 | 99.58173 | 0.7614941 | 0.1277991 |
| tidyAML | 2 | LM | Test | 0.6786029 | 137.27432 | 0.7700177 | 99.93992 | 0.8123515 | 0.0213394 |
| tidyAML | 3 | EARTH | Test | 2.4176693 | 474.43663 | 2.7433542 | 180.36494 | 2.6549120 | 0.0213394 |
| tidyAML | 4 | NNAR | Test | 0.6808768 | 138.68470 | 0.7725979 | 106.78480 | 0.8129653 | 0.0159173 |
| RandomWalker | 1 | ARIMA | Test | 1.2733428 | 117.22345 | 0.6040533 | 168.93673 | 1.4331330 | 0.0436383 |
| RandomWalker | 2 | LM | Test | 1.3221599 | 123.97470 | 0.6272114 | 191.40091 | 1.4685068 | 0.0001170 |
| RandomWalker | 3 | EARTH | Test | 1.2381903 | 84.98028 | 0.5873775 | 154.99608 | 1.4589233 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3706285 | 166.42144 | 0.6502041 | 166.42453 | 1.5303966 | 0.0027869 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.764  108. 0.649 178.  0.896 1.22e-1
    ## 2 healthyR             2 LM          Test  0.981  104. 0.723 196.  1.12  1.28e-1
    ## 3 healthyR.ts          3 EARTH       Test  0.991  127. 0.621 128.  1.19  7.94e-2
    ## 4 healthyverse         4 NNAR        Test  0.604  129. 0.717 122.  0.736 2.82e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.847  101. 0.685 186.  0.985 2.64e-4
    ## 6 TidyDensity          1 ARIMA       Test  0.714  178. 0.673 118.  0.836 8.31e-2
    ## 7 tidyAML              1 ARIMA       Test  0.628  129. 0.712  99.6 0.761 1.28e-1
    ## 8 RandomWalker         1 ARIMA       Test  1.27   117. 0.604 169.  1.43  4.36e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1490|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1483|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1429|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1400|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1226|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1080|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [695|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [129|28]>  <mdl_tm_t [1 × 5]>

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
