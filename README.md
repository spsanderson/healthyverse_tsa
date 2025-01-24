Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 128,393
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

The last day in the data set is 2025-01-22 22:49:45, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4043.23
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 128393        |
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
| r_version     |     91521 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     91521 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     91521 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10870 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-22 | 2023-04-23 | 1522 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1142722.82 | 1532308.25 | 355 | 14701 | 260378 | 2367889 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10462.58 | 18466.76 | 1 | 320 | 3098 | 11961 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-22 22:49:45 | 2023-04-23 17:59:32 | 77754 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 41S |       60 |

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
    ## -156.04  -35.19   -9.66   27.45  803.35 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.954e+02  7.809e+01
    ## date                                                1.163e-02  4.139e-03
    ## lag(value, 1)                                       1.233e-01  2.533e-02
    ## lag(value, 7)                                       9.477e-02  2.626e-02
    ## lag(value, 14)                                      1.098e-01  2.628e-02
    ## lag(value, 21)                                      5.283e-02  2.644e-02
    ## lag(value, 28)                                      6.599e-02  2.632e-02
    ## lag(value, 35)                                      6.937e-02  2.643e-02
    ## lag(value, 42)                                      5.052e-02  2.651e-02
    ## lag(value, 49)                                      9.436e-02  2.638e-02
    ## month(date, label = TRUE).L                        -1.206e+01  5.330e+00
    ## month(date, label = TRUE).Q                         2.867e+00  5.195e+00
    ## month(date, label = TRUE).C                        -1.204e+01  5.274e+00
    ## month(date, label = TRUE)^4                        -6.910e+00  5.298e+00
    ## month(date, label = TRUE)^5                        -1.325e+01  5.296e+00
    ## month(date, label = TRUE)^6                        -5.390e-01  5.379e+00
    ## month(date, label = TRUE)^7                        -9.192e+00  5.282e+00
    ## month(date, label = TRUE)^8                        -2.093e+00  5.280e+00
    ## month(date, label = TRUE)^9                         4.179e+00  5.271e+00
    ## month(date, label = TRUE)^10                        5.079e+00  5.271e+00
    ## month(date, label = TRUE)^11                       -6.148e+00  5.284e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.169e+01  2.420e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.175e+00  2.540e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.502 0.012462 *  
    ## date                                                 2.809 0.005029 ** 
    ## lag(value, 1)                                        4.867 1.26e-06 ***
    ## lag(value, 7)                                        3.608 0.000319 ***
    ## lag(value, 14)                                       4.178 3.12e-05 ***
    ## lag(value, 21)                                       1.998 0.045856 *  
    ## lag(value, 28)                                       2.507 0.012272 *  
    ## lag(value, 35)                                       2.624 0.008777 ** 
    ## lag(value, 42)                                       1.905 0.056933 .  
    ## lag(value, 49)                                       3.577 0.000359 ***
    ## month(date, label = TRUE).L                         -2.263 0.023792 *  
    ## month(date, label = TRUE).Q                          0.552 0.581168    
    ## month(date, label = TRUE).C                         -2.283 0.022600 *  
    ## month(date, label = TRUE)^4                         -1.304 0.192346    
    ## month(date, label = TRUE)^5                         -2.502 0.012442 *  
    ## month(date, label = TRUE)^6                         -0.100 0.920201    
    ## month(date, label = TRUE)^7                         -1.740 0.082069 .  
    ## month(date, label = TRUE)^8                         -0.396 0.691805    
    ## month(date, label = TRUE)^9                          0.793 0.428012    
    ## month(date, label = TRUE)^10                         0.964 0.335403    
    ## month(date, label = TRUE)^11                        -1.163 0.244843    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.833 1.49e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.825 0.004794 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.98 on 1450 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2631, Adjusted R-squared:  0.252 
    ## F-statistic: 23.54 on 22 and 1450 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,487 × 2]> <tibble [28 × 2]> <split [1459|28]>
    ## 2 healthyR      <tibble [1,480 × 2]> <tibble [28 × 2]> <split [1452|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,426 × 2]> <tibble [28 × 2]> <split [1398|28]>
    ## 5 healthyverse  <tibble [1,397 × 2]> <tibble [28 × 2]> <split [1369|28]>
    ## 6 healthyR.ai   <tibble [1,223 × 2]> <tibble [28 × 2]> <split [1195|28]>
    ## 7 TidyDensity   <tibble [1,077 × 2]> <tibble [28 × 2]> <split [1049|28]>
    ## 8 tidyAML       <tibble [693 × 2]>   <tibble [28 × 2]> <split [665|28]> 
    ## 9 RandomWalker  <tibble [127 × 2]>   <tibble [28 × 2]> <split [99|28]>

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
| healthyR.data | 1 | ARIMA | Test | 1.0290976 | 229.6758 | 0.7753818 | 166.8385 | 1.1995563 | 0.0157987 |
| healthyR.data | 2 | LM | Test | 1.0619604 | 255.3762 | 0.8001425 | 166.4317 | 1.2150580 | 0.0962202 |
| healthyR.data | 3 | EARTH | Test | 1.2263275 | 376.3081 | 0.9239862 | 164.6994 | 1.3630301 | 0.0962202 |
| healthyR.data | 4 | NNAR | Test | 0.8709451 | 105.2169 | 0.6562206 | 162.9535 | 1.0936588 | 0.0022774 |
| healthyR | 1 | ARIMA | Test | 0.7839761 | 113.7870 | 0.7490044 | 165.6036 | 0.9852602 | 0.0340094 |
| healthyR | 2 | LM | Test | 0.7792258 | 101.4562 | 0.7444660 | 181.9999 | 0.9802232 | 0.0223191 |
| healthyR | 3 | EARTH | Test | 0.7925278 | 110.5996 | 0.7571746 | 158.3048 | 1.0060990 | 0.0223191 |
| healthyR | 4 | NNAR | Test | 0.8094052 | 136.2654 | 0.7732992 | 161.7979 | 1.0123733 | 0.0031898 |
| healthyR.ts | 1 | ARIMA | Test | 0.8244328 | 235.2152 | 0.6521624 | 129.9239 | 1.1022043 | 0.0009847 |
| healthyR.ts | 2 | LM | Test | 0.8561692 | 301.1627 | 0.6772673 | 128.6463 | 1.1013818 | 0.0015284 |
| healthyR.ts | 3 | EARTH | Test | 0.8783163 | 337.9534 | 0.6947866 | 128.7349 | 1.1058767 | 0.0015284 |
| healthyR.ts | 4 | NNAR | Test | 0.8493525 | 104.5704 | 0.6718750 | 172.2695 | 1.1730042 | 0.0001368 |
| healthyverse | 1 | ARIMA | Test | 0.7396527 | 252.0247 | 0.7016406 | 113.8037 | 0.8856199 | 0.0065728 |
| healthyverse | 2 | LM | Test | 0.7895523 | 314.4066 | 0.7489757 | 112.6970 | 0.9249278 | 0.0001475 |
| healthyverse | 3 | EARTH | Test | 0.8741632 | 398.0588 | 0.8292383 | 112.5506 | 1.0208789 | 0.0001475 |
| healthyverse | 4 | NNAR | Test | 0.7186339 | 182.5858 | 0.6817019 | 125.6777 | 0.8708119 | 0.0074245 |
| healthyR.ai | 1 | ARIMA | Test | 0.7556539 | 118.9853 | 0.7676565 | 184.1487 | 0.9370271 | 0.0053482 |
| healthyR.ai | 2 | LM | Test | 0.7551176 | 142.3255 | 0.7671117 | 158.2107 | 0.9552373 | 0.0544996 |
| healthyR.ai | 3 | EARTH | Test | 0.7906025 | 203.9761 | 0.8031601 | 135.5342 | 1.0329084 | 0.0544996 |
| healthyR.ai | 4 | NNAR | Test | 0.7944940 | 147.8764 | 0.8071135 | 166.7804 | 0.9936332 | 0.0113589 |
| TidyDensity | 1 | ARIMA | Test | 0.7469285 | 133.6427 | 0.7400684 | 113.9025 | 0.9248973 | 0.0184860 |
| TidyDensity | 2 | LM | Test | 0.7703218 | 164.4649 | 0.7632469 | 106.8367 | 0.9416328 | 0.0135001 |
| TidyDensity | 3 | EARTH | Test | 0.7258626 | 125.2875 | 0.7191960 | 111.4856 | 0.8980957 | 0.0135001 |
| TidyDensity | 4 | NNAR | Test | 0.7876977 | 116.2700 | 0.7804632 | 145.8126 | 0.9551433 | 0.0114701 |
| tidyAML | 1 | ARIMA | Test | 0.9750690 | 322.6758 | 0.8468374 | 108.5576 | 1.1369289 | 0.0233608 |
| tidyAML | 2 | LM | Test | 0.9892960 | 223.4500 | 0.8591935 | 123.2633 | 1.1347907 | 0.0869067 |
| tidyAML | 3 | EARTH | Test | 1.1953656 | 474.9950 | 1.0381627 | 108.4489 | 1.4025433 | 0.0869067 |
| tidyAML | 4 | NNAR | Test | 0.9989229 | 249.6250 | 0.8675543 | 118.2568 | 1.1586483 | 0.0170649 |
| RandomWalker | 1 | ARIMA | Test | 1.2118879 | 203.3990 | 0.5973795 | 134.5572 | 1.4293544 | 0.0768016 |
| RandomWalker | 2 | LM | Test | 1.2131805 | 105.6946 | 0.5980166 | 159.6040 | 1.4285103 | 0.0176273 |
| RandomWalker | 3 | EARTH | Test | 1.2077454 | 102.4606 | 0.5953375 | 161.0553 | 1.4223787 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4689383 | 193.8302 | 0.7240881 | 158.6077 | 1.7151906 | 0.0320162 |

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
    ## 1 healthyR.d…         4 NNAR        Test  0.871  105. 0.656  163. 1.09   0.00228
    ## 2 healthyR            2 LM          Test  0.779  101. 0.744  182. 0.980  0.0223 
    ## 3 healthyR.ts         2 LM          Test  0.856  301. 0.677  129. 1.10   0.00153
    ## 4 healthyver…         4 NNAR        Test  0.719  183. 0.682  126. 0.871  0.00742
    ## 5 healthyR.ai         1 ARIMA       Test  0.756  119. 0.768  184. 0.937  0.00535
    ## 6 TidyDensity         3 EARTH       Test  0.726  125. 0.719  111. 0.898  0.0135 
    ## 7 tidyAML             2 LM          Test  0.989  223. 0.859  123. 1.13   0.0869 
    ## 8 RandomWalk…         3 EARTH       Test  1.21   102. 0.595  161. 1.42  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1459|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1452|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1398|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1369|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1195|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1049|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [665|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [99|28]>   <mdl_tm_t [1 × 5]>

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
